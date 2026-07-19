(ns geschichte.projection
  "JVM physical-worktree projection and `.geschichte/repo.edn` discovery.

  The marker locates Geschichte storage; it is not repository history. The
  physical directory is a projection which is scanned into the Datahike
  worktree before Git-compatible operations and updated from it afterwards."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [geschichte.bytes :as bytes]
            [geschichte.repo :as repo])
  (:import [java.nio.file AtomicMoveNotSupportedException Files LinkOption Path
            StandardCopyOption]
           [java.nio.file.attribute BasicFileAttributes PosixFilePermission]
           [java.io File]
           [java.util.concurrent TimeUnit]
           [java.util UUID]))

(set! *warn-on-reflection* true)

(def metadata-directory ".geschichte")
(def marker-name "repo.edn")
(def cache-name "stat-cache.edn")
(def marker-version 1)
(def symlink-mode 40960)
(def executable-mode 33261)

(defn ^File canonical-file
  "Return the canonical JVM file used for physical workspace identity."
  [file]
  (.getCanonicalFile (io/file file)))

(defn ^File marker-file [root]
  (io/file root metadata-directory marker-name))

(defn- ^File cache-file [root]
  (io/file root metadata-directory cache-name))

(defn discover
  "Find the nearest physical Geschichte projection at or above `start`."
  [start]
  (loop [^File directory (canonical-file start)]
    (let [^File directory (if (.isDirectory directory)
                            directory (.getParentFile directory))
          ^File marker (when directory (marker-file directory))]
      (cond
        (and marker (.isFile marker))
        {:root directory :marker marker}

        (nil? directory) nil
        (= directory (.getParentFile directory)) nil
        :else (recur (.getParentFile directory))))))

(defn default-config
  "Create the portable local Datahike configuration stored in a marker."
  []
  {:store {:backend :file :path "store" :id (UUID/randomUUID)}
   ;; Geschichte produces both large import transactions and streams of small
   ;; worktree commits. Buffering index diffs and fusing their roots reduced a
   ;; representative imported repository by about one fifth on disk while
   ;; preserving Datahike's structural sharing and query semantics.
   :index-config {:diff-buf-size 256}
   :fuse-index-roots? true
   ;; Datahike/Konserve cache ordinary values and persistent-index nodes by
   ;; entry count. Binary store-ref payloads bypass this cache, but bounding the
   ;; node cache keeps large checkout transactions from retaining 1000 wide
   ;; persistent-set nodes per cache layer.
   :store-cache-size 256
   :search-cache-size 0
   :schema-flexibility :write
   :keep-history? true
   :commit-graph? true})

(defn write-marker!
  "Write a projection marker. Workspace metadata is local coordination state,
  not Geschichte history, and may therefore contain canonical host paths."
  ([root config] (write-marker! root config {}))
  ([root config workspace]
   (let [^File directory (io/file root metadata-directory)
         ^File marker (marker-file root)]
     (.mkdirs directory)
     (spit marker
           (str (pr-str (merge {:geschichte/version marker-version
                                :datahike/config config}
                               workspace))
                "\n"))
     marker)))

(defn- resolve-store-path [^File marker config]
  (let [path (get-in config [:store :path])]
    (if (and path (not (.isAbsolute (io/file path))))
      (assoc-in config [:store :path]
                (.getPath (canonical-file (io/file (.getParentFile marker) path))))
      config)))

(defn read-marker
  "Read either a projection marker or a legacy raw Datahike config file."
  [marker]
  (let [^File marker (canonical-file marker)
        value (edn/read-string (slurp marker))
        wrapped? (contains? value :datahike/config)
        config (if wrapped? (:datahike/config value) value)
        root (if wrapped?
               (.getParentFile (.getParentFile marker))
               (.getParentFile marker))
        workspace-branch (or (:workspace/branch value)
                             (:branch config) :db)
        canonical-marker (canonical-file
                          (or (:workspace/canonical-marker value) marker))]
    {:root root :marker marker :projection? wrapped?
     :config (assoc (resolve-store-path marker config)
                    :branch workspace-branch)
     :workspace-id (:workspace/id value)
     :workspace-branch workspace-branch
     :publication (or (:workspace/publication value) :manual)
     :canonical-marker canonical-marker}))

(defn locate
  "Resolve an explicit marker/config, or discover a projection from `cwd`."
  [cwd explicit]
  (if explicit
    (read-marker explicit)
    (if-let [{:keys [marker]} (discover cwd)]
      (read-marker marker)
      (throw (ex-info "not a Geschichte repository (or any parent directory)"
                      {:cwd (.getPath (canonical-file cwd))
                       :exit 128 :kind :repository-not-found})))))

(defn initialize-projection!
  "Create a marker for `root`. Returns the resolved repository locator."
  ([root] (initialize-projection! root (default-config)))
  ([root config]
   (let [^File root (canonical-file root)]
     (.mkdirs root)
     (when-not (.isDirectory root)
       (throw (ex-info "cannot create work tree" {:root (.getPath root)})))
     (if (.isFile (marker-file root))
       (read-marker (marker-file root))
       (let [marker (marker-file root)]
         (write-marker! root config
                        {:workspace/id (UUID/randomUUID)
                         :workspace/branch (or (:branch config) :db)
                         :workspace/publication :manual
                         :workspace/canonical-marker (.getPath marker)})
         (read-marker (marker-file root)))))))

(defn- relative-path [^Path root ^Path path]
  (-> (.toString (.relativize root path)) (str/replace #"\\" "/")))

(defn- administrative-path? [^Path root ^Path path]
  (or (.startsWith path (.resolve root ^String metadata-directory))
      ;; Native Git administration is an import source, never worktree data.
      ;; This covers both a directory and linked-worktree `.git` pointer file.
      (.startsWith path (.resolve root ".git"))))

(defn- physical-files [root]
  (let [^Path root-path (.toPath (canonical-file root))]
    (with-open [stream (Files/walk root-path (make-array java.nio.file.FileVisitOption 0))]
      (->> (iterator-seq (.iterator stream))
           (remove (fn [^Path path]
                     (or (= root-path path)
                         (administrative-path? root-path path))))
           (filter (fn [^Path path]
                     (or (Files/isRegularFile path (make-array LinkOption 0))
                         (Files/isSymbolicLink path))))
           (map (fn [^Path path] [(relative-path root-path path) path]))
           (into (sorted-map))))))

(defn- executable? [^Path path]
  (try
    (contains? (Files/getPosixFilePermissions path (make-array LinkOption 0))
               PosixFilePermission/OWNER_EXECUTE)
    (catch UnsupportedOperationException _ (.canExecute (.toFile path)))))

(defn- fingerprint [^Path path]
  (let [^BasicFileAttributes attributes
        (Files/readAttributes
         path BasicFileAttributes
         ^"[Ljava.nio.file.LinkOption;"
         (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))
        symlink? (.isSymbolicLink attributes)
        mode (cond symlink? symlink-mode
                   (executable? path) executable-mode
                   :else repo/default-mode)]
    {:size (.size attributes)
     :mtime-ns (.to (.lastModifiedTime attributes) TimeUnit/NANOSECONDS)
     :file-key (some-> (.fileKey attributes) str)
     :mode mode
     :symlink? symlink?}))

(defn- read-cache [root]
  (try (edn/read-string (slurp (cache-file root)))
       (catch Exception _ {})))

(defn- write-cache! [root cache]
  (spit (cache-file root) (str (pr-str cache) "\n")))

(defn scan!
  "Import physical changes into Geschichte. Stat-cache hits are accepted only
  when they still name the current worktree content identity. Candidates are
  streamed and content-hashed by Geschichte before becoming repository data."
  [conn root]
  (let [^File root (canonical-file root)
        disk (physical-files root)
        old-cache (read-cache root)
        worktree (repo/worktree conn)
        disk-paths (set (keys disk))
        deleted (set/difference (set (keys worktree)) disk-paths)
        cache (atom {})
        imported (atom [])]
    (doseq [path deleted]
      (repo/remove! conn path))
    (doseq [[path ^Path physical] disk]
      (let [stat (fingerprint physical)
            cached (get old-cache path)
            current-id (get-in worktree [path :content])]
        (if (and (= stat (dissoc cached :content))
                 (= current-id (:content cached)))
          (swap! cache assoc path cached)
          (let [result (if (:symlink? stat)
                         (repo/write! conn path
                                      (bytes/utf8 (str (Files/readSymbolicLink physical)))
                                      {:mode symlink-mode})
                         (repo/write-file! conn path (.toFile physical)
                                           {:mode (:mode stat)}))]
            (swap! imported conj path)
            (swap! cache assoc path (assoc stat :content (:content result)))))))
    (write-cache! root @cache)
    {:imported @imported :deleted (vec (sort deleted)) :files (count disk)}))

(defn- ensure-parent! [^Path path]
  (when-let [parent (.getParent path)] (Files/createDirectories parent (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- delete-path! [^Path path]
  (Files/deleteIfExists path))

(defn- executable-permissions [permissions executable?]
  (reduce (fn [result [read execute]]
            ((if (and executable? (contains? permissions read)) conj disj)
             result execute))
          (set permissions)
          [[PosixFilePermission/OWNER_READ PosixFilePermission/OWNER_EXECUTE]
           [PosixFilePermission/GROUP_READ PosixFilePermission/GROUP_EXECUTE]
           [PosixFilePermission/OTHERS_READ PosixFilePermission/OTHERS_EXECUTE]]))

(defn- prepare-materialized-mode!
  "Apply umask-derived read/write permissions and Git's executable distinction
  to the temporary inode before its atomic move. Returns false on non-POSIX
  filesystems so the caller can use the portable executable-bit fallback."
  [^Path target ^Path temporary executable?]
  (try
    (let [existing? (Files/exists target (make-array LinkOption 0))
          permissions
          (if existing?
            (Files/getPosixFilePermissions target (make-array LinkOption 0))
            (do
              ;; Files/createFile uses the process umask; createTempFile
              ;; deliberately starts at 0600 and therefore cannot be used as
              ;; the permission template for a checked-out worktree file.
              (Files/createFile
               target (make-array java.nio.file.attribute.FileAttribute 0))
              (try
                (Files/getPosixFilePermissions target (make-array LinkOption 0))
                (finally (Files/deleteIfExists target)))))]
      (Files/setPosixFilePermissions
       temporary (executable-permissions permissions executable?))
      true)
    (catch UnsupportedOperationException _ false)))

(defn- materialize-entry! [conn ^Path root ^String path {:keys [mode]}]
  (let [^Path target (.resolve root path)]
    (ensure-parent! target)
    (when (Files/isSymbolicLink target) (delete-path! target))
    (if (= mode symlink-mode)
      (let [value (repo/read conn path)]
        (delete-path! target)
        (Files/createSymbolicLink target (java.nio.file.Paths/get
                                          (bytes/decode-utf8 value)
                                          (make-array String 0))
                                  (make-array java.nio.file.attribute.FileAttribute 0)))
      (let [^Path temporary
            (Files/createTempFile (.resolve root ^String metadata-directory)
                                  "project-" ".tmp"
                                  (make-array java.nio.file.attribute.FileAttribute 0))]
        (try
          (with-open [out (io/output-stream (.toFile temporary))]
            (repo/copy-to! conn path out))
          (let [posix? (prepare-materialized-mode!
                        target temporary (= mode executable-mode))]
            (try
              (Files/move temporary target
                          (into-array StandardCopyOption
                                      [StandardCopyOption/REPLACE_EXISTING
                                       StandardCopyOption/ATOMIC_MOVE]))
              (catch AtomicMoveNotSupportedException _
                (Files/move temporary target
                            (into-array StandardCopyOption
                                        [StandardCopyOption/REPLACE_EXISTING]))))
            (when-not posix?
              (.setExecutable (.toFile target) (= mode executable-mode) false)))
          (finally (Files/deleteIfExists temporary)))))))

(defn materialize-changes!
  "Apply a Geschichte worktree transition to disk without touching unrelated
  paths created after the preceding scan. Content is streamed through a temp
  file and atomically renamed."
  [conn root before after]
  (let [^Path root-path (.toPath (canonical-file root))
        before-paths (set (keys before))
        after-paths (set (keys after))
        deleted (set/difference before-paths after-paths)
        changed (filter #(not= (get before %) (get after %)) after-paths)]
    (doseq [^String path deleted] (delete-path! (.resolve root-path path)))
    (doseq [^String path changed]
      (materialize-entry! conn root-path path (get after path)))
    ;; Refresh fingerprints after our own writes so the next command is cheap.
    (let [disk (physical-files root)]
      (write-cache!
       root
       (into {}
             (map (fn [[path ^Path physical]]
                    [path (assoc (fingerprint physical)
                                 :content (get-in after [path :content]))]))
             disk)))
    {:written (vec (sort changed)) :deleted (vec (sort deleted))}))
