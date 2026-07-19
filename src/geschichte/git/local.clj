(ns geschichte.git.local
  "Direct JVM import of native Git repositories without invoking Git.

  Loose objects and complete on-disk packs enter the same exact-object store as
  HTTP/SSH fetches. Refs/config are interpreted from the common Git directory;
  linked worktrees retain their private HEAD and share objects/refs through
  `commondir`."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [geschichte.git.client :as client]
            [geschichte.git.object :as object]
            [geschichte.git.store :as store]
            [geschichte.repo :as repo])
  (:import [java.io ByteArrayOutputStream File FileInputStream]
           [java.net URI]
           [java.nio.file Files Path]
           [java.util.zip InflaterInputStream]))

(set! *warn-on-reflection* true)

(defn- canonical-file ^File [file]
  (.getCanonicalFile (io/file file)))

(defn source-file
  "Resolve a local path or file:// URL relative to `cwd`; return nil for a
  non-local transport URL."
  [cwd source]
  (cond
    (str/starts-with? source "file:")
    (canonical-file (io/file (URI. source)))

    (re-find #"^[A-Za-z][A-Za-z0-9+.-]*://" source) nil

    ;; Preserve SCP-shaped SSH remotes such as host:path.
    (and (not (str/starts-with? source "/"))
         (re-matches #"^[^/]+:.+" source)) nil

    :else
    (let [file (io/file source)]
      (canonical-file (if (.isAbsolute file) file (io/file cwd source))))))

(defn- git-pointer [^File file]
  (when (.isFile file)
    (when-let [[_ path] (re-matches #"(?s)gitdir:\s*(.+?)\s*" (slurp file))]
      (canonical-file (if (.isAbsolute (io/file path))
                        path
                        (io/file (.getParentFile file) path))))))

(defn- git-directory? [^File directory]
  (and (.isDirectory directory)
       (.isFile (io/file directory "HEAD"))
       (.isDirectory (io/file directory "objects"))))

(defn discover
  "Discover a normal, bare, or linked-worktree Git repository.

  Returns `:git-dir` for worktree-private administration, `:common-dir` for
  shared objects/refs/config, and `:work-tree` when the source is non-bare."
  [source]
  (let [^File source (canonical-file source)
        dot-git (when (.isDirectory source) (io/file source ".git"))
        [^File git-dir ^File work-tree]
        (cond
          (and dot-git (.isDirectory dot-git))
          [(canonical-file dot-git) source]

          (and dot-git (.isFile dot-git))
          [(git-pointer dot-git) source]

          (git-directory? source)
          [source nil]

          (= ".git" (.getName source))
          [(when (git-directory? source) source) (.getParentFile source)]

          :else nil)]
    (when git-dir
      (let [^File commondir-file (io/file git-dir "commondir")
            ^File common-dir (if (.isFile commondir-file)
                               (let [path (str/trim (slurp commondir-file))]
                                 (canonical-file
                                  (if (.isAbsolute (io/file path))
                                    path (io/file git-dir path))))
                               git-dir)]
        (when-not (and (git-directory? common-dir)
                       (.isFile (io/file git-dir "HEAD")))
          (throw (ex-info "invalid Git directory or commondir"
                          {:source (.getPath source)
                           :git-dir (.getPath git-dir)
                           :common-dir (.getPath common-dir)})))
        {:source source :git-dir git-dir :common-dir common-dir
         :work-tree work-tree
         :bare? (nil? work-tree)}))))

(defn require-repository [source]
  (or (discover source)
      (throw (ex-info "not a native Git repository"
                      {:source (.getPath ^File (canonical-file source))}))))

(defn- config-section [line]
  (when-let [[_ section subsection]
             (re-matches #"\s*\[([A-Za-z0-9.-]+)(?:\s+\"(.*)\")?\]\s*"
                         line)]
    (str/lower-case (str section (when subsection (str "." subsection))))))

(defn read-config
  "Read the scalar subset of Git config needed for object format, remotes, and
  upstream tracking. Repeated keys retain their last value, like `git config`'s
  ordinary scalar lookup."
  [repository]
  (let [files (distinct [(io/file (:common-dir repository) "config")
                         (io/file (:git-dir repository) "config.worktree")])]
    (reduce
     (fn [result ^File file]
       (if-not (.isFile file)
         result
         (loop [lines (str/split-lines (slurp file)), section nil, result result]
           (if-let [line (first lines)]
             (if-let [next-section (config-section line)]
               (recur (next lines) next-section result)
               (if-let [[_ key value]
                        (and section
                             (re-matches #"\s*([A-Za-z][A-Za-z0-9.-]*)\s*=\s*(.*?)\s*"
                                         line))]
                 (recur (next lines) section
                        (assoc result
                               (str section "." (str/lower-case key))
                               (str/replace value #"^\"|\"$" "")))
                 (recur (next lines) section result)))
             result))))
     {} files)))

(defn- object-format [config]
  (case (some-> (get config "extensions.objectformat") str/lower-case)
    nil :sha1
    "sha1" :sha1
    "sha256" :sha256
    (throw (ex-info "unsupported Git object format"
                    {:object-format (get config "extensions.objectformat")}))))

(defn- ref-files [^File directory]
  (let [root (io/file directory "refs")]
    (if-not (.isDirectory root)
      {}
      (with-open [paths (Files/walk (.toPath root)
                                    (make-array java.nio.file.FileVisitOption 0))]
        (into {}
              (keep (fn [^Path path]
                      (let [file (.toFile path)]
                        (when (.isFile file)
                          [(str "refs/"
                                (-> (.toString (.relativize (.toPath root) path))
                                    (str/replace #"\\" "/")))
                           (str/trim (slurp file))]))))
              (iterator-seq (.iterator paths)))))))

(defn- packed-refs [repository]
  (let [file (io/file (:common-dir repository) "packed-refs")]
    (if-not (.isFile file)
      {}
      (loop [lines (str/split-lines (slurp file)), result {}, preceding nil]
        (if-let [line (first lines)]
          (cond
            (or (str/blank? line) (str/starts-with? line "#"))
            (recur (next lines) result preceding)

            (str/starts-with? line "^")
            (recur (next lines)
                   (if preceding
                     (assoc-in result [preceding :peeled] (subs line 1)) result)
                   preceding)

            :else
            (let [[oid ref] (str/split line #" " 2)]
              (recur (next lines) (assoc result ref {:oid oid}) ref)))
          result)))))

(defn- resolve-ref [values ref seen]
  (when (contains? seen ref)
    (throw (ex-info "symbolic Git ref cycle" {:ref ref :seen seen})))
  (let [value (get values ref)]
    (if (and value (str/starts-with? value "ref:"))
      (resolve-ref values (str/trim (subs value 4)) (conj seen ref))
      value)))

(defn read-refs
  "Read loose/packed refs and the worktree-private HEAD as ls-refs-shaped maps."
  [repository]
  (let [packed (packed-refs repository)
        loose (merge (ref-files (:common-dir repository))
                     (when (not= (:common-dir repository) (:git-dir repository))
                       (ref-files (:git-dir repository))))
        values (merge (into {} (map (fn [[ref data]] [ref (:oid data)])) packed)
                      loose)
        head-value (str/trim (slurp (io/file (:git-dir repository) "HEAD")))
        head-symbolic (when (str/starts-with? head-value "ref:")
                        (str/trim (subs head-value 4)))
        head-oid (if head-symbolic
                   (resolve-ref values head-symbolic #{"HEAD"})
                   head-value)
        oid-pattern (case (:object-format repository)
                      :sha1 #"[0-9a-fA-F]{40}"
                      :sha256 #"[0-9a-fA-F]{64}")
        validate (fn [ref oid]
                   (when (and (not (str/blank? oid))
                              (not (re-matches oid-pattern oid)))
                     (throw (ex-info "invalid Git ref object ID"
                                     {:ref ref :oid oid})))
                   (some-> oid str/lower-case))]
    (into
     [(cond-> {:ref "HEAD" :oid (validate "HEAD" head-oid)
               :attributes {}}
        head-symbolic (assoc-in [:attributes :symref-target] head-symbolic))]
     (map (fn [[ref _]]
            (let [oid (resolve-ref values ref #{})]
              (cond-> {:ref ref :oid (validate ref oid) :attributes {}}
                (get-in packed [ref :peeled])
                (assoc-in [:attributes :peeled]
                          (validate ref (get-in packed [ref :peeled]))))))
          (sort-by key values)))))

(defn- alternates [^File object-directory]
  (let [file (io/file object-directory "info/alternates")]
    (if-not (.isFile file)
      []
      (->> (str/split-lines (slurp file))
           (remove str/blank?)
           (mapv (fn [path]
                   (canonical-file
                    (if (.isAbsolute (io/file path))
                      path (io/file object-directory path)))))))))

(defn- object-directories [repository]
  (loop [pending [(canonical-file (io/file (:common-dir repository) "objects"))]
         seen #{} result []]
    (if-let [directory (first pending)]
      (let [path (.getPath ^File directory)]
        (if (contains? seen path)
          (recur (next pending) seen result)
          (recur (into (vec (next pending)) (alternates directory))
                 (conj seen path) (conj result directory))))
      result)))

(defn- inflate-loose ^bytes [^File file]
  (with-open [input (InflaterInputStream. (io/input-stream file))
              output (ByteArrayOutputStream.)]
    (io/copy input output)
    (.toByteArray output)))

(defn- parse-loose [^File file oid object-format]
  (let [^bytes framed (inflate-loose file)
        ^long nul (loop [index 0]
                    (when (>= index (alength framed))
                      (throw (ex-info "loose Git object has no header terminator"
                                      {:file (.getPath file)})))
                    (if (zero? (aget framed index)) index (recur (inc index))))
        header (String. framed 0 (int nul) "US-ASCII")
        [_ type size] (re-matches #"(commit|tree|blob|tag) ([0-9]+)" header)
        ^bytes payload (java.util.Arrays/copyOfRange framed (int (inc nul))
                                                     (alength framed))]
    (when-not type
      (throw (ex-info "invalid loose Git object header"
                      {:file (.getPath file) :header header})))
    (when-not (= (parse-long size) (alength payload))
      (throw (ex-info "loose Git object size mismatch"
                      {:file (.getPath file) :declared size
                       :actual (alength payload)})))
    (let [actual (object/object-id object-format (keyword type) payload)]
      (when-not (= (str/lower-case oid) actual)
        (throw (ex-info "loose Git object ID mismatch"
                        {:file (.getPath file) :expected oid :actual actual})))
      [actual {:type (keyword type) :payload payload}])))

(defn- loose-objects [^File objects object-format]
  (let [suffix-length (case object-format :sha1 38 :sha256 62)]
    (for [^File prefix (or (seq (.listFiles objects)) [])
          :when (and (.isDirectory prefix)
                     (re-matches #"[0-9a-fA-F]{2}" (.getName prefix)))
          ^File file (or (seq (.listFiles prefix)) [])
          :when (and (.isFile file)
                     (= suffix-length (count (.getName file)))
                     (re-matches #"[0-9a-fA-F]+" (.getName file)))]
      [file (str (.getName prefix) (.getName file))])))

(defn- import-object-directory! [object-store ^File objects object-format opts]
  (let [conn (:conn object-store)
        packs (->> (or (seq (.listFiles (io/file objects "pack"))) [])
                   (filter #(and (.isFile ^File %)
                                 (str/ends-with? (.getName ^File %) ".pack")))
                   (sort-by #(.getName ^File %)))
        pack-results (mapv #(with-open [input (FileInputStream. ^File %)]
                              (store/import-pack-stream!
                               object-store input
                               (assoc opts :object-format object-format)))
                           packs)
        loose (loose-objects objects object-format)
        imported (reduce
                  (fn [count batch]
                    (let [graph {:objects
                                 (into {}
                                       (map (fn [[file oid]]
                                              (parse-loose file oid object-format)))
                                       batch)
                                 :commits {}}]
                      (+ count (:persisted
                                (store/persist-graph! object-store graph)))))
                  0 (partition-all 256 loose))]
    {:packs (count packs)
     :pack-objects (reduce + (map :persisted pack-results))
     :storage-types (apply merge-with + (map :storage-types pack-results))
     :delta-spilled? (boolean (some :delta-spilled? pack-results))
     :delta-spill-peak-live-bytes
     (reduce max 0 (keep :delta-spill-peak-live-bytes pack-results))
     :delta-spill-file-bytes
     (reduce max 0 (keep :delta-spill-file-bytes pack-results))
     :delta-parallelism (reduce max 1 (keep :delta-parallelism pack-results))
     :timings-ms (apply merge-with + (map :timings-ms pack-results))
     :loose-objects imported}))

(defn- import-config! [conn config source remote clone?]
  (doseq [[key value] config
          :when (or (= "extensions.objectformat" key)
                    (and (not clone?)
                         (or (str/starts-with? key "remote.")
                             (str/starts-with? key "branch.")
                             (contains? #{"user.name" "user.email"} key))))]
    (repo/set-config! conn key value))
  (when clone?
    (repo/set-config! conn (str "remote." remote ".url") source)))

(defn- checkout-imported! [conn object-store refs branch format]
  (let [head (first refs)
        selected-ref (when branch (str "refs/heads/" branch))
        selected (if selected-ref
                   (some #(when (= selected-ref (:ref %)) %) refs)
                   head)]
    (cond
      (:oid selected)
      (client/checkout-fetched! conn {:refs refs}
                                {:branch branch :object-format format
                                 :object-store object-store
                                 :force? true})

      branch
      (throw (ex-info (str "Git branch " branch " has no importable tip")
                      {:branch branch :refs refs}))

      :else
      (let [ref (or (get-in head [:attributes :symref-target])
                    "refs/heads/main")]
        (when-not (contains? (repo/refs conn) ref)
          (repo/create-ref! conn ref))
        (when-not (= ref (repo/current-ref conn))
          (repo/checkout! conn ref {:force? true}))
        {:empty? true :ref ref :files 0 :imported-commits 0
         :fetched {:refs refs}}))))

(defn import!
  "Import a local Git repository into an initialized Geschichte connection.

  Options include `:remote`, `:branch`, and `:clone?`. The selected HEAD is
  materialized lazily through the same boundary used by network fetch."
  [conn source {:keys [remote branch clone? no-checkout?]
                :or {remote "origin"}
                :as opts}]
  (let [repository (require-repository source)
        config (read-config repository)
        format (object-format config)
        repository (assoc repository :object-format format)
        shallow (io/file (:git-dir repository) "shallow")
        grafts (io/file (:common-dir repository) "info/grafts")]
    (when (or (.isFile shallow)
              (.isFile (io/file (:common-dir repository) "shallow")))
      (throw (ex-info "shallow Git repositories cannot be imported completely"
                      {:source (.getPath ^File (:source repository))})))
    (when (.isFile grafts)
      (throw (ex-info "Git grafts must be made permanent before import"
                      {:source (.getPath ^File (:source repository))})))
    (when (or (= "reftable" (some-> (get config "extensions.refstorage")
                                    str/lower-case))
              (contains? config "extensions.partialclone")
              (some (fn [[key value]]
                      (and (str/ends-with? key ".promisor")
                           (= "true" (str/lower-case value))))
                    config))
      (throw (ex-info "reftable and partial-clone Git repositories are not yet importable"
                      {:source (.getPath ^File (:source repository))})))
    (let [refs (read-refs repository)
          source-string (.getPath ^File (:source repository))
          _ (when (some #(str/starts-with? (:ref %) "refs/replace/") refs)
              (throw (ex-info "Git replace refs must be made permanent before import"
                              {:source source-string})))
          object-directories (object-directories repository)]
      (store/with-store
        conn
        (fn [object-store]
          (let [storage (mapv #(import-object-directory!
                                object-store % format opts)
                              object-directories)]
            (store/record-refs! conn remote refs)
            (import-config! conn config source-string remote clone?)
            (let [checkout (if no-checkout?
                             {:skipped? true :fetched {:refs refs}}
                             (checkout-imported! conn object-store refs branch
                                                 format))]
              (when (and clone?
                         (str/starts-with? (repo/current-ref conn) "refs/heads/"))
                (let [ref (repo/current-ref conn)
                      short (subs ref (count "refs/heads/"))]
                  (repo/set-config! conn (str "branch." short ".remote") remote)
                  (repo/set-config! conn (str "branch." short ".merge") ref)))
              {:source source-string
               :git-dir (.getPath ^File (:git-dir repository))
               :common-dir (.getPath ^File (:common-dir repository))
               :object-format format
               :refs (count refs)
               :packs (reduce + (map :packs storage))
               :pack-objects (reduce + (map :pack-objects storage))
               :storage-types (apply merge-with + (map :storage-types storage))
               :delta-spilled? (boolean (some :delta-spilled? storage))
               :delta-spill-peak-live-bytes
               (reduce max 0 (keep :delta-spill-peak-live-bytes storage))
               :delta-spill-file-bytes
               (reduce max 0 (keep :delta-spill-file-bytes storage))
               :delta-parallelism
               (reduce max 1 (keep :delta-parallelism storage))
               :timings-ms (apply merge-with + (map :timings-ms storage))
               :loose-objects (reduce + (map :loose-objects storage))
               :checkout checkout})))))))
