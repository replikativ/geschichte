(ns geschichte.workspace-physical
  "JVM lifecycle adapter for physical Geschichte workspace projections.

  The registry is local coordination metadata. Repository history and file
  content remain in Datahike; every workspace receives a structurally shared
  Datahike branch and may independently check out the same logical Git ref."
  (:refer-clojure :exclude [list remove])
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [datahike.api :as d]
            [geschichte.git.revision :as revision]
            [geschichte.projection :as projection]
            [geschichte.repo :as repo]
            [geschichte.workspace :as workspace])
  (:import [java.io File RandomAccessFile]
           [java.nio.file AtomicMoveNotSupportedException Files LinkOption Path
            StandardCopyOption]
           [java.nio.file.attribute FileAttribute]
           [java.util UUID]))

(set! *warn-on-reflection* true)

(def registry-version 1)
(def interrupted-grace-ms (* 5 60 1000))

(declare branch-config)

(defn- registry-file ^File [locator]
  (io/file (.getParentFile ^File (:canonical-marker locator)) "workspaces.edn"))

(defn- lock-file ^File [locator]
  (io/file (.getParentFile ^File (:canonical-marker locator)) "workspaces.lock"))

(defn- empty-registry [locator]
  {:geschichte/version registry-version
   :canonical-marker (.getPath ^File (:canonical-marker locator))
   :workspaces {}})

(defn- read-registry [locator]
  (let [file (registry-file locator)]
    (if (.isFile file)
      (merge (empty-registry locator) (edn/read-string (slurp file)))
      (empty-registry locator))))

(defn- atomic-write! [^File destination value]
  (.mkdirs (.getParentFile destination))
  (let [^Path parent (.toPath (.getParentFile destination))
        ^Path temporary (Files/createTempFile parent "workspaces-" ".tmp"
                                              (make-array FileAttribute 0))]
    (try
      (spit (.toFile temporary) (str (pr-str value) "\n"))
      (try
        (Files/move temporary (.toPath destination)
                    (into-array StandardCopyOption
                                [StandardCopyOption/REPLACE_EXISTING
                                 StandardCopyOption/ATOMIC_MOVE]))
        (catch AtomicMoveNotSupportedException _
          (Files/move temporary (.toPath destination)
                      (into-array StandardCopyOption
                                  [StandardCopyOption/REPLACE_EXISTING]))))
      (finally (Files/deleteIfExists temporary)))))

(defn- update-registry! [locator f]
  (with-open [random-access (RandomAccessFile. (lock-file locator) "rw")
              channel (.getChannel random-access)
              held (.lock channel)]
    (let [updated (f (read-registry locator))]
      (atomic-write! (registry-file locator) updated)
      updated)))

(defn- record [locator]
  {:workspace/id (:workspace-id locator)
   :workspace/path (.getPath ^File (projection/canonical-file (:root locator)))
   :workspace/branch (:workspace-branch locator)
   :workspace/state :active
   :workspace/publication (:publication locator)})

(defn ensure-registered!
  "Migrate an old projection marker if necessary and register it locally.
  Returns the possibly refreshed locator."
  [locator]
  (if-not (:projection? locator)
    locator
    (let [id (or (:workspace-id locator) (UUID/randomUUID))
          canonical-marker (or (:canonical-marker locator) (:marker locator))
          ;; The publication branch deliberately has no physical checkout.
          ;; Existing pre-release roots that pointed at :db are cheaply forked.
          branch (if (= workspace/canonical-branch (:workspace-branch locator))
                   (workspace/branch-key (str "workspace-" id))
                   (:workspace-branch locator))
          _ (when (and (= workspace/canonical-branch
                          (:workspace-branch locator))
                       (d/database-exists? (:config locator)))
              (let [canonical (d/connect (:config locator))]
                (try
                  (when-not (contains? (set (d/branches canonical)) branch)
                    (workspace/fork! canonical branch))
                  (finally (d/release canonical)))))
          migrated? (or (nil? (:workspace-id locator))
                        (not= branch (:workspace-branch locator)))
          locator
          (if migrated?
            (do
              (projection/write-marker!
               (:root locator) (branch-config locator branch)
               {:workspace/id id
                :workspace/branch branch
                :workspace/publication :manual
                :workspace/canonical-marker (.getPath ^File canonical-marker)})
              (projection/read-marker (:marker locator)))
            locator)
          entry (record locator)]
      (update-registry!
       locator
       #(assoc-in % [:workspaces (:workspace-id locator)] entry))
      locator)))

(defn- relative-to [^File directory path]
  (let [^File file (io/file path)]
    (projection/canonical-file
     (if (.isAbsolute file) file (io/file directory path)))))

(defn- same-or-descendant? [^File parent ^File child]
  (.startsWith (.toPath child) (.toPath parent)))

(defn- overlapping? [^File left ^File right]
  (or (same-or-descendant? left right)
      (same-or-descendant? right left)))

(defn- branch-config [locator branch]
  (assoc (:config locator) :branch branch))

(defn- connect-record [locator entry]
  (let [path (io/file (:workspace/path entry))
        marker (projection/marker-file path)]
    (when (and (.isFile marker)
               (= (:canonical-marker locator)
                  (:canonical-marker (projection/read-marker marker))))
      (d/connect (branch-config locator (:workspace/branch entry))))))

(defn list-records
  "Return Git-shaped records for registered, live physical workspaces."
  [locator]
  (->> (:workspaces (read-registry locator))
       vals
       (filter #(= :active (:workspace/state %)))
       (keep (fn [entry]
               (when-let [conn (connect-record locator entry)]
                 (try
                   {:path (:workspace/path entry)
                    :head (some-> (repo/head-commit conn)
                                  :geschichte.commit/id str)
                    :branch (repo/current-ref conn)}
                   (finally (d/release conn))))))
       (sort-by :path)
       vec))

(defn- ensure-empty-destination! [locator ^File target]
  ;; Fast preflight avoids creating a directory inside an existing workspace;
  ;; reserve! repeats this check while holding the registry lock.
  (when (some #(overlapping? target (io/file (:workspace/path %)))
              (vals (:workspaces (read-registry locator))))
    (throw (ex-info "worktree path overlaps a registered Geschichte workspace"
                    {:path (.getPath target)})))
  (when (and (.exists target)
             (or (not (.isDirectory target)) (seq (.list target))))
    (throw (ex-info "worktree path already exists and is not empty"
                    {:path (.getPath target)})))
  (.mkdirs target)
  (when-not (.isDirectory target)
    (throw (ex-info "could not create worktree directory"
                    {:path (.getPath target)}))))

(defn- reserve! [locator id entry destination]
  (update-registry!
   locator
   (fn [registry]
     (when (some #(overlapping? destination (io/file (:workspace/path %)))
                 (vals (:workspaces registry)))
       (throw (ex-info "worktree path overlaps a registered Geschichte workspace"
                       {:path (.getPath ^File destination)})))
     (assoc-in registry [:workspaces id] entry))))

(defn- delete-tree! [^File root]
  (when (.exists root)
    (with-open [paths (Files/walk (.toPath root) (make-array java.nio.file.FileVisitOption 0))]
      (doseq [^Path path (reverse (sort-by #(.getNameCount ^Path %)
                                           (iterator-seq (.iterator paths))))]
        (Files/deleteIfExists path)))))

(defn add-worktree!
  "Fork a Datahike workspace and materialize it at `:path`. Registration uses
  creating/active states so a failed lifecycle can be recovered by prune."
  [source locator cwd {:keys [path target] :as options}]
  (let [^File destination (relative-to cwd path)
        existed? (.exists destination)
        target-commit (revision/require source (or target "HEAD"))
        id (UUID/randomUUID)
        branch (workspace/branch-key (str "workspace-" id))
        entry {:workspace/id id
               :workspace/path (.getPath destination)
               :workspace/branch branch
               :workspace/state :creating
               :workspace/updated-at (System/currentTimeMillis)
               :workspace/publication :manual}]
    (ensure-empty-destination! locator destination)
    (reserve! locator id entry destination)
    (try
      (workspace/fork! source branch)
      (let [config (branch-config locator branch)
            conn (d/connect config)]
        (try
          (workspace/prepare-checkout! conn target-commit options)
          (projection/write-marker!
           destination config
           {:workspace/id id
            :workspace/branch branch
            :workspace/publication :manual
            :workspace/canonical-marker
            (.getPath ^File (:canonical-marker locator))})
          (projection/materialize-changes! conn destination {} (repo/worktree conn))
          (finally (d/release conn))))
      (update-registry!
       locator #(-> %
                    (assoc-in [:workspaces id :workspace/state] :active)
                    (assoc-in [:workspaces id :workspace/updated-at]
                              (System/currentTimeMillis))))
      (.getPath destination)
      (catch Throwable error
        (try (workspace/remove! source branch) (catch Throwable _))
        (try (delete-tree! destination) (catch Throwable _))
        (when existed? (.mkdirs destination))
        (update-registry! locator #(update % :workspaces dissoc id))
        (throw error)))))

(defn- find-entry [locator cwd path]
  (let [target (.getPath ^File (relative-to cwd path))]
    (some (fn [[id entry]]
            (when (= target (:workspace/path entry)) [id entry]))
          (:workspaces (read-registry locator)))))

(defn remove-worktree!
  "Remove one registered physical workspace and its Datahike branch."
  [source locator cwd {:keys [path force?]}]
  (let [[id entry] (or (find-entry locator cwd path)
                       (throw (ex-info "not a registered Geschichte worktree"
                                       {:path path})))
        root (io/file (:workspace/path entry))]
    (when (= id (:workspace-id locator))
      (throw (ex-info "cannot remove the current Geschichte worktree"
                      {:path (.getPath root)})))
    (let [conn (or (connect-record locator entry)
                   (throw (ex-info "worktree marker is missing or belongs to another repository"
                                   {:path (.getPath root)})))]
      (try
        (projection/scan! conn root)
        (let [status (repo/status conn)]
          (when (and (not force?) (not (:clean? status)))
            (throw (ex-info "worktree contains modified or untracked files"
                            {:path (.getPath root) :status status}))))
        (finally (d/release conn))))
    (update-registry! locator #(-> %
                                   (assoc-in [:workspaces id :workspace/state]
                                             :removing)
                                   (assoc-in [:workspaces id :workspace/updated-at]
                                             (System/currentTimeMillis))))
    (workspace/remove! source (:workspace/branch entry))
    (delete-tree! root)
    (update-registry! locator #(update % :workspaces dissoc id))
    (.getPath root)))

(defn prune-worktrees!
  "Reconcile interrupted and physically missing workspace projections."
  [source locator _options]
  (let [branches (set (d/branches source))
        entries (:workspaces (read-registry locator))
        stale (filter
               (fn [[_ entry]]
                 (let [marker (projection/marker-file (:workspace/path entry))
                       interrupted? (and (not= :active (:workspace/state entry))
                                         (> (- (System/currentTimeMillis)
                                               (or (:workspace/updated-at entry) 0))
                                            interrupted-grace-ms))]
                   (or interrupted?
                       (not (.isFile marker))
                       (not (contains? branches (:workspace/branch entry))))))
               entries)]
    (doseq [[id entry] stale]
      (when (contains? branches (:workspace/branch entry))
        (try (workspace/remove! source (:workspace/branch entry))
             (catch Throwable _)))
      (when (not= id (:workspace-id locator))
        (try (delete-tree! (io/file (:workspace/path entry)))
             (catch Throwable _)))
      (update-registry! locator #(update % :workspaces dissoc id)))
    (mapv (comp :workspace/path second) stale)))

(defn operations
  "Build the host adapter consumed by Geschichte's shared Git command engine."
  [source locator cwd]
  {:list #(list-records locator)
   :add #(add-worktree! source locator cwd %)
   :remove #(remove-worktree! source locator cwd %)
   :prune #(prune-worktrees! source locator %)})

(defn publish!
  "Publish this workspace's current logical ref into the canonical branch."
  [locator workspace-conn]
  (when (= workspace/canonical-branch (:workspace-branch locator))
    (throw (ex-info "the canonical workspace is already published" {})))
  (let [canonical (d/connect (branch-config locator workspace/canonical-branch))]
    (try
      (workspace/publish! canonical workspace-conn {:create? true})
      (finally (d/release canonical)))))

(defn advance!
  "Advance this clean workspace from its corresponding canonical ref and
  materialize the resulting tree into the physical projection."
  [locator workspace-conn]
  (when (= workspace/canonical-branch (:workspace-branch locator))
    (throw (ex-info "the canonical workspace cannot advance from itself" {})))
  (let [canonical (d/connect (branch-config locator workspace/canonical-branch))
        before (repo/worktree workspace-conn)]
    (try
      (let [result (workspace/advance! canonical workspace-conn)
            after (repo/worktree workspace-conn)]
        (projection/materialize-changes! workspace-conn (:root locator)
                                         before after)
        result)
      (finally (d/release canonical)))))
