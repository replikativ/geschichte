(ns geschichte.repo
  "Portable Datahike-backed repository API.

  Pure current-db views return directly. Operations which transact, load a
  historical Datahike checkpoint, or access payloads return partial-cps
  computations. JVM callers retain direct synchronous values."
  (:refer-clojure :exclude [await read remove reset!])
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [datahike.api :as d]
            [geschichte.async :as execution]
            [geschichte.bytes :as bytes]
            [geschichte.content :as content]
            [geschichte.query :as query]
            [geschichte.schema :as schema]
            [is.simm.partial-cps.async :refer [await]]
            #?(:clj [clojure.java.io :as io]))
  #?(:cljs (:require-macros [geschichte.macros :refer [platform-async]]))
  #?(:clj (:require [geschichte.macros :refer [platform-async]])))

(def default-branch "refs/heads/main")
(def default-mode 33188)

(defn- normalize-path [path]
  (let [path (-> (str path)
                 (str/replace #"\\" "/")
                 (str/replace #"^/+" ""))
        parts (clojure.core/remove #(or (str/blank? %) (= "." %))
                                   (str/split path #"/+"))]
    (when (or (str/blank? path) (some #{".."} parts))
      (throw (ex-info "Invalid repository path" {:path path})))
    (str/join "/" parts)))

(defn- transact-data [conn tx-data]
  (execution/io-result #?(:clj (d/transact conn tx-data)
                          :cljs (d/transact! conn tx-data))
                       execution/default-opts))

(defn initialized? [conn]
  (query/initialized? @conn))

(defn configuration [conn]
  (query/configuration @conn))

(defn set-config!
  "Persist one repository-local string configuration value."
  [conn key value]
  (platform-async
   (await (transact-data conn [{:geschichte.config/key (str key)
                                :geschichte.config/value (str value)}]))
   value))

(defn unset-config!
  "Remove one repository-local configuration value."
  [conn key]
  (platform-async
   (when-let [eid (d/q '[:find ?entry .
                         :in $ ?key
                         :where [?entry :geschichte.config/key ?key]]
                       @conn (str key))]
     (await (transact-data conn [[:db/retractEntity eid]])))
   true))

(defn init!
  "Install Geschichte's schema and initialize an unborn branch."
  ([conn] (init! conn {}))
  ([conn {:keys [name branch]
          :or {name "repository" branch default-branch}}]
   (platform-async
    (let [installed (set (d/q '[:find [?ident ...]
                                :where [_ :db/ident ?ident]] @conn))
          missing (filterv #(not (contains? installed (:db/ident %)))
                           schema/schema)]
      (when (seq missing)
        (await (transact-data conn missing))))
    (if-let [repo (query/repository @conn)]
      (:geschichte.repo/id repo)
      (let [id (random-uuid)]
        (await
         (transact-data conn [{:geschichte.repo/id id
                               :geschichte.repo/name name
                               :geschichte.repo/head branch}
                              {:geschichte.ref/name branch}]))
        id)))))

(defn current-ref [conn]
  (query/current-ref @conn))

(defn- ref-entity [db ref-name]
  (d/q '[:find (pull ?ref [:db/id :geschichte.ref/name
                           {:geschichte.ref/target
                            [:db/id :geschichte.commit/id
                             :geschichte.commit/snapshot]}]) .
         :in $ ?name
         :where [?ref :geschichte.ref/name ?name]]
       db ref-name))

(defn head-commit [conn]
  (some-> (ref-entity @conn (current-ref conn)) :geschichte.ref/target))

(defn commit-by-id [conn id]
  (query/commit @conn id))

(defn- work-row [db path]
  (d/q '[:find (pull ?entry [:db/id :geschichte.work/path
                             :geschichte.work/size :geschichte.work/mode
                             {:geschichte.work/content
                              [:geschichte.content/id]}]) .
         :in $ ?path
         :where [?entry :geschichte.work/path ?path]] db path))

(defn- stage-row [db path]
  (d/q '[:find (pull ?entry [:db/id :geschichte.stage/path
                             :geschichte.stage/state :geschichte.stage/size
                             :geschichte.stage/mode
                             {:geschichte.stage/content
                              [:geschichte.content/id]}]) .
         :in $ ?path
         :where [?entry :geschichte.stage/path ?path]] db path))

(defn- stage-map [db]
  (into (sorted-map)
        (keep (fn [[path {:keys [state] :as entry}]]
                (when (= :present state)
                  [path (dissoc entry :state)])))
        (query/stage db)))

(defn- snapshot-db [conn commit]
  (platform-async
   (when-let [cid (:geschichte.commit/snapshot commit)]
     (or (await (execution/io-result
                 (d/commit-as-db conn cid)
                 execution/default-opts))
         (throw (ex-info
                 "Geschichte commit names a missing Datahike checkpoint"
                 {:commit (:geschichte.commit/id commit)
                  :snapshot cid}))))))

(defn- head-map [conn]
  (platform-async
   (if-let [commit (head-commit conn)]
     (stage-map (await (snapshot-db conn commit)))
     {})))

(declare tree-at)

(defn index-tree
  "Return the current staging index as a sorted path-to-entry map. Explicit
   deletion markers are omitted, so the result has ordinary tree semantics."
  [conn]
  (stage-map @conn))

(defn tree
  "Return a repository tree selected by `source`.

   Portable source names are `:head`, `:index`, `:worktree`, and `:empty`.
   A logical commit map or UUID selects that historical committed tree."
  ([conn] (tree conn :head))
  ([conn source]
   (platform-async
    (cond
      (= source :empty) {}
      (= source :index) (index-tree conn)
      (= source :worktree) (query/worktree @conn)
      (or (nil? source) (= source :head)) (await (head-map conn))
      :else (await (tree-at conn source))))))

(defn read-entry
  "Load the bytes named by a tree entry returned by `tree` or `changes`."
  [conn entry]
  (platform-async
   (when-let [id (:content entry)]
     (await (content/read-by-id conn id)))))

(defn- change-kind [before after]
  (cond
    (and (nil? before) after) :added
    (and before (nil? after)) :deleted
    (not= before after) :modified))

(defn changes
  "Compare two repository trees without loading their content payloads.

   Returns sorted `{:path :kind :before :after}` records. Sources use the same
   vocabulary as `tree`, making this suitable for status, diff UIs, and CLI
   compatibility layers on both the JVM and ClojureScript."
  ([conn] (changes conn :index :worktree))
  ([conn from to]
   (platform-async
    (let [before-tree (await (tree conn from))
          after-tree (await (tree conn to))
          paths (sort (set/union (set (keys before-tree))
                                 (set (keys after-tree))))]
      (->> paths
           (keep (fn [path]
                   (let [before (get before-tree path)
                         after (get after-tree path)]
                     (when-let [kind (change-kind before after)]
                       {:path path :kind kind
                        :before before :after after}))))
           vec)))))

(defn status-entries
  "Return one structured three-tree status record per changed path.

   `:index` describes HEAD-to-index change and `:worktree` describes
   index-to-worktree change. An unindexed worktree path is `:untracked`."
  [conn]
  (platform-async
   (let [head (await (tree conn :head))
         index (index-tree conn)
         work (query/worktree @conn)
         paths (sort (set/union (set (keys head))
                                (set (keys index))
                                (set (keys work))))]
     (->> paths
          (keep (fn [path]
                  (let [head-entry (get head path)
                        index-entry (get index path)
                        work-entry (get work path)
                        index-kind (change-kind head-entry index-entry)
                        work-kind (if index-entry
                                    (change-kind index-entry work-entry)
                                    (when work-entry :untracked))]
                    (when (or index-kind work-kind)
                      (cond-> {:path path}
                        index-kind (assoc :index index-kind)
                        work-kind (assoc :worktree work-kind))))))
          vec))))

(defn tree-at
  "Return path metadata for a logical commit, or current HEAD when omitted."
  ([conn]
   (head-map conn))
  ([conn commit]
   (platform-async
    (let [commit (cond
                   (map? commit) commit
                   (uuid? commit)
                   (d/pull @conn [:geschichte.commit/id
                                  :geschichte.commit/snapshot]
                           [:geschichte.commit/id commit])
                   :else nil)]
      (when-not (:geschichte.commit/snapshot commit)
        (throw (ex-info "Unknown Geschichte commit" {:commit commit})))
      (stage-map (await (snapshot-db conn commit)))))))

(defn read-at [conn commit path]
  (platform-async
   (let [commit (cond
                  (map? commit) commit
                  (uuid? commit)
                  (d/pull @conn [:geschichte.commit/id
                                 :geschichte.commit/snapshot]
                          [:geschichte.commit/id commit])
                  :else nil)]
     (when-not (:geschichte.commit/snapshot commit)
       (throw (ex-info "Unknown Geschichte commit" {:commit commit})))
     (let [snapshot (await (snapshot-db conn commit))
           id (get-in (stage-map snapshot)
                      [(normalize-path path) :content])]
       (when id
         ;; Resolve content metadata from the immutable commit snapshot, not
         ;; from the caller's current branch. Published workspace commits are
         ;; intentionally absent from the canonical mutable catalog; their
         ;; snapshot and store-ref payload graph remain reachable via Datahike
         ;; merge ancestry.
         (await (content/read-by-id (atom snapshot) id)))))))

(defn write!
  "Write bytes into the working tree through the delta representation layer."
  ([conn path value] (write! conn path value {}))
  ([conn path value {:keys [mode] :or {mode default-mode} :as opts}]
   (platform-async
    (let [path (normalize-path path)
          size (bytes/length value)
          base-id (get-in (work-row @conn path)
                          [:geschichte.work/content :geschichte.content/id])
          content-id
          (await
           (content/transact-content!
            conn value base-id
            (fn [id]
              [{:geschichte.work/path path
                :geschichte.work/content [:geschichte.content/id id]
                :geschichte.work/size (long size)
                :geschichte.work/mode (long mode)}])
            opts))]
      {:path path :content content-id :size size :mode mode}))))

#?(:clj
   (defn write-file!
     "Ingest a physical file into the working tree with bounded heap use."
     ([conn path file] (write-file! conn path file {}))
     ([conn path file {:keys [mode] :or {mode default-mode} :as opts}]
      (let [path (normalize-path path)
            file (io/file file)
            size (.length ^java.io.File file)
            base-id (get-in (work-row @conn path)
                            [:geschichte.work/content :geschichte.content/id])
            content-id
            (content/transact-file!
             conn file base-id
             (fn [id]
               [{:geschichte.work/path path
                 :geschichte.work/content [:geschichte.content/id id]
                 :geschichte.work/size (long size)
                 :geschichte.work/mode (long mode)}])
             opts)]
        {:path path :content content-id :size size :mode mode}))))

(defn read
  "Read working-tree bytes, or nil when absent."
  [conn path]
  (platform-async
   (when-let [id (some-> (work-row @conn (normalize-path path))
                         :geschichte.work/content
                         :geschichte.content/id)]
     (await (content/read-by-id conn id)))))

#?(:clj
   (defn copy-to!
     "Copy a working-tree file to an OutputStream without assembling chunked
     content in memory. Returns nil when the path is absent."
     [conn path out]
     (when-let [id (some-> (work-row @conn (normalize-path path))
                           :geschichte.work/content
                           :geschichte.content/id)]
       (content/copy-by-id! conn id out))))

(defn files [conn]
  (->> (keys (query/worktree @conn)) sort vec))

(defn worktree [conn]
  (query/worktree @conn))

(defn remove! [conn path]
  (platform-async
   (when-let [eid (:db/id (work-row @conn (normalize-path path)))]
     (await (transact-data conn [[:db/retractEntity eid]]))
     true)))

(defn- stage-tx [db path]
  (if-let [work (work-row db path)]
    [{:geschichte.stage/path path
      :geschichte.stage/state :present
      :geschichte.stage/content
      [:geschichte.content/id
       (get-in work [:geschichte.work/content :geschichte.content/id])]
      :geschichte.stage/size (:geschichte.work/size work)
      :geschichte.stage/mode (:geschichte.work/mode work)}]
    (if-let [stage (stage-row db path)]
      [[:db/retract (:db/id stage) :geschichte.stage/content
        [:geschichte.content/id
         (get-in stage [:geschichte.stage/content :geschichte.content/id])]]
       [:db/retract (:db/id stage) :geschichte.stage/size
        (:geschichte.stage/size stage)]
       [:db/retract (:db/id stage) :geschichte.stage/mode
        (:geschichte.stage/mode stage)]
       [:db/add (:db/id stage) :geschichte.stage/state :deleted]]
      [])))

(defn stage! [conn paths]
  (platform-async
   (let [paths (mapv normalize-path paths)
         tx-data (vec (mapcat #(stage-tx @conn %) paths))]
     (when (seq tx-data)
       (await (transact-data conn tx-data)))
     paths)))

(defn stage-all! [conn]
  (platform-async
   (let [paths (sort (set/union
                      (set (keys (query/stage @conn)))
                      (set (keys (await (head-map conn))))
                      (set (keys (query/worktree @conn)))))]
     (await (stage! conn paths)))))

(defn- changed-paths [a b]
  (->> (set/union (set (keys a)) (set (keys b)))
       (filter #(not= (get a %) (get b %)))
       sort vec))

(defn status
  "Return Git-shaped three-tree status data."
  [conn]
  (platform-async
   (let [head (await (head-map conn))
         index (stage-map @conn)
         work (query/worktree @conn)
         tracked (set (keys index))]
     {:branch (current-ref conn)
      :head (some-> (head-commit conn) :geschichte.commit/id)
      :staged (changed-paths head index)
      :unstaged (->> (changed-paths index work)
                     (filter tracked) vec)
      :untracked (->> (set/difference (set (keys work)) tracked)
                      sort vec)
      :clean? (and (= head index) (= index work))})))

(defn refs [conn]
  (query/refs @conn))

(defn- merge-parent [conn]
  (d/q '[:find (pull ?commit [:db/id :geschichte.commit/id]) .
         :where
         [?repo :geschichte.repo/id]
         [?repo :geschichte.repo/merge-parent ?commit]] @conn))

(defn commit!
  "Publish the current staging index as a logical Geschichte commit.

  With `:amend? true`, replace the current ref tip while retaining the
  amended commit's parents. The old commit remains addressable by ID, matching
  Git's immutable-object model and making reflog support possible later."
  [conn {:keys [message author time amend?]
         :or {author "unknown"}}]
  (platform-async
   (when (str/blank? message)
     (throw (ex-info "Commit message must not be blank" {})))
   (let [head (head-commit conn)
         second-parent (merge-parent conn)
         head-tree (await (head-map conn))
         index (stage-map @conn)]
     (when (and (= head-tree index) (nil? second-parent) (not amend?))
       (throw (ex-info "Nothing staged to commit"
                       {:status (await (status conn))})))
     (let [id (random-uuid)
           snapshot (d/commit-id @conn)
           ref-name (current-ref conn)
           parents (if amend?
                     (mapv (fn [parent]
                             [:geschichte.commit/id
                              (:geschichte.commit/id parent)])
                           (:geschichte.commit/parents head))
                     (cond-> []
                       head (conj [:geschichte.commit/id
                                   (:geschichte.commit/id head)])
                       second-parent
                       (conj [:geschichte.commit/id
                              (:geschichte.commit/id second-parent)])))
           commit (cond-> {:geschichte.commit/id id
                           :geschichte.commit/snapshot snapshot
                           :geschichte.commit/message message
                           :geschichte.commit/author author
                           :geschichte.commit/time
                           (or time #?(:clj (java.util.Date.)
                                       :cljs (js/Date.)))}
                    (seq parents)
                    (assoc :geschichte.commit/parents parents))
           tx-data
           (cond-> [commit
                    {:geschichte.ref/name ref-name
                     :geschichte.ref/target [:geschichte.commit/id id]}]
             second-parent
             (conj [:db/retract
                    (:db/id (query/repository @conn))
                    :geschichte.repo/merge-parent
                    (:db/id second-parent)]))]
       (await (transact-data conn tx-data))
       (assoc commit :geschichte.ref/name ref-name)))))

(defn create-ref!
  "Create a logical ref at `commit` (or current HEAD when omitted)."
  ([conn name] (create-ref! conn name nil))
  ([conn name commit]
   (platform-async
    (let [name (if (str/starts-with? name "refs/")
                 name
                 (str "refs/heads/" name))]
      (when (ref-entity @conn name)
        (throw (ex-info "Ref already exists" {:ref name})))
      (let [head (or commit (head-commit conn))]
        (await
         (transact-data
          conn
          [(cond-> {:geschichte.ref/name name}
             head (assoc :geschichte.ref/target
                         [:geschichte.commit/id
                          (:geschichte.commit/id head)]))]))
        name)))))

(defn branch!
  "Create a logical Geschichte branch at HEAD without switching to it."
  [conn name]
  (create-ref! conn name))

(defn rename-ref!
  "Rename a logical ref without changing its target. With `:force?`, replace an
  existing destination ref. The checked-out ref name follows the rename."
  ([conn old-name new-name] (rename-ref! conn old-name new-name {}))
  ([conn old-name new-name {:keys [force?]}]
   (platform-async
    (let [normalize #(if (str/starts-with? % "refs/") % (str "refs/heads/" %))
          old-name (normalize old-name)
          new-name (normalize new-name)
          old-ref (or (ref-entity @conn old-name)
                      (throw (ex-info "Unknown ref" {:ref old-name})))
          existing (ref-entity @conn new-name)]
      (when (and existing (not force?))
        (throw (ex-info "Ref already exists" {:ref new-name})))
      (let [target-id (get-in old-ref [:geschichte.ref/target
                                       :geschichte.commit/id])
            current? (= old-name (current-ref conn))
            repo-id (:geschichte.repo/id (query/repository @conn))
            tx-data (cond-> [[:db/retractEntity (:db/id old-ref)]
                             (cond-> {:geschichte.ref/name new-name}
                               target-id
                               (assoc :geschichte.ref/target
                                      [:geschichte.commit/id target-id]))]
                      existing (into [[:db/retractEntity (:db/id existing)]])
                      current? (conj {:geschichte.repo/id repo-id
                                      :geschichte.repo/head new-name}))]
        (await (transact-data conn tx-data))
        new-name)))))

(defn set-ref!
  "Move an existing logical ref to `commit` without changing a worktree."
  [conn name commit]
  (platform-async
   (let [name (if (str/starts-with? name "refs/") name (str "refs/heads/" name))
         ref (or (ref-entity @conn name)
                 (throw (ex-info "Unknown ref" {:ref name})))
         commit (if (map? commit) commit (commit-by-id conn commit))]
     (when (= name (current-ref conn))
       (throw (ex-info "Cannot force-update the checked out branch" {:ref name})))
     (when-not (:geschichte.commit/id commit)
       (throw (ex-info "Unknown commit" {:commit commit})))
     (await (transact-data conn [{:db/id (:db/id ref)
                                  :geschichte.ref/target
                                  [:geschichte.commit/id
                                   (:geschichte.commit/id commit)]}]))
     name)))

(defn- retract-entities-tx [db attr]
  (mapv (fn [[eid]] [:db/retractEntity eid])
        (d/q '[:find ?e :in $ ?attr :where [?e ?attr]] db attr)))

(defn- retract-paths-tx [db attr paths]
  (mapv (fn [[eid]] [:db/retractEntity eid])
        (d/q '[:find ?e
               :in $ ?attr [?path ...]
               :where [?e ?attr ?path]]
             db attr (vec paths))))

(defn- materialize-tx [tree]
  (vec
   (mapcat
    (fn [[path {:keys [content size mode]}]]
      [{:geschichte.work/path path
        :geschichte.work/content [:geschichte.content/id content]
        :geschichte.work/size size
        :geschichte.work/mode mode}
       {:geschichte.stage/path path
        :geschichte.stage/state :present
        :geschichte.stage/content [:geschichte.content/id content]
        :geschichte.stage/size size
        :geschichte.stage/mode mode}])
    tree)))

(defn- work-entry-tx [path {:keys [content size mode]}]
  {:geschichte.work/path path
   :geschichte.work/content [:geschichte.content/id content]
   :geschichte.work/size size
   :geschichte.work/mode mode})

(defn- stage-entry-tx [path {:keys [content size mode]}]
  {:geschichte.stage/path path
   :geschichte.stage/state :present
   :geschichte.stage/content [:geschichte.content/id content]
   :geschichte.stage/size size
   :geschichte.stage/mode mode})

(defn restore-paths!
  "Restore exact repository paths from the index, HEAD, or a logical commit.

   Options:
   - `:source` is `:index`, `:head`, a commit map, or commit UUID.
   - `:staged?` replaces index entries.
   - `:worktree?` replaces working-tree entries.

   Missing source entries remove the corresponding destination entry. Other
   paths, including untracked files, are preserved."
  ([conn paths] (restore-paths! conn paths {}))
  ([conn paths {:keys [source staged? worktree?]
                :or {staged? false worktree? true}}]
   (platform-async
    (when-not (or staged? worktree?)
      (throw (ex-info "Restore needs --staged or --worktree" {})))
    (let [paths (mapv normalize-path paths)
          source (or source (if staged? :head :index))
          tree (await (tree conn source))
          tx-data
          (vec
           (mapcat
            (fn [path]
              (let [entry (get tree path)]
                (concat
                 (when worktree?
                   (concat (retract-paths-tx @conn :geschichte.work/path [path])
                           (when entry [(work-entry-tx path entry)])))
                 (when staged?
                   (concat (retract-paths-tx @conn :geschichte.stage/path [path])
                           (when entry [(stage-entry-tx path entry)]))))))
            paths))]
      (when (seq tx-data) (await (transact-data conn tx-data)))
      paths))))

(defn reset!
  "Move the current logical ref to `commit` with Git-shaped reset modes.

   `:soft` moves only the ref, `:mixed` also replaces the complete index, and
   `:hard` additionally replaces tracked worktree paths. Hard reset preserves
   untracked paths that do not collide with the target tree."
  ([conn commit] (reset! conn commit {}))
  ([conn commit {:keys [mode] :or {mode :mixed}}]
   (platform-async
    (when-not (#{:soft :mixed :hard} mode)
      (throw (ex-info "Unknown reset mode" {:mode mode})))
    (let [commit (cond
                   (map? commit) commit
                   (uuid? commit) (commit-by-id conn commit)
                   :else nil)
          _ (when-not (:geschichte.commit/id commit)
              (throw (ex-info "Unknown reset commit" {:commit commit})))
          tree (await (tree-at conn commit))
          current-ref-name (current-ref conn)
          current-tracked (set (keys (query/stage @conn)))
          target-paths (set (keys tree))
          tracked-paths (set/union current-tracked target-paths)
          ref-tx {:geschichte.ref/name current-ref-name
                  :geschichte.ref/target
                  [:geschichte.commit/id (:geschichte.commit/id commit)]}
          index-tx (when (#{:mixed :hard} mode)
                     (concat
                      (retract-entities-tx @conn :geschichte.stage/path)
                      (map (fn [[path entry]] (stage-entry-tx path entry)) tree)))
          work-tx (when (= :hard mode)
                    (concat
                     (retract-paths-tx @conn :geschichte.work/path tracked-paths)
                     (map (fn [[path entry]] (work-entry-tx path entry)) tree)))
          tx-data (vec (concat [ref-tx] index-tx work-tx))]
      (await (transact-data conn tx-data))
      (:geschichte.commit/id commit)))))

(defn materialize-bytes!
  "Replace worktree and index from a sequence of `[path entry]` pairs.

  JVM imports publish bounded batches so a large checkout does not retain every
  file payload and transaction map at once. Maps remain accepted."
  ([conn file-map] (materialize-bytes! conn file-map nil))
  ([conn file-map {:keys [force?]}]
   (platform-async
    (let [current-status (await (status conn))]
      (when (and (not force?) (not (:clean? current-status)))
        (throw (ex-info "Materialization would overwrite local changes"
                        {:status current-status}))))
    (await
     (transact-data
      conn
      (into (retract-entities-tx @conn :geschichte.work/path)
            (retract-entities-tx @conn :geschichte.stage/path))))
    #?(:clj
       (doseq [batch (partition-all 256 file-map)]
         (content/transact-content-batch!
          conn
          (mapv (fn [[path {:keys [bytes mode]}]]
                  (let [path (normalize-path path)
                        mode (or mode default-mode)]
                    {:value bytes
                     :base-id nil
                     :tx-fn
                     (fn [id]
                       [{:geschichte.work/path path
                         :geschichte.work/content
                         [:geschichte.content/id id]
                         :geschichte.work/size (long (bytes/length bytes))
                         :geschichte.work/mode (long mode)}])}))
                batch)))
       :cljs
       (doseq [[path {:keys [bytes mode]}] file-map]
         (await (write! conn path bytes {:mode (or mode default-mode)}))))
    (await (stage-all! conn))
    (d/commit-id @conn))))

(defn prepare-merge!
  "Install a clean merge plan and remember its second logical parent."
  [conn {:keys [ours theirs tree conflicts clean?] :as plan}]
  (platform-async
   (when-not clean?
     (throw (ex-info "Cannot apply a merge plan with unresolved conflicts"
                     {:conflicts conflicts})))
   (when-not (= ours (some-> (head-commit conn) :geschichte.commit/id))
     (throw (ex-info "Merge plan is stale: HEAD has moved"
                     {:planned ours
                      :head (some-> (head-commit conn)
                                    :geschichte.commit/id)})))
   (let [current-status (await (status conn))]
     (when-not (:clean? current-status)
       (throw (ex-info "Merge requires a clean worktree and index"
                       {:status current-status}))))
   (let [repo-id (:geschichte.repo/id (query/repository @conn))
         tx-data
         (into (into (retract-entities-tx @conn :geschichte.work/path)
                     (retract-entities-tx @conn :geschichte.stage/path))
               (concat (materialize-tx tree)
                       [{:geschichte.repo/id repo-id
                         :geschichte.repo/merge-parent
                         [:geschichte.commit/id theirs]}]))]
     (await (transact-data conn tx-data))
     (assoc plan :prepared? true))))

(defn checkout!
  "Switch logical branches and materialize the selected committed tree."
  ([conn name] (checkout! conn name {}))
  ([conn name {:keys [force?]}]
   (platform-async
    (let [name (if (str/starts-with? name "refs/")
                 name
                 (str "refs/heads/" name))
          ref (or (ref-entity @conn name)
                  (throw (ex-info "Unknown ref" {:ref name})))
          current-status (await (status conn))
          commit (:geschichte.ref/target ref)
          tree (if commit
                 (stage-map (await (snapshot-db conn commit)))
                 {})
          conflicting-untracked
          (set/intersection (set (:untracked current-status))
                            (set (keys tree)))]
      (when (and (not force?)
                 (or (seq (:staged current-status))
                     (seq (:unstaged current-status))
                     (seq conflicting-untracked)))
        (throw (ex-info "Checkout would overwrite local changes"
                        {:status current-status
                         :conflicting-untracked conflicting-untracked})))
      (let [tracked-paths (set/union (set (keys (query/stage @conn)))
                                     (set (keys tree)))
            tx-data
            (into (into (retract-paths-tx @conn :geschichte.work/path
                                          tracked-paths)
                        (retract-entities-tx @conn :geschichte.stage/path))
                  (concat
                   (materialize-tx tree)
                   [{:geschichte.repo/id
                     (:geschichte.repo/id (query/repository @conn))
                     :geschichte.repo/head name}]))]
        (await (transact-data conn tx-data))
        name)))))

(defn delete-branch!
  "Delete a logical branch. Without `:force?`, its target must be the current
   HEAD (the conservative subset of Git's fully-merged check)."
  ([conn name] (delete-branch! conn name {}))
  ([conn name {:keys [force?]}]
   (platform-async
    (let [name (if (str/starts-with? name "refs/")
                 name
                 (str "refs/heads/" name))
          ref (or (ref-entity @conn name)
                  (throw (ex-info "Unknown ref" {:ref name})))
          current (current-ref conn)
          target-id (get-in ref [:geschichte.ref/target
                                 :geschichte.commit/id])
          head-id (some-> (head-commit conn) :geschichte.commit/id)]
      (when (= name current)
        (throw (ex-info "Cannot delete the checked out branch" {:ref name})))
      (when (and (not force?) target-id (not= target-id head-id))
        (throw (ex-info "Branch is not fully merged" {:ref name})))
      (await (transact-data conn [[:db/retractEntity (:db/id ref)]]))
      name))))

(defn log
  "Return commits reachable from `:start` (or current HEAD), newest first."
  ([conn] (log conn {}))
  ([conn {:keys [limit start starts exclude] :or {limit 100 exclude #{}}}]
   (let [starts (or (seq starts)
                    (when-let [start (or start (head-commit conn))] [start]))]
     (loop [queue (vec starts)
            seen #{}
            result []]
       (if (or (empty? queue) (>= (count result) limit))
         result
         (let [commit (first queue)
               id (:geschichte.commit/id commit)]
           (if (or (seen id) (contains? exclude id))
             (recur (subvec queue 1) seen result)
             (let [full (d/pull
                         @conn
                         [:geschichte.commit/id :geschichte.commit/snapshot
                          :geschichte.commit/message :geschichte.commit/author
                          :geschichte.commit/time
                          {:geschichte.commit/parents
                           [:geschichte.commit/id
                            :geschichte.commit/snapshot]}]
                         [:geschichte.commit/id id])
                   parents (vec (:geschichte.commit/parents full))]
               (recur (into (subvec queue 1) parents)
                      (conj seen id)
                      (conj result full))))))))))

(defn fork-workspace!
  "Create a Datahike workspace branch, distinct from a logical ref."
  [conn branch-keyword]
  (platform-async
   (await
    (execution/io-result
     (d/branch! conn (d/commit-id @conn) branch-keyword)
     execution/default-opts))
   branch-keyword))
