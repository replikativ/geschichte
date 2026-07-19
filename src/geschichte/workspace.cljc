(ns geschichte.workspace
  "Structurally shared, isolated Geschichte workspaces over Datahike branches.

  A Datahike branch owns one mutable worktree/index/ref catalog. Logical Git
  refs remain data inside that branch, so many workspaces may independently
  check out `refs/heads/main`. Publication copies only immutable Geschichte
  commit metadata and atomically advances one canonical ref; the workspace's
  worktree, index, configuration, and other refs stay private."
  (:refer-clojure :exclude [await list remove])
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [datahike.api :as d]
            [geschichte.async :as execution]
            [geschichte.merge.core :as graph]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [is.simm.partial-cps.async :refer [await]])
  #?(:cljs (:require-macros [geschichte.macros :refer [platform-async]]))
  #?(:clj (:require [geschichte.macros :refer [platform-async]])))

(def canonical-branch :db)

(defn branch-key
  "Return a Datahike branch keyword for a workspace name. Existing keywords
  remain unchanged; strings use the `geschichte.workspace` namespace."
  [name]
  (if (keyword? name)
    name
    (keyword "geschichte.workspace" (str name))))

(defn- normalize-ref [ref]
  (let [ref (or ref (throw (ex-info "A logical ref is required" {})))]
    (if (str/starts-with? ref "refs/") ref (str "refs/heads/" ref))))

(defn- io-result [value]
  (execution/io-result value execution/default-opts))

(defn fork!
  "Fork `conn`'s current immutable Datahike snapshot into an isolated physical
  workspace branch. Forking is O(1) in logical repository size because the
  persistent indices and store-ref payloads remain structurally shared."
  [conn name]
  (repo/fork-workspace! conn (branch-key name)))

(defn prepare-checkout!
  "Prepare a newly forked workspace for Git-shaped `worktree add` options.
  `target-commit` must already be resolved in the source workspace. Detached
  checkouts are deliberately rejected; independent named-ref checkouts are the
  Geschichte workspace model."
  [conn target-commit {:keys [target new-branch reset-branch? detach?]}]
  (platform-async
   (when detach?
     (throw (ex-info
             "detached Geschichte workspaces are not implemented; use a named branch"
             {:requires :detached-head})))
   (if new-branch
     (let [ref (str "refs/heads/" new-branch)
           existing (get (repo/refs conn) ref)]
       (cond
         (and existing (not reset-branch?))
         (throw (ex-info (str "a branch named '" new-branch "' already exists")
                         {:branch new-branch}))

         (= ref (repo/current-ref conn))
         (await (repo/reset! conn target-commit {:mode :hard}))

         existing
         (do (await (repo/set-ref! conn ref target-commit))
             (await (repo/checkout! conn ref {:force? true})))

         :else
         (do (await (repo/create-ref! conn ref target-commit))
             (await (repo/checkout! conn ref {:force? true})))))
     (when target
       (let [ref (if (str/starts-with? target "refs/")
                   target (str "refs/heads/" target))]
         (when-not (contains? (repo/refs conn) ref)
           (throw (ex-info
                   "a commit may only be checked out in a named Geschichte workspace branch"
                   {:target target :requires :detached-head})))
         (await (repo/checkout! conn ref {:force? true})))))))

(defn list
  "List physical workspace branch keywords. The canonical `:db` branch is not
  included unless a different `:canonical` branch is supplied."
  ([conn] (list conn {}))
  ([conn {:keys [canonical] :or {canonical canonical-branch}}]
   (platform-async
    (disj (set (await (io-result (d/branches conn)))) canonical))))

(defn remove!
  "Delete a physical workspace branch. Published snapshots remain reachable
  through canonical Datahike merge ancestry; unpublished unique data becomes
  eligible for Datahike garbage collection."
  [conn name]
  (platform-async
   (let [branch (branch-key name)]
     (when (= canonical-branch branch)
       (throw (ex-info "Cannot remove the canonical workspace"
                       {:branch branch})))
     (await (io-result (d/delete-branch! conn branch)))
     branch)))

(defn- commit-index [db]
  (into {}
        (map (juxt :geschichte.commit/id identity))
        (query/commits db)))

(defn- parents-of [commits id]
  (mapv :geschichte.commit/id
        (:geschichte.commit/parents (get commits id))))

(defn- reachable-ids [commits tip]
  (set (keys (graph/ancestor-distances #(parents-of commits %) tip))))

(defn- commit-tx [commit]
  (let [parents (mapv (fn [parent]
                        [:geschichte.commit/id
                         (:geschichte.commit/id parent)])
                      (:geschichte.commit/parents commit))]
    (cond-> (select-keys commit
                         [:geschichte.commit/id
                          :geschichte.commit/snapshot
                          :geschichte.commit/message
                          :geschichte.commit/author
                          :geschichte.commit/time
                          :geschichte.commit/git-oid])
      (seq parents) (assoc :geschichte.commit/parents parents))))

(def ^:private content-pattern
  [:geschichte.content/id
   :geschichte.content/kind
   :geschichte.content/payload
   :geschichte.content/depth
   :geschichte.content/size
   :geschichte.content/chunking-algorithm
   :geschichte.content/chunking-version
   :geschichte.content/chunk-min-size
   :geschichte.content/chunk-size
   :geschichte.content/chunk-max-size
   {:geschichte.content/base [:geschichte.content/id]}
   {:geschichte.content/chunks
    [:geschichte.chunk/index :geschichte.chunk/offset
     :geschichte.chunk/payload :geschichte.chunk/size]}])

(defn- content-entity [db id]
  (d/pull db content-pattern [:geschichte.content/id id]))

(defn- content-closure [db ids]
  (loop [pending (vec ids), seen #{}, result []]
    (if-let [id (peek pending)]
      (if (contains? seen id)
        (recur (pop pending) seen result)
        (let [entity (content-entity db id)
              base-id (get-in entity [:geschichte.content/base
                                      :geschichte.content/id])]
          (when-not (:geschichte.content/id entity)
            (throw (ex-info "Published tree refers to unknown content"
                            {:content id})))
          (recur (cond-> (pop pending) base-id (conj base-id))
                 (conj seen id) (conj result entity))))
      result)))

(defn- content-tx [entity]
  (let [base-id (get-in entity [:geschichte.content/base
                                :geschichte.content/id])
        chunks (mapv #(select-keys % [:geschichte.chunk/index
                                      :geschichte.chunk/offset
                                      :geschichte.chunk/payload
                                      :geschichte.chunk/size])
                     (:geschichte.content/chunks entity))]
    (cond-> (select-keys entity
                         [:geschichte.content/id
                          :geschichte.content/kind
                          :geschichte.content/payload
                          :geschichte.content/depth
                          :geschichte.content/size
                          :geschichte.content/chunking-algorithm
                          :geschichte.content/chunking-version
                          :geschichte.content/chunk-min-size
                          :geschichte.content/chunk-size
                          :geschichte.content/chunk-max-size])
      base-id (assoc :geschichte.content/base
                     [:geschichte.content/id base-id])
      (seq chunks) (assoc :geschichte.content/chunks chunks))))

(defn- transfer-data [source target commits tip tree]
  (let [reachable (reachable-ids commits tip)
        target-commits (set (map :geschichte.commit/id
                                 (query/commits target)))
        copied-commit-ids (->> reachable
                               (clojure.core/remove target-commits) set)
        copied-commits (->> copied-commit-ids (map commits)
                            (sort-by (comp str :geschichte.commit/id)) vec)
        target-content (set (d/q '[:find [?id ...]
                                   :where [_ :geschichte.content/id ?id]]
                                 target))
        tree-content (keep (comp :content val) tree)
        copied-content (->> (content-closure source tree-content)
                            (clojure.core/remove
                             #(contains? target-content
                                         (:geschichte.content/id %)))
                            vec)]
    {:reachable reachable
     :copied-commits copied-commits
     :copied-content copied-content
     :tx-data
     (vec
      (concat
       ;; Establish identities before any base/parent lookup refs resolve.
       (map (fn [entity]
              {:geschichte.content/id (:geschichte.content/id entity)})
            copied-content)
       (map (fn [commit]
              {:geschichte.commit/id (:geschichte.commit/id commit)})
            copied-commits)
       (map content-tx copied-content)
       (map commit-tx copied-commits)))}))

(defn- merge-data [conn parents tx-data]
  (io-result #?(:clj (d/merge-db conn parents tx-data)
                :cljs (d/merge-db! conn parents tx-data))))

(defn publish!
  "Publish one logical workspace ref into a canonical Geschichte connection.

  The update is a fast-forward by default. `:db.fn/cas` checks the canonical
  ref again inside the serialized Datahike merge transaction, so concurrent
  publishers cannot silently overwrite each other. `:force? true` permits a
  deliberate non-fast-forward update.

  Only reachable immutable commit metadata and the selected ref are copied to
  the canonical current DB. The workspace's worktree, index, config, and other
  refs are excluded. Its current Datahike commit becomes merge ancestry, which
  keeps every published Geschichte snapshot and store-ref blob reachable after
  the physical workspace branch is removed."
  ([canonical workspace] (publish! canonical workspace {}))
  ([canonical workspace {:keys [ref commit force? create?]
                         :or {force? false create? false}}]
   (platform-async
    (let [ref (normalize-ref (or ref (repo/current-ref workspace)))
          canonical-ref (get (repo/refs canonical) ref ::missing)
          _ (when (and (= ::missing canonical-ref) (not create?))
              (throw (ex-info "Canonical ref does not exist"
                              {:ref ref})))
          canonical-ref (when-not (= ::missing canonical-ref) canonical-ref)
          commits (commit-index @workspace)
          tip (or commit (get (repo/refs workspace) ref))
          tip (if (map? tip) (:geschichte.commit/id tip) tip)
          _ (when-not (contains? commits tip)
              (throw (ex-info "Workspace ref has no publishable commit"
                              {:ref ref :commit tip})))
          reachable (reachable-ids commits tip)
          _ (when (and canonical-ref (not force?)
                       (not (contains? reachable canonical-ref)))
              (throw (ex-info "Publication is not a fast-forward"
                              {:ref ref :expected canonical-ref :new tip})))
          tree (await (repo/tree-at workspace tip))
          transfer (transfer-data @workspace @canonical commits tip tree)
          ref-entity (d/q '[:find ?ref .
                            :in $ ?name
                            :where [?ref :geschichte.ref/name ?name]]
                          @canonical ref)
          ref-update (cond
                       (nil? ref-entity)
                       {:geschichte.ref/name ref
                        :geschichte.ref/target
                        [:geschichte.commit/id tip]}

                       force?
                       {:db/id ref-entity
                        :geschichte.ref/target
                        [:geschichte.commit/id tip]}

                       :else
                       [:db.fn/cas ref-entity :geschichte.ref/target
                        (when canonical-ref
                          [:geschichte.commit/id canonical-ref])
                        [:geschichte.commit/id tip]])
          workspace-head (d/commit-id @workspace)
          tx-data (conj (:tx-data transfer) ref-update)]
      (await (merge-data canonical #{workspace-head} tx-data))
      {:ref ref
       :old canonical-ref
       :new tip
       :copied-commits (count (:copied-commits transfer))
       :copied-content (count (:copied-content transfer))
       :workspace-commit workspace-head
       :forced? force?}))))

(defn- retract-entities [db attr]
  (mapv (fn [[eid]] [:db/retractEntity eid])
        (d/q '[:find ?e :in $ ?attr :where [?e ?attr]] db attr)))

(defn- retract-paths [db attr paths]
  (mapv (fn [[eid]] [:db/retractEntity eid])
        (d/q '[:find ?e
               :in $ ?attr [?path ...]
               :where [?e ?attr ?path]]
             db attr (vec paths))))

(defn- materialize-tx [tree]
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
   tree))

(defn advance!
  "Fast-forward a clean workspace ref to the corresponding canonical ref.

  This is the manual observation step for a frozen Yggdrasil overlay. Immutable
  commit/content facts and the committed tree are imported atomically; private
  workspace config and unrelated refs remain untouched."
  ([canonical workspace] (advance! canonical workspace {}))
  ([canonical workspace {:keys [ref]}]
   (platform-async
    (let [ref (normalize-ref (or ref (repo/current-ref workspace)))
          status (await (repo/status workspace))
          _ (when (or (seq (:staged status)) (seq (:unstaged status)))
              (throw (ex-info "Cannot advance a dirty workspace"
                              {:status status :ref ref})))
          source-commits (commit-index @canonical)
          tip (get (repo/refs canonical) ref)
          old (get (repo/refs workspace) ref ::missing)
          _ (when (or (= ::missing old) (nil? tip))
              (throw (ex-info "Ref is unavailable for workspace advance"
                              {:ref ref :workspace old :canonical tip})))
          reachable (reachable-ids source-commits tip)
          _ (when (and old (not (contains? reachable old)))
              (throw (ex-info "Workspace advance is not a fast-forward"
                              {:ref ref :old old :new tip})))
          tree (await (repo/tree-at canonical tip))
          conflicting-untracked (set/intersection
                                 (set (:untracked status))
                                 (set (keys tree)))
          _ (when (seq conflicting-untracked)
              (throw (ex-info "Workspace advance would overwrite untracked files"
                              {:ref ref :paths conflicting-untracked})))
          transfer (transfer-data @canonical @workspace source-commits tip tree)
          ref-eid (d/q '[:find ?ref .
                         :in $ ?name
                         :where [?ref :geschichte.ref/name ?name]]
                       @workspace ref)
          ref-update [:db.fn/cas ref-eid :geschichte.ref/target
                      (when old [:geschichte.commit/id old])
                      [:geschichte.commit/id tip]]
          tracked-paths (set/union (set (keys (query/stage @workspace)))
                                   (set (keys tree)))
          tx-data (vec
                   (concat (:tx-data transfer)
                           (retract-paths @workspace :geschichte.work/path
                                          tracked-paths)
                           (retract-entities @workspace :geschichte.stage/path)
                           (materialize-tx tree)
                           [ref-update]))
          canonical-head (d/commit-id @canonical)]
      (await (merge-data workspace #{canonical-head} tx-data))
      {:ref ref :old old :new tip
       :copied-commits (count (:copied-commits transfer))
       :copied-content (count (:copied-content transfer))
       :canonical-commit canonical-head}))))
