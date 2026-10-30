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
            [datahike.index.audit :as audit]
            [datahike.index.interface :as index]
            [datahike.store :as store]
            [datahike.versioning :as versioning]
            [geschichte.async :as execution]
            [geschichte.merge.core :as graph]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [geschichte.tree :as path-tree]
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

(defn checkpoint-origin?
  "Closed portable identity of an exact repository database state. An origin
   conveys neither authorization nor GC retention. Logical commits use refs."
  [value]
  (and (map? value) (= #{:version :kind :commit :source} (set (keys value)))
       (= 1 (:version value)) (= :datahike (:kind value)) (uuid? (:commit value))
       (map? (:source value)) (= #{:store :repository} (set (keys (:source value))))
       (every? uuid? ((juxt :store :repository) (:source value)))))

(defn- checkpoint-opts! [opts]
  (when-not (and (or (nil? opts) (map? opts))
                 (every? #{:sync?} (keys opts))
                 (or (not (contains? opts :sync?))
                     (= (:sync? execution/default-opts) (:sync? opts))))
    (throw (ex-info "Unsupported checkpoint execution mode" {:type ::invalid-checkpoint-mode}))))

(defn- checkpoint-roots [db]
  ;; Compare a constant number of persisted addresses. In identity-preserving
  ;; memory stores a shared immutable root object is sufficient. No datom scan.
  (into {}
        (for [key (cond-> [:eavt :aevt :avet]
                    (get-in db [:config :keep-history?])
                    (into [:temporal-eavt :temporal-aevt :temporal-avet]))
              :let [tree (get db key)
                    supported? (and (satisfies? audit/IAuditable tree) (satisfies? index/IIndex tree))
                    address (when supported? (audit/-merkle-root tree))
                    root (when (and supported? (nil? address)) (index/-root-node tree))]]
          (do (when-not (or address root)
                (throw (ex-info "Checkpoint index has no verifiable root"
                                {:type ::checkpoint-index-unavailable})))
              [key {:address address :root root}]))))

(defn- same-checkpoint-roots? [before after]
  (and (= (set (keys before)) (set (keys after)))
       (every? (fn [[key {:keys [address root]}]]
                 (let [other (get after key)]
                   (if address (= address (:address other))
                       (and (nil? (:address other)) (identical? root (:root other)))))) before)))

(defn- checkpoint-source! [conn origin db]
  (when-not (and (checkpoint-origin? origin) db
                 (= (:commit origin) (d/commit-id db))
                 (= (get-in origin [:source :store])
                    (store/store-identity (get-in @conn [:config :store]))
                    (store/store-identity (get-in db [:config :store])))
                 (= (get-in origin [:source :repository])
                    (:geschichte.repo/id (query/repository @conn))
                    (:geschichte.repo/id (query/repository db))))
    (throw (ex-info "Geschichte checkpoint source is unavailable" {:type ::checkpoint-unavailable})))
  ;; Datahike drops unfinished secondary generations when branching. Such a
  ;; normalization cannot satisfy the exact-workspace contract.
  (when (some #(= :building (:db.secondary/status %)) (vals (:schema db)))
    (throw (ex-info "Checkpoint has a building secondary index" {:type ::checkpoint-building-index})))
  (checkpoint-roots db)
  db)

(defn resolve-checkpoint
  "Read and validate an exact origin without allocating a branch, connecting
   a writer or retaining evidence. Returns the native immutable DB, not a
   portable proof or capability. Caller owns source lifetime and retention.
   JVM sync / CLJS CPS; an explicit unsupported mode refuses before source IO."
  ([conn origin] (resolve-checkpoint conn origin nil))
  ([conn origin opts]
   (platform-async
    (checkpoint-opts! opts)
    (when-not (checkpoint-origin? origin)
      (throw (ex-info "Invalid Geschichte checkpoint origin" {:type ::invalid-checkpoint})))
    (checkpoint-source! conn origin
                        (await (execution/io-result
                                (versioning/commit-as-db conn (:commit origin) execution/default-opts)
                                execution/default-opts))))))

(defn fork-at-checkpoint!
  "Allocate ONLY a named physical workspace from an already resolved native
   DB. Source/origin correspondence is rechecked; a plain map is not a proof.
   Return immediately after allocation ACK, before connect/child verification.
   Caller must own construction cleanup before invoking: a thrown allocation
   can have created the branch. Never infer absence or blindly replay it.
   Resolve before acquiring allocation ownership, then connect the acknowledged
   branch and call verify-checkpoint-workspace!. No parent checkout/reset."
  ([conn name origin db] (fork-at-checkpoint! conn name origin db nil))
  ([conn name origin db opts]
   (platform-async
    (checkpoint-opts! opts)
    (checkpoint-source! conn origin db)
    (let [branch (branch-key name)]
      (when (= canonical-branch branch)
        (throw (ex-info "Cannot allocate over the canonical workspace" {:type ::invalid-checkpoint-destination})))
      (await (execution/io-result
              (versioning/branch! conn (d/commit-id db) branch execution/default-opts)
              execution/default-opts))
      branch))))

(defn verify-checkpoint-workspace!
  "Pure connected-child verification against the resolved immutable DB.
   A new physical branch identity is intentional; repository data, source
   configuration and index generations must remain exact. No files are read."
  [db child branch]
  (let [state #(select-keys % [:hash :max-tx :max-eid :op-count :schema :secondary-index-keys])]
    (when-not (and (= (d/commit-id db) (d/commit-id child))
                   (= (state db) (state child))
                   (same-checkpoint-roots? (checkpoint-roots db) (checkpoint-roots child))
                   (= (dissoc (:config db) :branch) (dissoc (:config child) :branch))
                   (= (branch-key branch) (get-in child [:config :branch])))
      (throw (ex-info "Checkpoint child changed during construction" {:type ::checkpoint-child-changed}))))
  child)

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

(defn- create-publication-ref-tx [db ref tip]
  (when (d/q '[:find ?e . :in $ ?name :where [?e :geschichte.ref/name ?name]] db ref)
    (throw (ex-info "Publication ref was created while preparing publication"
                    {:type ::publication-ref-exists :ref ref})))
  [{:geschichte.ref/name ref :geschichte.ref/target [:geschichte.commit/id tip]}])

(defn- update-publication-ref-tx [db ref eid old tip]
  (when-not (= eid (d/q '[:find ?e . :in $ ?name :where [?e :geschichte.ref/name ?name]] db ref))
    (throw (ex-info "Publication ref identity changed while preparing publication"
                    {:type ::stale-publication-ref :ref ref})))
  [[:db.fn/cas eid :geschichte.ref/target
    (when old [:geschichte.commit/id old]) [:geschichte.commit/id tip]]])

(defn- guarded-publication-tx [db expected tx-data]
  ;; O(1): never enumerate a raced, potentially larger target in the writer.
  (when-not (= expected (d/commit-id db))
    (throw (ex-info "Canonical checkpoint changed while preparing publication"
                    {:type ::stale-publication :expected-checkpoint expected
                     :checkpoint (d/commit-id db)})))
  tx-data)

(defn publication-profile
  "Positive compatibility marker: consumers must not send create-only options
  to an older runtime that could ignore them. This grants no publication rights."
  []
  {:id ::guarded-publication :version 1 :create-only? true :expected-checkpoint? true})

(defn publish!
  "Publish one logical workspace ref into a canonical Geschichte connection.

  The update is a fast-forward by default. `:db.fn/cas` checks the canonical
  ref again inside the serialized Datahike merge transaction, and newly created
  refs validate absence there, so concurrent publishers cannot silently
  overwrite each other. `:force? true` permits a
  deliberate non-fast-forward update. `:create-only? true` requires a missing
  ref and validates that absence again in the serialized transaction; it also
  allows creation without `:create?`. Existing refs, including unborn refs or
  the same tip, refuse. It cannot be combined with `:force?`.

  Optional `:expected-checkpoint` must name the canonical Datahike commit. It
  is checked before metadata projection and again in the serialized writer,
  allowing callers to preflight finite bounds on the exact target snapshot.
  The caller must separately own/exclude concurrent writes to the workspace.

  Only reachable immutable commit metadata and the selected ref are copied to
  the canonical current DB. The workspace's worktree, index, config, and other
  refs are excluded. Its current Datahike commit becomes merge ancestry, which
  keeps every published Geschichte snapshot and store-ref blob reachable after
  the physical workspace branch is removed."
  ([canonical workspace] (publish! canonical workspace {}))
  ([canonical workspace {:keys [ref commit force? create? create-only? expected-checkpoint]
                         :or {force? false create? false create-only? false} :as options}]
   (platform-async
    (let [_ (when (and (contains? options :expected-checkpoint) (not (uuid? expected-checkpoint)))
              (throw (ex-info "Expected publication checkpoint must be a UUID"
                              {:type ::invalid-publication-options})))
          _ (when (and force? create-only?)
              (throw (ex-info "Create-only publication cannot force an existing ref"
                              {:type ::invalid-publication-options})))
          canonical-db @canonical
          _ (when expected-checkpoint (guarded-publication-tx canonical-db expected-checkpoint []))
          ref (normalize-ref (or ref (repo/current-ref workspace)))
          canonical-ref (get (query/refs canonical-db) ref ::missing)
          _ (when (and create-only? (not= ::missing canonical-ref))
              (throw (ex-info "Create-only publication requires an absent ref"
                              {:type ::publication-ref-exists :ref ref})))
          _ (when (and (= ::missing canonical-ref) (not (or create? create-only?)))
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
          transfer (transfer-data @workspace canonical-db commits tip tree)
          ref-entity (d/q '[:find ?ref .
                            :in $ ?name
                            :where [?ref :geschichte.ref/name ?name]]
                          canonical-db ref)
          ref-update (cond
                       force?
                       {:geschichte.ref/name ref
                        :geschichte.ref/target
                        [:geschichte.commit/id tip]}

                       (nil? ref-entity)
                       [:db.fn/call create-publication-ref-tx ref tip]

                       :else
                       [:db.fn/call update-publication-ref-tx ref ref-entity canonical-ref tip])
          workspace-head (d/commit-id @workspace)
          tx-data (conj (:tx-data transfer) ref-update)
          tx-data (if expected-checkpoint
                    [[:db.fn/call guarded-publication-tx expected-checkpoint tx-data]]
                    tx-data)]
      (path-tree/assert-valid! tree)
      (await (merge-data canonical #{workspace-head} tx-data))
      (cond-> {:ref ref
               :old canonical-ref
               :new tip
               :copied-commits (count (:copied-commits transfer))
               :copied-content (count (:copied-content transfer))
               :workspace-commit workspace-head
               :forced? force?}
        create-only? (assoc :created? true)
        expected-checkpoint (assoc :expected-checkpoint expected-checkpoint))))))

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

(defn- advance-basis [db]
  {:store (select-keys (get-in db [:config :store]) [:backend :id])
   :branch (get-in db [:config :branch])
   :repository (query/repository db)
   :checkpoint (d/commit-id db)
   :max-tx (:max-tx db)
   :ref (query/current-ref db)
   :refs (query/refs db)
   :index (query/stage db)
   :worktree (query/worktree db)})

(defn- guarded-advance-tx [db basis tx-data]
  ;; Refuse a raced snapshot before enumerating its index/worktree. The old
  ;; complete-basis comparison already required this same checkpoint equality.
  (when-not (= (:checkpoint basis) (d/commit-id db))
    (throw (ex-info "Workspace changed while preparing advance"
                    {:type ::stale-advance :ref (:ref basis)
                     :expected-checkpoint (:checkpoint basis)
                     :actual-checkpoint (d/commit-id db)})))
  (when-not (= basis (advance-basis db))
    (throw (ex-info "Workspace changed while preparing advance"
                    {:type ::stale-advance :ref (:ref basis)})))
  tx-data)

(defn advance-profile
  "Positive marker for finite preflight callers: a raced destination refuses
   before full basis projection in the serialized writer as well as on entry."
  []
  {:id ::guarded-advance :version 1 :checkpoint-before-projection? true})

(defn advance!
  "Fast-forward a clean workspace ref to the corresponding canonical ref.

  This is the manual observation step for a frozen Yggdrasil overlay. Immutable
  commit/content facts and the committed tree are imported atomically; private
  workspace config and unrelated refs remain untouched. Only the checked-out
  ref can advance, and pending merges are refused. Preparation uses immutable
  source/target snapshots; any intervening target transaction refuses the
  publication without overwriting concurrent edits. Optional :expected-checkpoint
  binds the caller's previously inspected target checkpoint to that same captured
  target snapshot. A stale token refuses before preparation; it never refreshes
  itself implicitly."
  ([canonical workspace] (advance! canonical workspace {}))
  ([canonical workspace {:keys [ref expected-checkpoint] :as opts}]
   (platform-async
    (let [_ (when (or (seq (set/difference (set (keys opts)) #{:ref :expected-checkpoint}))
                      (and (contains? opts :expected-checkpoint)
                           (not (uuid? expected-checkpoint))))
              (throw (ex-info "Invalid workspace advance options"
                              {:type ::invalid-advance-options})))
          source-db @canonical
          target-db @workspace
          _ (when (and expected-checkpoint
                       (not= expected-checkpoint (d/commit-id target-db)))
              (throw (ex-info "Workspace changed since the caller inspected it"
                              {:type ::stale-advance
                               :expected-checkpoint expected-checkpoint
                               :actual-checkpoint (d/commit-id target-db)})))
          source (atom source-db)
          target (atom target-db)
          basis (advance-basis target-db)
          ref (normalize-ref (or ref (:ref basis)))
          _ (when-not (= ref (:ref basis))
              (throw (ex-info "Workspace advance requires the checked-out ref"
                              {:type ::noncurrent-ref :ref ref :current (:ref basis)})))
          _ (when (:geschichte.repo/merge-parent (:repository basis))
              (throw (ex-info "Cannot advance a workspace with a pending merge"
                              {:type ::pending-merge :ref ref})))
          status (await (repo/status target))
          _ (when (or (seq (:staged status)) (seq (:unstaged status)))
              (throw (ex-info "Cannot advance a dirty workspace"
                              {:status status :ref ref})))
          source-commits (commit-index source-db)
          tip (get (repo/refs source) ref)
          old (get (repo/refs target) ref ::missing)
          _ (when (or (= ::missing old) (nil? tip))
              (throw (ex-info "Ref is unavailable for workspace advance"
                              {:ref ref :workspace old :canonical tip})))
          reachable (reachable-ids source-commits tip)
          _ (when (and old (not (contains? reachable old)))
              (throw (ex-info "Workspace advance is not a fast-forward"
                              {:ref ref :old old :new tip})))
          tree (await (repo/tree-at source tip))
          conflicting-untracked (set/intersection
                                 (set (:untracked status))
                                 (set (keys tree)))
          _ (when (seq conflicting-untracked)
              (throw (ex-info "Workspace advance would overwrite untracked files"
                              {:ref ref :paths conflicting-untracked})))
          transfer (transfer-data source-db target-db source-commits tip tree)
          ref-eid (d/q '[:find ?ref .
                         :in $ ?name
                         :where [?ref :geschichte.ref/name ?name]]
                       target-db ref)
          ref-update [:db.fn/cas ref-eid :geschichte.ref/target
                      (when old [:geschichte.commit/id old])
                      [:geschichte.commit/id tip]]
          tracked-paths (set/union (set (keys (query/stage target-db)))
                                   (set (keys tree)))
          _ (repo/validate-worktree! target-db
                                     (merge (apply dissoc (query/worktree target-db) tracked-paths) tree))
          tx-data (vec
                   (concat (:tx-data transfer)
                           (retract-paths target-db :geschichte.work/path
                                          tracked-paths)
                           (retract-entities target-db :geschichte.stage/path)
                           (materialize-tx tree)
                           [ref-update]))
          canonical-head (d/commit-id source-db)]
      (await (merge-data workspace #{canonical-head}
                         [[:db.fn/call guarded-advance-tx basis tx-data]]))
      {:ref ref :old old :new tip
       :copied-commits (count (:copied-commits transfer))
       :copied-content (count (:copied-content transfer))
       :canonical-commit canonical-head}))))
