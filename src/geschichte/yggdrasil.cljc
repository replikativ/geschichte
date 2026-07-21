(ns geschichte.yggdrasil
  "Optional Yggdrasil adapter for Geschichte's two-level version model.

  Yggdrasil `Branchable` maps to logical Geschichte refs. `Overlayable` maps
  to structurally shared Datahike workspace branches, keeping those physical
  branches out of the user-visible Git branch namespace."
  (:refer-clojure :exclude [await ancestors])
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [geschichte.async :as execution]
            [geschichte.merge.core :as graph]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [geschichte.workspace :as workspace]
            [is.simm.partial-cps.async :refer [await]]
            [yggdrasil.protocols :as p]
            [yggdrasil.types :as t])
  #?(:cljs (:require-macros [geschichte.macros :refer [platform-async]]))
  #?(:clj (:require [geschichte.macros :refer [platform-async]])))

(defn- branch-ref [name]
  (let [name (clojure.core/name name)]
    (if (str/starts-with? name "refs/") name (str "refs/heads/" name))))

(defn- branch-name [ref]
  (keyword (str/replace ref #"^refs/heads/" "")))

(defn- head [conn]
  (repo/head-commit conn))

(defn- commit-index [conn]
  (into {} (map (juxt :geschichte.commit/id identity)) (query/commits @conn)))

(defn- parents-of [commits id]
  (mapv :geschichte.commit/id (:geschichte.commit/parents (get commits id))))

(defn- resolve-commit [conn value]
  (cond
    (nil? value) (head conn)
    (map? value) value
    (keyword? value) (some->> (get (repo/refs conn) (branch-ref value))
                              (repo/commit-by-id conn))
    (uuid? value) (repo/commit-by-id conn value)
    :else (some #(when (= (str (:geschichte.commit/id %)) (str value)) %)
                (query/commits @conn))))

(declare ->GeschichteSystem)

(defrecord GeschichteOverlay
           [parent local-writes workspace-branch base-snapshot mode]
  p/Overlayable
  (base-ref [_] base-snapshot)
  (peek-parent [_] parent)
  (peek-parent [_ _] parent)
  (overlay-writes [_]
    {:base base-snapshot
     :worktree (query/worktree @(:conn @local-writes))
     :stage (query/stage @(:conn @local-writes))})
  (advance! [this] (p/advance! this nil))
  (advance! [this _opts]
    (platform-async
     (await (workspace/advance! (:conn parent) (:conn @local-writes)))
     this))
  (merge-down! [this] (p/merge-down! this nil))
  (merge-down! [_ opts]
    (platform-async
     (let [parent-status (await (repo/status (:conn parent)))]
       (when-not (:clean? parent-status)
         (throw (ex-info "Cannot merge a Geschichte workspace into a dirty parent"
                         {:status parent-status})))
       (let [{:keys [new]}
             (await (workspace/publish! (:conn parent) (:conn @local-writes)
                                        (select-keys opts [:ref :commit :force?])))]
         ;; Publication intentionally transfers only immutable history + the
         ;; canonical ref. A live parent workspace also needs its index/worktree
         ;; advanced so virtual filesystem readers observe the merge immediately.
         (await (repo/reset! (:conn parent) new {:mode :hard}))))
     ;; Spindel/Yggdrasil deliberately calls discard! after merge-down! to
     ;; dispose the overlay. Keep cleanup in that single lifecycle phase; doing
     ;; it here made merge-to-parent! attempt to delete the branch twice.
     parent))
  (discard! [this] (p/discard! this nil))
  (discard! [_ _opts]
    (platform-async
     (d/release (:conn @local-writes))
     (await (workspace/remove! (:conn parent) workspace-branch))
     parent)))

(defrecord GeschichteSystem [conn system-name]
  p/SystemIdentity
  (system-id [_]
    (or system-name
        (str "geschichte:" (get-in @conn [:config :store :id]))))
  (system-type [_] :geschichte)
  (capabilities [_]
    (t/->Capabilities true true true false true false true false true))

  p/Snapshotable
  (snapshot-id [_] (some-> (head conn) :geschichte.commit/id str))
  (parent-ids [_]
    (set (map (comp str :geschichte.commit/id)
              (:geschichte.commit/parents (head conn)))))
  (as-of [this snap-id] (p/as-of this snap-id nil))
  (as-of [_ snap-id _opts]
    (platform-async
     (let [commit (or (resolve-commit conn snap-id)
                      (throw (ex-info "Unknown Geschichte snapshot"
                                      {:snapshot snap-id})))]
       (await (repo/tree-at conn commit)))))
  (snapshot-meta [this snap-id] (p/snapshot-meta this snap-id nil))
  (snapshot-meta [_ snap-id _opts]
    (some-> (resolve-commit conn snap-id)
            (select-keys [:geschichte.commit/id :geschichte.commit/message
                          :geschichte.commit/author :geschichte.commit/time
                          :geschichte.commit/snapshot])))

  p/Branchable
  (branches [this] (p/branches this nil))
  (branches [_ _opts]
    (into #{} (comp (filter #(str/starts-with? % "refs/heads/"))
                    (map branch-name))
          (keys (repo/refs conn))))
  (current-branch [_] (branch-name (repo/current-ref conn)))
  (branch! [this name] (p/branch! this name nil nil))
  (branch! [this name from] (p/branch! this name from nil))
  (branch! [this name from _opts]
    (platform-async
     (let [commit (or (resolve-commit conn from)
                      (throw (ex-info "Unknown branch point" {:from from})))]
       (await (repo/create-ref! conn (branch-ref name) commit))
       this)))
  (delete-branch! [this name] (p/delete-branch! this name nil))
  (delete-branch! [this name opts]
    (platform-async
     (await (repo/delete-branch! conn (branch-ref name)
                                 (select-keys opts [:force?])))
     this))
  (checkout [this name] (p/checkout this name nil))
  (checkout [_ name opts]
    (platform-async
     (await (repo/checkout! conn (branch-ref name)
                            (select-keys opts [:force?])))
     (->GeschichteSystem conn system-name)))

  p/Graphable
  (history [this] (p/history this nil))
  (history [_ opts]
    (mapv (comp str :geschichte.commit/id)
          (repo/log conn {:limit (or (:limit opts) 100)})))
  (ancestors [this snap-id] (p/ancestors this snap-id nil))
  (ancestors [_ snap-id _opts]
    (let [commit (resolve-commit conn snap-id)
          commits (commit-index conn)]
      (set (map str (keys (graph/ancestor-distances
                           #(parents-of commits %)
                           (:geschichte.commit/id commit)))))))
  (ancestor? [this a b] (p/ancestor? this a b nil))
  (ancestor? [this a b _opts]
    (contains? (p/ancestors this b) (str (:geschichte.commit/id
                                          (resolve-commit conn a)))))
  (common-ancestor [this a b] (p/common-ancestor this a b nil))
  (common-ancestor [_ a b _opts]
    (let [commits (commit-index conn)
          a (:geschichte.commit/id (resolve-commit conn a))
          b (:geschichte.commit/id (resolve-commit conn b))]
      (some-> (graph/merge-base #(parents-of commits %) a b) str)))
  (commit-graph [_ _opts]
    (let [commits (commit-index conn)]
      {:nodes (into {}
                    (map (fn [[id commit]]
                           [(str id)
                            {:parent-ids
                             (set (map (comp str :geschichte.commit/id)
                                       (:geschichte.commit/parents commit)))
                             :meta (select-keys
                                    commit
                                    [:geschichte.commit/message
                                     :geschichte.commit/author
                                     :geschichte.commit/time])}]))
                    commits)
       :branches (into {}
                       (comp (filter (fn [[ref _]]
                                       (str/starts-with? ref "refs/heads/")))
                             (map (fn [[ref id]] [(branch-name ref) (str id)])))
                       (repo/refs conn))
       :roots (into #{}
                    (keep (fn [[id commit]]
                            (when (empty? (:geschichte.commit/parents commit))
                              (str id))))
                    commits)}))
  (commit-graph [this] (p/commit-graph this nil))
  (commit-info [this snap-id] (p/commit-info this snap-id nil))
  (commit-info [this snap-id _opts] (p/snapshot-meta this snap-id))

  p/Committable
  (commit! [this] (p/commit! this nil nil))
  (commit! [this message] (p/commit! this message nil))
  (commit! [this message opts]
    (platform-async
     (await (repo/commit! conn {:message (or message "")
                                :author (or (:author opts) "unknown")
                                :time (:time opts)}))
     this))

  p/GarbageCollectable
  (gc-roots [_]
    ;; Every ref tip, as the snapshot it names. These are the git refs whose
    ;; commits must survive collection.
    ;; `repo/refs` is {logical-ref -> commit-id-or-nil}; a ref with no target
    ;; (a freshly created branch) contributes nothing.
    (->> (repo/refs conn)
         vals
         (remove nil?)
         (keep (fn [cid] (:geschichte.commit/snapshot (repo/commit-by-id conn cid))))
         (map str)
         set))

  (gc-sweep! [this snapshot-ids] (p/gc-sweep! this snapshot-ids nil))
  (gc-sweep! [_ _snapshot-ids opts]
    ;; Reclaim orphaned Datahike index blobs. Datahike computes its own
    ;; reachability, so the coordinator's snapshot-ids are ignored — same
    ;; contract as the datahike adapter.
    ;;
    ;; A NON-EPOCH `:remove-before` IS REFUSED, and that is the whole point of
    ;; this method existing rather than letting the generic adapter run.
    ;; `:geschichte.commit/snapshot` resolves through `d/commit-as-db`, so a
    ;; Datahike commit record IS a Git tree — and Datahike follows ancestry only
    ;; while a record is newer than the cutoff, because its own liveness rule is
    ;; reachability AND recency. Geschichte's refs are ordinary datoms, invisible
    ;; to that rule, so every commit looks like unreferenced old history.
    ;;
    ;; Measured: three commits plus a non-epoch cutoff reclaimed 75 keys, after
    ;; which `repo/tree`, `repo/status` and `repo/tree-at` all throw "Geschichte
    ;; commit names a missing Datahike checkpoint" while `repo/read` still works
    ;; — a silently half-destroyed repository. Refusing is strictly better than
    ;; reclaiming and bricking.
    ;;
    ;; Epoch (the default) is safe and reclaims writes published to Konserve
    ;; that no transaction ever made reachable: interrupted pack imports, killed
    ;; agents, aborted large writes.
    (let [opts (t/async-gc-opts "geschichte/gc-sweep!" opts)
          rb   (:remove-before opts)
          epoch (#?(:clj java.util.Date. :cljs js/Date.) 0)]
      (when (and rb (pos? (inst-ms rb)))
        (throw (ex-info (str "Refusing a non-epoch :remove-before on a Geschichte repository. "
                             "Geschichte's refs are datoms, so Datahike cannot see them as "
                             "roots; a cutoff would delete the commit snapshots the refs name "
                             "and silently brick the repository.")
                        {:type :geschichte/unsafe-gc-cutoff
                         :remove-before rb
                         :system system-name})))
      (platform-async
       (if (:dry-run? opts)
         {:system-id system-name :dry-run? true}
         (let [removed (await (execution/io-result (d/gc-storage conn epoch)
                                                   execution/default-opts))]
           {:system-id system-name
            :reclaimed (if (counted? removed) (count removed) 0)})))))

  p/Overlayable
  (overlay [this _opts]
    (platform-async
     (let [branch (workspace/branch-key (random-uuid))
           base (p/snapshot-id this)
           _ (await (workspace/fork! conn branch))
           cfg (assoc (:config @conn) :branch branch)
           child-conn (await (execution/io-result
                              (d/connect cfg #?(:clj {:sync? true}
                                                :cljs {:sync? false}))
                              execution/default-opts))]
       (->GeschichteOverlay this
                            (atom (->GeschichteSystem child-conn system-name))
                            branch base :frozen)))))

(defn create
  "Wrap an initialized Geschichte connection as a Yggdrasil system."
  ([conn] (create conn {}))
  ([conn {:keys [system-name]}]
   (->GeschichteSystem conn system-name)))

(defn connection
  "Return the active Geschichte workspace connection represented by a
  Yggdrasil system or overlay. This is the virtual-filesystem integration seam:
  consumers receive a branch-local connection without assuming that the
  workspace has a physical `working-path`."
  [system]
  (cond
    (instance? GeschichteSystem system) (:conn system)
    (instance? GeschichteOverlay system) (some-> system :local-writes deref :conn)
    :else nil))

(defn workspace-id
  "Stable identity for the active virtual workspace. The repository store id is
  shared by structurally related workspaces; the Datahike branch distinguishes
  one fork from another."
  [system]
  (when-let [conn (connection system)]
    [(get-in @conn [:config :store :id])
     (get-in @conn [:config :branch] :db)]))
