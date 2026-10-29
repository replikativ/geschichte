(ns geschichte.merge
  "Commit ancestry and three-tree merge planning over a Geschichte repository.

  Ancestry queries run natively over the `:geschichte.commit/parents` ref edge
  with datahike's graph algorithms — a single-source BFS that touches only the
  reachable subgraph. Entity ids are the traversal space; stable commit ids are
  mapped back at the query boundary. The cross-peer-stable merge-base tie-break
  stays in `geschichte.merge.core` (commit ids, not local entity ids)."
  (:require [datahike.api :as d]
            [datahike.experimental.graph :as graph]
            [datahike.experimental.graph-spec :as graph-spec]
            [geschichte.merge.core :as core]
            [geschichte.budget :as budget]
            [geschichte.merge.policy :as policy]
            [geschichte.text-budget :as text-budget]
            [geschichte.repo :as repo]))

(def ^:private commit-graph
  (graph-spec/attr-graph :geschichte.commit/parents))

(defn budget-profile
  "Positive runtime compatibility marker for bounded native planning."
  [] (policy/profile))

(defn recommended-budget-policy
  "Finite merge limits; combine with import limits BEFORE creating one shared
  operation. Omitted timeout defaults to 30 seconds."
  ([] (policy/recommended-policy))
  ([timeout-ms] (policy/recommended-policy timeout-ms)))

(defn- budget-graph [operation]
  (reify graph-spec/GraphSpec
    (out-neighbors [_ db node]
      (budget/charge! operation :commit-visits 1)
      (map (fn [neighbor] (budget/charge! operation :graph-edges 1) neighbor)
           (graph-spec/out-neighbors commit-graph db node)))
    (in-neighbors [_ db node] (graph-spec/in-neighbors commit-graph db node))
    (all-edges [_ db] (graph-spec/all-edges commit-graph db))
    (all-nodes [_ db] (graph-spec/all-nodes commit-graph db))
    (edge-weight [_ db source target] (graph-spec/edge-weight commit-graph db source target))
    (directed? [_] (graph-spec/directed? commit-graph))))

(defn- distances-at [db root operation]
  (budget/check! operation)
  (let [root-eid (d/q '[:find ?e . :in $ ?id :where [?e :geschichte.commit/id ?id]] db root)
        _ (budget/check! operation)]
    (if-not root-eid
      {}
      (let [by-eid (graph/bfs-distances (budget-graph operation) db root-eid)]
        (into {} (map (fn [[eid distance]]
                        (budget/check! operation)
                        [(:geschichte.commit/id (d/pull db [:geschichte.commit/id] eid)) distance])) by-eid)))))

(defn- tree-at-bounded [db commit operation]
  (budget/check! operation)
  (let [commit (d/pull db [:geschichte.commit/snapshot] [:geschichte.commit/id commit])
        snapshot (when-let [id (:geschichte.commit/snapshot commit)] (d/commit-as-db db id))]
    (when-not snapshot (throw (ex-info "Missing commit snapshot" {:commit commit})))
    (reduce (fn [out datom]
              (budget/charge! operation :tree-entries 1)
              (let [path (:v datom)
                    size (text-budget/utf8-size operation path)]
                (budget/bound! operation :path-bytes size)
                (budget/charge! operation :tree-path-bytes size)
                (let [entry (d/pull snapshot [:geschichte.stage/state :geschichte.stage/size
                                              :geschichte.stage/mode
                                              {:geschichte.stage/content [:geschichte.content/id]}] (:e datom))]
                  (if (= :present (:geschichte.stage/state entry))
                    (do (let [id (get-in entry [:geschichte.stage/content :geschichte.content/id])
                              size (:geschichte.stage/size entry)
                              mode (:geschichte.stage/mode entry)
                              content (when (uuid? id) (d/pull db [:geschichte.content/size] [:geschichte.content/id id]))]
                          (when-not (and (integer? size) (<= 0 size Long/MAX_VALUE)
                                         (integer? mode) (<= 0 mode Long/MAX_VALUE)
                                         (uuid? id) (= size (:geschichte.content/size content)))
                            (throw (ex-info "Invalid logical tree entry metadata" {:type ::invalid-tree-entry :path path}))))
                        (budget/bound! operation :object-bytes (:geschichte.stage/size entry))
                        (budget/charge! operation :tree-bytes (:geschichte.stage/size entry))
                        (assoc out path {:content (get-in entry [:geschichte.stage/content :geschichte.content/id])
                                         :size (:geschichte.stage/size entry) :mode (:geschichte.stage/mode entry)}))
                    out))))
            (sorted-map) (d/datoms snapshot :avet :geschichte.stage/path))))

(defn ancestor-distances
  "Return {commit-id shortest-parent-distance}, including the root at distance 0.
  Single-source BFS over the parent ref edge; entity ids mapped to commit ids."
  [conn root]
  (let [db @conn
        root-eid (d/q '[:find ?e . :in $ ?id
                        :where [?e :geschichte.commit/id ?id]]
                      db root)]
    (if (nil? root-eid)
      {}
      (let [by-eid (graph/bfs-distances commit-graph db root-eid)
            id-of (into {} (d/q '[:find ?e ?id
                                  :in $ [?e ...]
                                  :where [?e :geschichte.commit/id ?id]]
                                db (keys by-eid)))]
        (into {} (map (fn [[e dist]] [(id-of e) dist])) by-eid)))))

(defn merge-base
  "Choose a nearest common ancestor. Criss-cross ties are broken deterministically
  by commit id, so the choice is stable across peers; a later recursive merge-base
  strategy can synthesize virtual ancestors at this seam."
  [conn ours theirs]
  (core/merge-base-from-distances (ancestor-distances conn ours)
                                  (ancestor-distances conn theirs)))

(defn plan
  "Plan a three-tree merge without staging or advancing repository refs.
  Default content resolution may store merged immutable content in this
  caller-owned private connection, even if a later path conflicts/refuses.
  :merge-budget opts into the complete finite profile and one captured source
  snapshot; typed resource refusals are errors, never ordinary conflicts.

  Tree values are stable logical content/mode metadata, so physical full-vs-delta
  choices cannot create conflicts. Returns a resolved `:tree` plus structural
  conflicts containing base/ours/theirs values (or `::absent` for deletion)."
  ([conn ours theirs] (plan conn ours theirs nil))
  ([conn ours theirs {:keys [merge-content? merge-budget] :or {merge-content? true}}]
   (when (some? merge-budget) (policy/require! merge-budget))
   (let [db @conn
         base (if merge-budget
                (core/merge-base-from-distances (distances-at db ours merge-budget)
                                                (distances-at db theirs merge-budget))
                (merge-base conn ours theirs))]
     (budget/check! merge-budget)
     (when-not base
       (throw (ex-info "Commits have no common ancestor"
                       {:ours ours :theirs theirs})))
     ;; Content merging on by default: a path both sides edited in different
     ;; places is not a conflict anyone wants reported, and leaving it off by
     ;; default would mean every existing caller keeps arbitrating merges that
     ;; resolve themselves. `:merge-content? false` restores the structural-only
     ;; plan for callers that want to see raw tree divergence.
     (core/plan-trees base ours theirs
                      (if merge-budget (tree-at-bounded db base merge-budget) (repo/tree-at conn base))
                      (if merge-budget (tree-at-bounded db ours merge-budget) (repo/tree-at conn ours))
                      (if merge-budget (tree-at-bounded db theirs merge-budget) (repo/tree-at conn theirs))
                      {:merge-budget merge-budget
                       :resolve-content (when merge-content?
                                          (repo/content-merger conn {:merge-budget merge-budget :read-db db}))}))))

(defn prepare!
  "Plan and stage a clean merge. Conflicting plans are returned unchanged and do
  not mutate the repository. Call `geschichte.repo/commit!` after preparation."
  [conn ours theirs]
  (let [result (plan conn ours theirs)]
    (if (:clean? result)
      (repo/prepare-merge! conn result)
      result)))
