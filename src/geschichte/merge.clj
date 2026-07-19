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
            [geschichte.repo :as repo]))

(def ^:private commit-graph
  (graph-spec/attr-graph :geschichte.commit/parents))

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
  "Plan a three-tree merge without mutating the repository.

  Tree values are stable logical content/mode metadata, so physical full-vs-delta
  choices cannot create conflicts. Returns a resolved `:tree` plus structural
  conflicts containing base/ours/theirs values (or `::absent` for deletion)."
  [conn ours theirs]
  (let [base (merge-base conn ours theirs)]
    (when-not base
      (throw (ex-info "Commits have no common ancestor"
                      {:ours ours :theirs theirs})))
    (core/plan-trees base ours theirs
                     (repo/tree-at conn base)
                     (repo/tree-at conn ours)
                     (repo/tree-at conn theirs))))

(defn prepare!
  "Plan and stage a clean merge. Conflicting plans are returned unchanged and do
  not mutate the repository. Call `geschichte.repo/commit!` after preparation."
  [conn ours theirs]
  (let [result (plan conn ours theirs)]
    (if (:clean? result)
      (repo/prepare-merge! conn result)
      result)))
