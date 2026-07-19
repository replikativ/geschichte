# Querying the repository

A geschichte repository is not an opaque object store you reach through
commands — it is a [Datahike](https://github.com/replikativ/datahike) database.
Commits, refs, the worktree, and content are ordinary datoms, and the commit DAG
is a plain ref edge (`:geschichte.commit/parents`). So the whole repository
answers to Datalog, and the parent edge answers to graph algorithms — in the
same query.

There are three layers, from plainest to most powerful:

1. Datalog over repository attributes (commits, refs, paths).
2. Graph algorithms over the commit DAG (ancestry, distance, merge-base).
3. Graph algorithms *inside* a query, and queries across *many* databases.

```clojure
(require '[datahike.api :as d]
         '[datahike.experimental.graph :as graph]
         '[datahike.experimental.graph-spec :as gs])
```

The results shown throughout are illustrative — a single live import of
Clojure's history (3,586 commits) into an in-memory repository. Your numbers
will differ; the queries will not.

## 1. Datalog over repository attributes

Commit metadata is just attributes, so ordinary Datalog replaces the
`git log | … | sort | uniq` pipelines. Every author, ranked:

```clojure
(->> (d/q '[:find ?author (count ?e)
            :where [?e :geschichte.commit/author ?author]]
          @conn)
     (sort-by second >))
;; => (["Rich Hickey <…>" 1837] ["Alex Miller <…>" 344]
;;     ["Stuart Halloway <…>" 219] …)   ; 207 authors in all
```

The worktree at a materialized commit is data too — files grouped by extension:

```clojure
(->> (d/q '[:find ?path :where [?f :geschichte.work/path ?path]] @conn)
     (map (comp second #(re-find #"\.([^.]+)$" (first %))))
     frequencies)
;; => {"java" 185, "clj" 141, "xml" 4, "yml" 4, "html" 3, …}
```

Refs are `:geschichte.ref/*` and `:geschichte.git.ref/*`; content is
`:geschichte.content/*` — all queryable the same way. See the
[schema](../src/geschichte/schema.cljc) for the full vocabulary.

## 2. The commit graph

Ancestry, distance, and merge-base *are* graph questions. Because
`:geschichte.commit/parents` is a `:db.type/ref`, Datahike's graph algorithms
run over it directly at O(V+E) — touching only the reachable subgraph — where a
recursive Datalog rule would materialize the whole transitive relation.

Build a graph spec once, then call an algorithm. The traversal space is entity
ids; resolve a stable `:geschichte.commit/id` to an entity id at the boundary:

```clojure
(def g (gs/attr-graph :geschichte.commit/parents))

(defn commit-eid [db id]
  (d/q '[:find ?e . :in $ ?id :where [?e :geschichte.commit/id ?id]] db id))

;; every ancestor of a commit — reachable set, O(V+E)
(graph/transitive-closure g @conn (commit-eid @conn head-id))
;; => #{…}   ; 3,585 entity ids, in ~114 ms

;; how deep the history runs — single-source BFS distances
(->> (graph/bfs-distances g @conn (commit-eid @conn head-id)) vals (apply max))
;; => 3293
```

For the merge-base of two commits, `lowest-common-ancestors` returns the
graph-theoretic set (a criss-cross has more than one). geschichte wraps this in
[`geschichte.merge`](../src/geschichte/merge.clj) with a **cross-peer-stable**
tie-break (by commit id, never a local entity id), so every replica picks the
same base:

```clojure
(require '[geschichte.merge :as merge])

(merge/ancestor-distances conn commit-id)   ; {commit-id shortest-distance}
(merge/merge-base conn ours-id theirs-id)    ; the deterministic merge-base
```

## 3. Graph algorithms inside a query

The graph algorithms are Datalog function clauses, so reachability composes with
attribute joins in one query. "Who authored the merge commits in HEAD's
ancestry?" — reachability × structure × attributes, together:

```clojure
(d/q '[:find ?author (count ?c)
       :in $ ?g ?head
       :where [(datahike.experimental.graph/transitive-closure ?g $ ?head) [?c ...]]
              [?c :geschichte.commit/parents ?p1]      ; two distinct parents
              [?c :geschichte.commit/parents ?p2]      ; ⇒ a merge commit
              [(not= ?p1 ?p2)]
              [?c :geschichte.commit/author ?author]]
     @conn g (commit-eid @conn head-id))
;; => #{["Rich Hickey <…>" 41] ["Stuart Halloway <…>" 2]}
```

The graph result binds `[?c ...]` and joins the rest of the clause like any
relation. In Git this is a pipeline of `rev-list --merges --format` and `awk`,
and you still can't hand the result to the next join.

## 4. Across repositories, and with application data

Each repository is its own database, and a Datalog query takes many sources — so
one query correlates two independent repositories. Which people committed to
*both* of two repos:

```clojure
(d/q '[:find [?author ...]
       :in $a $b
       :where [$a _ :geschichte.commit/author ?author]
              [$b _ :geschichte.commit/author ?author]]
     @repo-a @repo-b)
;; => ["Brandon Bloom <…>" "Nikita Prokopov <…>"]
```

Git has no notion of asking two repositories a single question. The same
multi-source join reaches beyond repositories to *any* other database — the
commit that introduced a file joined to the knowledge-base page that documents
it, and its owner:

```clojure
(d/q '[:find ?title ?author
       :in $repo $kb
       :where [$kb   ?page :page/mentions-commit ?id]
              [$repo ?c    :geschichte.commit/id ?id]
              [$repo ?c    :geschichte.commit/author ?author]
              [$kb   ?page :page/title ?title]]
     @repo @knowledge-base)
```

Because Datahike is bitemporal, a query can also range over the database's *own*
history — not only "the repository at commit X" (which Git gives you) but "what
the repository looked like as of ingestion-time T", a second, independent time
axis via `d/as-of` and `d/history`.

---

The graph algorithms (`transitive-closure`, `bfs-distances`,
`lowest-common-ancestors`) ship in Datahike's experimental graph namespace and
are documented in
[Datahike's graph algorithms guide](https://github.com/replikativ/datahike/blob/main/doc/graph-algorithms.md).
For the low-level API used above see the [Clojure API](clojure-api.md); for how
geschichte compares to other version-control-meets-database systems see
[Prior art](prior-art.md).
