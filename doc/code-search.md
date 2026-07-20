# Semantic + syntactic code search

A geschichte repository is a Datahike database — its commit DAG, refs and file
contents are datoms (see [Querying](querying.md)). `geschichte.code` adds a code
model over them: each **top-level form** of every Clojure file version becomes an
entity, and you query code the same way you query the commit graph. Three kinds
of matching compose in one Datalog query:

| layer | namespace | how it matches | needs |
|---|---|---|---|
| **structural (callsites)** | `geschichte.code` | forms that *call* X | — (core) |
| **semantic** | `geschichte.code.embed` | forms *like* X (embedding KNN) | `:embed` alias |
| **structural (AST)** | `geschichte.code.ast` | forms with a control-flow *shape* | `:ast` alias |

…and every match joins back to the commit DAG — who wrote it, when, whether it
still exists at HEAD.

## Relation to Datomic codeq

[codeq](https://github.com/Datomic/codeq) (Rich Hickey, 2012) turns git history
into Datomic datoms and tracks **definitions across time** with source, location
and callsites. `geschichte.code` is the same idea on Datahike — `:code.form/*`
is codeq's def, `:code.form/calls` its callsites — with two things codeq never
had: forms are **content-addressed** (a form unchanged across N commits is one
entity, so you get de-duplication for free), and a **semantic** layer
(embeddings → nearest-neighbour search) sits alongside the syntactic one.

## Content addressing

The key move: a form's identity is the SHA-1 of its source text
(`:code.form/hash`, `:db.unique/identity`). On a full import of Clojure's history
that collapses **396,675 form instances → 10,090 distinct forms (38.7×)** — you
embed and analyze each distinct form once, and it carries its whole cross-commit
lifetime through `:code.occurrence/*`.

## Ingesting

Walk the repository's Clojure blobs and feed their source to `transact-forms!`.
Each source is `{:text <string> :path <string> :commit <commit-eid>}`; non-Clojure
files are skipped (`clojure-file?`).

```clojure
(require '[geschichte.code :as code]
         '[datahike.api :as d])

(d/transact conn code/schema)

;; `sources` — one entry per (commit, Clojure-blob). Producing it is a walk over
;; the repo's content; the shape is all `transact-forms!` needs:
(code/transact-forms! conn sources)
;; => {:forms 10090 :occurrences 396675}
```

Forms are de-duplicated by content hash (re-seeing a form is a no-op upsert);
each (form, commit, path) becomes one `:code.occurrence`.

## Layer 1 — structural (callsites), core

`:code.form/calls` holds the symbols each form invokes — codeq-style, cheap, no
analyzer. "Which forms call `swap!`?" is one clause:

```clojure
(d/q '[:find [?name ...] :where
       [?e :code.form/calls "swap!"] [?e :code.form/name ?name]]
     @conn)
```

## Layer 2 — semantic (embedding KNN), `:embed`

Embed each distinct form and index the vectors in
[Proximum](https://github.com/replikativ/proximum); a nearest-neighbour search is
then a Datalog `:where` clause (no manual bitset plumbing — via datahike's
external-engine query-spec-fn). The embedder is **pluggable** — any
`(fn [texts] -> [float[]])`;
[pretrained-rstr](https://github.com/replikativ/pretrained-rstr) is one.

```clojure
(require '[geschichte.code.embed :as embed])

(d/transact conn (embed/embedding-schema))
(d/transact conn (embed/index-declaration
                  {:dim 384 :capacity 20000
                   :store-config {:backend :file :path "/tmp/code-vectors"
                                  :id (random-uuid)}}))
(embed/index! conn my-embed-fn)          ; embeds forms lacking an embedding
```

Now KNN is a clause. Two binding shapes: `[?e]` (filter — entities only) and
`[[?e ?distance]]` (retrieval — entity + cosine distance):

```clojure
;; nearest to a query vector, joined to provenance
(d/q '[:find ?name ?distance
       :in $ ?qvec
       :where
       [(datahike.index.secondary.proximum/knn :code/embeddings ?qvec 10) [[?e ?distance]]]
       [?e :code.form/name ?name]]
     @conn (my-embed-fn ["reduce a collection to a single value"]))
```

### Semantic × syntactic — one query

The clause composes with the callsite layer. "Reduce-like code that uses an
explicit `loop`" — semantics find the family, syntax picks the imperative ones:

```clojure
(d/q '[:find [?name ...]
       :in $ ?qvec
       :where
       [(datahike.index.secondary.proximum/knn :code/embeddings ?qvec 50) [?e ...]]
       [?e :code.form/calls "loop"]
       [?e :code.form/name ?name]]
     @conn reduce-vec)
;; => on Clojure's history: iter-reduce, iterator-reduce!, …
```

### Semantic × temporal — dead ends

Restrict the neighbourhood to forms **absent from HEAD** — abandoned work similar
to a query, the "what were the dead ends" question. Compute the live set from the
commit graph, then use it as a Datalog constraint. On Clojure's history, the
nearest abandoned neighbours of the modern `reduce` are Rich Hickey's 2008–2009
hand-recursive implementations.

## Layer 3 — structural (AST), `:ast`

`:code.form/calls` is callsites; `geschichte.code.ast` goes to full control-flow
structure via `tools.analyzer` — `:loop`/`:recur`/`:if` presence and counts.
Analysis is heavier and can fail on forms referencing unresolvable host classes,
so run it **on-demand over a small result set** (a KNN neighbourhood), not as a
full index:

```clojure
(require '[geschichte.code.ast :as ast])

;; classify a semantic neighbourhood by implementation strategy
(for [{:keys [e name]} (embed/nearest @conn :code/embeddings reduce-vec 12)]
  (assoc (ast/shape-of @conn e) :name name))
;; => {:name "iter-reduce" :loop? true  :recur? true  :if 5 …}   ; imperative loop
;;    {:name "reduce"      :loop? false :recur? true  :if 2 …}   ; tail-recursive
;;    {:name "seq-reduce"  :loop? false :recur? false :if 1 …}   ; delegating
```

`ast/analyze` isolates `tools.analyzer.jvm`'s var-interning to a throwaway
namespace, so analyzing corpus code like `(defn map …)` never clobbers
`clojure.core` in your namespace.

## Why one store

None of these are expressible alone: Git has no joins, a vector database has no
ancestry, plain Datalog has no fuzzy similarity, and an AST index has no
semantics. Because a geschichte repository *is* a Datahike database, KNN, Datalog
joins, the commit DAG, and the AST live in one query engine — you ask for
"abandoned code semantically near this, that manually recurs, and who wrote it"
as a single query.
