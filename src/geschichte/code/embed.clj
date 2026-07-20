(ns geschichte.code.embed
  "Semantic layer over `geschichte.code`: embed each distinct form and index the
  vectors in Proximum, so nearest-neighbour search is a Datalog `:where` clause.

  Optional. Requires:
    - `org.replikativ/proximum` on the classpath, and its bridge
      `datahike.index.secondary.proximum` (datahike's `src-secondary`), via the
      `:embed` alias;
    - a Datahike with `:db.type/float-array` and the external-engine
      query-spec-fn (≥ 0.8.1746), so a `float[]` is a first-class value and
      `proximum/knn` is a clause;
    - Java 22+ (Proximum's HNSW).

  The embedder is *pluggable* — pass any `(fn [texts] -> seq-of-float[])`; this
  namespace has no dependency on a particular model. `org.replikativ/pretrained-rstr`
  is one such embedder (`emb/embed-texts`)."
  (:require [datahike.api :as d]
            ;; loading the bridge registers the :proximum secondary index type
            ;; and the `knn` external-engine clause fn
            [datahike.index.secondary.proximum :as prox]))

;; ---------------------------------------------------------------------------
;; Schema

(defn embedding-schema
  "The embedding attribute. `:db.secondary/only` keeps the (large) vector out of
  the primary index — it lives only in the covering Proximum index; the primary
  holds a content hash. `dim` must match your embedder's output width."
  []
  [{:db/ident :code.form/embedding
    :db/valueType :db.type/float-array
    :db/cardinality :db.cardinality/one
    :db.secondary/only true}])

(defn index-declaration
  "Schema datom declaring a Proximum HNSW secondary index over
  `:code.form/embedding`. Transact after `embedding-schema`. Options:
  `:idx-ident` (default :code/embeddings), `:dim` (required), `:distance`
  (default :cosine), `:capacity`, `:store-config` (a konserve store config —
  a `:file` backend keeps the heap small; **`:id` UUID is required**)."
  [{:keys [idx-ident dim distance capacity store-config]
    :or {idx-ident :code/embeddings distance :cosine}}]
  [{:db/ident idx-ident
    :db.secondary/type :proximum
    :db.secondary/attrs [:code.form/embedding]
    :db.secondary/config (cond-> {:dim dim :distance distance
                                  :store-config store-config}
                           capacity (assoc :capacity capacity))}])

;; ---------------------------------------------------------------------------
;; Indexing

(defn index!
  "Embed every form that has no embedding yet and transact the vectors, which
  the declared Proximum index picks up. `embed-fn` maps a batch of strings to a
  seq of `float[]` (one per string). Returns the number embedded.

  Call after the forms are ingested (`geschichte.code/transact-forms!`) and the
  index is declared. Idempotent — only forms missing `:code.form/embedding` are
  processed, so it resumes cleanly."
  ([conn embed-fn] (index! conn embed-fn {}))
  ([conn embed-fn {:keys [batch-size] :or {batch-size 256}}]
   (let [todo (d/q '[:find ?e ?text
                     :where [?e :code.form/text ?text]
                     (not [?e :code.form/embedding _])]
                   @conn)]
     (doseq [batch (partition-all batch-size todo)]
       (let [vecs (embed-fn (mapv second batch))]
         (d/transact conn (mapv (fn [[e _] v] {:db/id e :code.form/embedding v})
                                batch vecs))))
     (count todo))))

;; ---------------------------------------------------------------------------
;; Query — KNN is a datalog clause (via datahike.index.secondary.proximum/knn)

(defn knn-clause
  "The `:where` clause form for a KNN search — a convenience so callers needn't
  spell the fully-qualified symbol. Splice into a query:

    (d/q (into '[:find ?name ?distance :in $ ?qvec :where]
               [(gc.embed/knn-clause :code/embeddings '?qvec 10 '[[?e ?distance]])
                '[?e :code.form/name ?name]])
         db query-vector)

  `binding` is `[?e]` (filter, entities only) or `[[?e ?distance]]` (retrieval,
  entity + cosine distance). Planner-only — external-engine clauses need the
  query planner (on by default)."
  [idx-ident qvec-var k binding]
  [(list `prox/knn idx-ident qvec-var k) binding])

(defn nearest
  "Convenience: k forms nearest to `qvec` (a `float[]`), optionally within a
  Datalog constraint. Returns `[{:e :name :distance} …]` ordered by distance.
  `extra-where` is spliced after the KNN clause (all vars join on `?e`), e.g.
  `'[[?e :code.form/calls \"loop\"]]`. For richer results, write the `d/q`
  directly with `knn-clause`."
  ([db idx-ident qvec k] (nearest db idx-ident qvec k []))
  ([db idx-ident qvec k extra-where]
   (->> (d/q (into [:find '?e '?name '?distance :in '$ '?qvec :where
                    [(list `prox/knn idx-ident '?qvec k) '[[?e ?distance]]]
                    '[?e :code.form/name ?name]]
                   extra-where)
             db qvec)
        (map (fn [[e name distance]] {:e e :name name :distance distance}))
        (sort-by :distance))))
