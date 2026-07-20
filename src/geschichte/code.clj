(ns geschichte.code
  "Code objects over a geschichte repository — the queryable, versioned code
  model.

  A geschichte repository is a Datahike database, so its commit DAG, refs and
  file contents are already datoms (see doc/querying.md). This namespace adds a
  thin layer above them: each top-level *form* of every Clojure file version
  becomes an entity, **content-addressed** by the hash of its source text, so a
  form unchanged across N commits is one entity — the same content-addressing
  geschichte uses for blobs, one level down. Occurrences link a form to the
  commit(s) that carried it, so a form has provenance in the DAG.

  This is close in spirit to Datomic's codeq (git history → datoms, definitions
  tracked across time): `:code.form/*` is codeq's def, and `:code.form/calls` is
  its callsites. Two capabilities geschichte adds on top live in optional
  namespaces:

    - `geschichte.code.embed` — semantic search (embeddings → Proximum KNN,
      callable as a Datalog clause). Needs `org.replikativ/proximum` and a
      Datahike with `:db.type/float-array` + the external-engine query-spec-fn.
    - `geschichte.code.ast` — deep structural queries via tools.analyzer.

  The core here (this namespace) has no optional dependencies: form splitting,
  content-addressing, and callsite extraction give codeq-style syntactic search
  out of the box."
  (:require [clojure.string :as str]
            [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :as rt]
            [datahike.api :as d])
  (:import [java.security MessageDigest]))

;; ---------------------------------------------------------------------------
;; Schema

(def schema
  "Datahike schema for the code layer. Transact once into a geschichte repo
  connection. `geschichte.code.embed/schema` adds the embedding attribute."
  [;; A distinct top-level form, keyed by the hash of its source text. Identical
   ;; text across commits collapses to one entity (and, downstream, one
   ;; embedding / one AST).
   {:db/ident :code.form/hash
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :code.form/kind          ; :defn :def :ns :deftest :defmacro …
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one}
   {:db/ident :code.form/name          ; best-effort defined name
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :code.form/text          ; verbatim source
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   ;; Symbols the form invokes (call position). Codeq-style callsites — cheap,
   ;; robust, needs no analyzer: powers "which forms call X".
   {:db/ident :code.form/calls
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/many}

   ;; A form appeared at a path in a commit. Reified so path + commit travel
   ;; together; this is the join from a form to the commit DAG.
   {:db/ident :code.occurrence/form
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}
   {:db/ident :code.occurrence/commit  ; -> a :geschichte.commit entity
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}
   {:db/ident :code.occurrence/path
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}])

;; ---------------------------------------------------------------------------
;; File selection — Clojure only

(def clojure-extensions #{"clj" "cljc" "cljs" "cljd" "bb" "edn"})

(defn clojure-file?
  "True iff `path` names a Clojure source file we can split into forms.
  Non-Clojure files are skipped — the analyzer/reader only understand Clojure,
  and splitting arbitrary text into 'forms' is meaningless."
  [path]
  (when-let [ext (second (re-find #"\.([^.]+)$" (str path)))]
    (contains? clojure-extensions (str/lower-case ext))))

;; ---------------------------------------------------------------------------
;; Form splitting — verbatim source slices

(defn- form-name [form]
  (when (and (seq? form) (>= (count form) 2))
    (let [op (first form) nm (second form)]
      (when (and (symbol? op) (symbol? nm)) (str nm)))))

(defn- form-kind [form]
  (when (and (seq? form) (symbol? (first form)))
    (keyword (name (first form)))))

(defn split-forms
  "Split Clojure source `text` into its top-level forms, returning
  `[{:kind :name :text :line} …]`.

  Reads with position tracking, then slices the ORIGINAL source lines so the
  stored text is the real source (comments, formatting) rather than a printed
  round-trip of the read data. Reader-conditionals are preserved (`:read-cond
  :preserve`); an unreadable file yields an empty vector."
  [text]
  (let [lines (vec (str/split-lines text))
        rdr (rt/indexing-push-back-reader text)
        eof (Object.)]
    (loop [acc [], start-line 1]
      (let [form (try (binding [r/*read-eval* false]
                        (r/read {:eof eof :read-cond :preserve} rdr))
                      (catch Exception _ eof))]
        (if (identical? form eof)
          acc
          (let [end-line (rt/get-line-number rdr)
                slice (str/join "\n" (subvec lines
                                             (max 0 (dec start-line))
                                             (min (count lines) end-line)))]
            (recur (if (str/blank? slice)
                     acc
                     (conj acc {:kind (form-kind form)
                                :name (form-name form)
                                :line start-line
                                :text slice}))
                   (inc end-line))))))))

;; ---------------------------------------------------------------------------
;; Content addressing + callsites

(defn form-hash
  "Hex SHA-1 of a form's source text — its content-addressed identity."
  [^String text]
  (let [md (MessageDigest/getInstance "SHA-1")]
    (->> (.digest md (.getBytes text "UTF-8"))
         (map #(format "%02x" (bit-and % 0xff)))
         (apply str))))

(defn called-symbols
  "The set of symbol names in call position anywhere in `form` (recursively) —
  the form's callsites. Read `text` with `read-form` first."
  [form]
  (let [acc (java.util.HashSet.)]
    (letfn [(walk [x]
              (cond
                (seq? x) (do (when (symbol? (first x)) (.add acc (name (first x))))
                             (doseq [e x] (walk e)))
                (coll? x) (doseq [e x] (walk e))))]
      (walk form))
    (set acc)))

(defn read-form
  "Read one form from `text` for structural inspection (callsites), or nil if it
  cannot be read. Never evals."
  [text]
  (try (binding [r/*read-eval* false]
         (r/read {:eof nil :read-cond :allow :features #{:clj}}
                 (rt/string-push-back-reader text)))
       (catch Throwable _ nil)))

(defn form-entity
  "A form map `{:kind :name :text …}` → a Datahike entity map keyed by content
  hash, including callsites. Merge extra keys (e.g. an embedding) as needed."
  [{:keys [kind name text]}]
  (let [calls (some-> (read-form text) called-symbols)]
    (cond-> {:code.form/hash (form-hash text)
             :code.form/text text}
      kind (assoc :code.form/kind kind)
      name (assoc :code.form/name name)
      (seq calls) (assoc :code.form/calls (vec calls)))))

;; ---------------------------------------------------------------------------
;; Ingestion

(defn transact-forms!
  "Ingest form sources into a geschichte repo `conn`. `sources` is a seq of
  `{:text <string> :path <string> :commit <commit-eid>}` — typically produced by
  walking the repository's Clojure blobs across history (see doc/code-search.md).

  Forms are content-addressed, so re-seeing an identical form is a no-op upsert;
  each (form, commit, path) becomes one occurrence. Returns
  `{:forms <n-distinct> :occurrences <n>}`.

  Non-`:code.form/embedding` only — embeddings are added by
  `geschichte.code.embed/index!`."
  [conn sources]
  (let [;; split each source into forms, keep the commit/path with each
        instances (for [{:keys [text path commit]} sources
                        :when (string? text)
                        form (split-forms text)]
                    (assoc form :path path :commit commit))
        by-hash (group-by (comp form-hash :text) instances)]
    ;; forms first (dedup by hash), in batches
    (doseq [batch (partition-all 500 by-hash)]
      (d/transact conn (mapv (fn [[_h [f & _]]] (form-entity f)) batch)))
    ;; then occurrences (need form eids resolved via the unique hash)
    (let [hash->eid (into {} (d/q '[:find ?h ?e :where [?e :code.form/hash ?h]] @conn))]
      (doseq [batch (partition-all 5000 instances)]
        (d/transact conn (into []
                               (keep (fn [{:keys [text path commit]}]
                                       (when-let [fe (hash->eid (form-hash text))]
                                         (when commit
                                           {:code.occurrence/form fe
                                            :code.occurrence/commit commit
                                            :code.occurrence/path path}))))
                               batch))))
    {:forms (count by-hash) :occurrences (count instances)}))
