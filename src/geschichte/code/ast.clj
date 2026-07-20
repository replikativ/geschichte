(ns geschichte.code.ast
  "Deep structural (syntactic) queries over forms, via tools.analyzer.

  Optional. Requires `org.clojure/tools.analyzer.jvm` (the `:ast` alias).

  `geschichte.code`'s `:code.form/calls` already covers callsites cheaply. This
  goes further — full control-flow structure (`:loop`/`:recur`/`:if`/… op
  counts) — at the cost of analyzing each form. Analysis is heavier and can
  fail on forms that reference unresolvable host classes, so prefer running it
  **on-demand over a small result set** (a KNN neighbourhood, a Datalog result)
  rather than indexing every form.

  A subtlety this namespace handles for you: `tools.analyzer.jvm` *interns* the
  var when it parses a `def` node (to resolve self-references), which would
  clobber `clojure.core` names in your namespace as it analyzes corpus code like
  `(defn map …)`. We point the analysis env at a throwaway namespace so the
  interning lands there instead. `run-passes` is bypassed (`identity`) so no
  resolution passes run — structural analysis needs none."
  (:require [clojure.tools.analyzer.jvm :as ana]
            [clojure.tools.analyzer.ast :as ast]
            [geschichte.code :as code]
            [datahike.api :as d]))

(def ^:private sandbox
  "Throwaway namespace the analyzer interns corpus `def`s into (with core
  referred so macroexpansion resolves), keeping the caller's ns clean."
  (delay (let [ns (create-ns (gensym "geschichte.code.ast.sandbox"))]
           (binding [*ns* ns] (refer-clojure))
           ns)))

(defn analyze
  "Analyze a Clojure `form` to a tools.analyzer AST, or nil if it can't be
  analyzed. Interning is isolated to a sandbox namespace; resolution passes are
  skipped so it works on forms out of their original context."
  [form]
  (try
    (binding [ana/run-passes identity]
      (ana/analyze form (assoc (ana/empty-env) :ns (ns-name @sandbox))))
    (catch Throwable _ nil)))

(defn ops
  "Frequency map of AST `:op`s in `form`'s text (e.g. `{:invoke 4 :if 2 :loop 1}`),
  or nil if it can't be analyzed."
  [text]
  (when-let [form (code/read-form text)]
    (when-let [a (analyze form)]
      (frequencies (map :op (ast/nodes a))))))

(defn shape
  "A compact structural summary of a form's text — control-flow presence and
  size — for classifying/filtering a result set:
  `{:loop? :recur? :if :invoke :nodes}`. nil if unanalyzable."
  [text]
  (when-let [o (ops text)]
    {:loop? (contains? o :loop)
     :recur? (contains? o :recur)
     :if (get o :if 0)
     :invoke (get o :invoke 0)
     :nodes (reduce + (vals o))}))

(defn shape-of
  "Structural shape for a form entity `e` in `db` (reads its `:code.form/text`)."
  [db e]
  (some-> (:code.form/text (d/pull db [:code.form/text] e)) shape))
