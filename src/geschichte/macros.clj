(ns ^:no-doc geschichte.macros
  "Compile one I/O body to direct JVM code or partial-cps CLJS code."
  (:require [clojure.walk :as walk]
            [is.simm.partial-cps.async]))

(def ^:private async->sync
  '{is.simm.partial-cps.async/async do
    is.simm.partial-cps.async/await do
    async do
    await do})

(defmacro async+sync
  "When `sync?` is true, erase partial-cps `async`/`await` forms to direct
  evaluation. Otherwise return the CPS-transformed asynchronous computation."
  [sync? async-code]
  (let [sync-code (walk/postwalk (fn [node] (get async->sync node node))
                                 async-code)]
    `(if ~sync? ~sync-code ~async-code)))

(defmacro platform-async
  "Compile `body` to a direct JVM value or a partial-cps CLJS computation."
  [& body]
  `(async+sync (:sync? geschichte.async/default-opts)
               (is.simm.partial-cps.async/async ~@body)))
