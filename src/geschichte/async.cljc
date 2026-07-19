(ns geschichte.async
  "Execution conventions at Geschichte's Datahike/platform I/O boundary."
  (:require [is.simm.partial-cps.core-async :as core-async]))

(def default-opts
  "Platform-default execution: direct values on JVM, awaitable CPS on CLJS."
  #?(:clj {:sync? true}
     :cljs {:sync? false}))

(defn opts
  "Merge caller options with the platform execution default."
  [provided]
  (merge default-opts (or provided {})))

(defn io-result
  "Bridge a Datahike/Konserve result into an awaitable value for the selected
  execution mode. Sync values pass through; async channels become CPS values."
  [result execution-opts]
  (core-async/sync-or-cps result execution-opts))

(defn promise->cps
  "Adapt a JavaScript Promise to partial-cps. Kept portable so callers can use
  it behind a reader conditional without adding another Promise abstraction."
  [promise]
  #?(:clj (throw (ex-info "Promises are a CLJS platform capability" {}))
     :cljs (fn [resolve reject] (.then promise resolve reject))))

(defn with-finalizer
  "Run an execution-mode-aware operation and always call `finalizer`.

  `operation` is a zero-argument function returning either a direct value in
  sync mode or a partial-cps computation in async mode. This is the resource
  bracket used for Datahike's unreferenced-write GC guard."
  [operation finalizer execution-opts]
  (if (:sync? execution-opts)
    (try
      (operation)
      (finally (finalizer)))
    (fn [resolve reject]
      (try
        (let [computation (operation)]
          (computation (fn [value]
                         (finalizer)
                         (resolve value))
                       (fn [error]
                         (finalizer)
                         (reject error))))
        (catch #?(:clj Throwable :cljs :default) error
          (finalizer)
          (reject error))))))
