(ns geschichte.git.budget
  "JVM Git stream adapter for the portable operation budget. Public Git budget
  functions and refusal keywords remain compatible with existing callers."
  (:require [geschichte.budget :as operation])
  (:import [java.io FilterInputStream InputStream]))

(defn create [policy]
  (try
    (assoc (operation/create policy) :error-namespace "geschichte.git.budget")
    (catch clojure.lang.ExceptionInfo error
      (if (= :geschichte.budget/invalid-policy (:type (ex-data error)))
        (throw (ex-info (ex-message error) (assoc (ex-data error) :type ::invalid-policy) error))
        (throw error)))))

(defn check! [budget] (operation/check! budget))
(defn remaining-ms [budget] (operation/remaining-ms budget))
(defn bound! [budget metric value] (operation/bound! budget metric value))
(defn charge! [budget metric value] (operation/charge! budget metric value))
(defn snapshot [budget] (operation/snapshot budget))

(defn input-stream
  "Borrow an input stream and charge every response byte read. Closing this
  wrapper closes the borrowed stream; checks surround each underlying read."
  ^InputStream [^InputStream input budget]
  (if (or (nil? budget) (identical? budget (::budget (meta input)))) input
      (proxy [FilterInputStream clojure.lang.IMeta] [input]
        (meta [] {::budget budget})
        (read
          ([]
           (check! budget)
           (let [value (.read input)]
             (charge! budget :response-bytes (if (neg? value) 0 1))
             value))
          ([buffer]
           (check! budget)
           (let [n (.read input ^bytes buffer 0 (alength ^bytes buffer))]
             (charge! budget :response-bytes (max 0 n))
             n))
          ([buffer offset length]
           (check! budget)
           (let [n (.read input ^bytes buffer (int offset) (int length))]
             (charge! budget :response-bytes (max 0 n))
             n))))))
