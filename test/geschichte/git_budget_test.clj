(ns geschichte.git-budget-test
  (:require [clojure.test :refer [deftest is]]
            [geschichte.budget :as operation]
            [geschichte.git.budget :as budget])
  (:import [java.io ByteArrayInputStream]))

(defn- refusal [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest policy-is-validated-before-allocation
  (doseq [policy [{:limits {:bogus 1}} {:unexpected 1}
                  {:limits {:objects -1}} {:limits {:objects 1.0}}
                  {:limits {:objects (inc (bigint Long/MAX_VALUE))}}
                  {:timeout-ms 0} {:timeout-ms Long/MAX_VALUE} {:cancelled? 3}]]
    (is (= ::budget/invalid-policy (:type (refusal #(budget/create policy)))))))

(deftest concurrent-reservations-never-overspend
  (let [b (budget/create {:limits {:objects 25}})
        workers (doall (repeatedly 100 #(future (nil? (refusal (fn [] (budget/charge! b :objects 1)))))))]
    (is (= 25 (count (filter true? (map deref workers)))))
    (is (= 25 (get-in (budget/snapshot b) [:used :objects])))
    (is (= {:type ::budget/exceeded :metric :objects :limit 25 :used 25 :requested 1}
           (refusal #(budget/charge! b :objects 1))))))

(deftest counters-refuse-before-overflow
  (let [b (budget/create {})]
    (budget/charge! b :checkout-bytes Long/MAX_VALUE)
    (is (= ::budget/exceeded (:type (refusal #(budget/charge! b :checkout-bytes 1)))))
    (is (= Long/MAX_VALUE (get-in (budget/snapshot b) [:used :checkout-bytes])))))

(deftest portable-kernel-and-git-adapter-share-the-same-handle
  (is (= ::operation/invalid-policy (:type (refusal #(operation/create {:unknown true})))))
  (is (= ::budget/invalid-policy (:type (refusal #(budget/create {:unknown true})))))
  (let [b (budget/create {:limits {:objects 1}})]
    (operation/charge! b :objects 1)
    (is (= {:type ::budget/exceeded :metric :objects :limit 1 :used 1 :requested 1}
           (refusal #(budget/charge! b :objects 1))))
    (is (= 1 (get-in (operation/snapshot b) [:used :objects])))
    (is (= ::budget/exceeded (:type (refusal #(operation/check! b)))))))

(deftest cancellation-and-deadline-are-distinct
  (is (= ::budget/cancelled (:type (refusal #(budget/check! (budget/create {:cancelled? (constantly true)}))))))
  (let [b (assoc (budget/create {:timeout-ms 1}) :started (- (System/nanoTime) 2000000))]
    (is (= ::budget/deadline-exceeded (:type (refusal #(budget/check! b)))))))

(deftest response-stream-shares-byte-counter-and-closes-borrowed-body
  (let [closed (atom false)
        body (proxy [ByteArrayInputStream] [(byte-array 8)] (close [] (reset! closed true)))
        b (budget/create {:limits {:response-bytes 5}})]
    (with-open [input (budget/input-stream body b)]
      (is (= 0 (.read input)))
      (is (= 4 (.read input (byte-array 4))))
      (is (= ::budget/exceeded (:type (refusal #(.read input))))))
    (is @closed)
    (is (= 5 (get-in (budget/snapshot b) [:used :response-bytes])))))
