(ns geschichte.budget-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [geschichte.budget :as budget]))

(defn- refusal [f]
  (try (f) nil (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) error (ex-data error))))

(deftest portable-operation-budget-has-one-terminal-account
  (let [b (budget/create {:limits {:objects 2 :object-bytes 8}})]
    (budget/charge! b :objects 2)
    (is (= 2 (get-in (budget/snapshot b) [:used :objects])))
    (let [error (refusal #(budget/charge! b :objects 1))]
      (is (= ::budget/exceeded (:type error)))
      (is (= error (refusal #(budget/bound! b :object-bytes 1))))
      (is (= 2 (get-in (budget/snapshot b) [:used :objects]))))))

(deftest portable-budget-validates-before-work
  (doseq [policy [{:limits {:unknown 1}} {:unknown true}
                  {:limits {:objects -1}} {:limits {:objects 1.25}} {:timeout-ms 0}]]
    (is (= ::budget/invalid-policy (:type (refusal #(budget/create policy)))))))

(deftest portable-cancellation-and-monotonic-deadline
  (is (= ::budget/cancelled
         (:type (refusal #(budget/check! (budget/create {:cancelled? (constantly true)}))))))
  (let [b (budget/create {:timeout-ms 1})
        expired (update b :started - #?(:clj 2000000 :cljs 2))]
    (is (= ::budget/deadline-exceeded (:type (refusal #(budget/check! expired)))))))

(deftest portable-counter-refuses-before-integer-precision-loss
  (let [maximum #?(:clj Long/MAX_VALUE :cljs js/Number.MAX_SAFE_INTEGER)
        b (budget/create {})]
    (budget/charge! b :checkout-bytes maximum)
    (is (= ::budget/exceeded (:type (refusal #(budget/charge! b :checkout-bytes 1)))))
    (is (= maximum (get-in (budget/snapshot b) [:used :checkout-bytes])))))

(deftest absent-budget-keeps-unbounded-callers-unchanged
  (is (nil? (budget/check! nil)))
  (is (nil? (budget/charge! nil :unknown -1)))
  (is (nil? (budget/bound! nil :unknown -1)))
  (is (nil? (budget/remaining-ms nil)))
  (is (nil? (budget/snapshot nil))))
