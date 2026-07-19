(ns geschichte.store-blob-test
  (:require [clojure.test :refer [deftest is testing]]
            [geschichte.store.blob :as blob]
            [konserve.core :as k]))

(deftest bounded-parallel-publication
  (let [values (mapv #(byte-array [(byte %)]) (range 8))
        active (atom 0)
        maximum-active (atom 0)
        connection (atom {:store ::store})]
    (with-redefs [k/bassoc
                  (fn [_store _key _value _opts]
                    (let [n (swap! active inc)]
                      (swap! maximum-active max n)
                      (Thread/sleep 20)
                      (swap! active dec)
                      true))]
      (is (= (mapv blob/content-id values)
             (blob/put-many! connection values
                             {:blob-write-parallelism 2})))
      (is (= 2 @maximum-active)))))

(deftest parallel-publication-propagates-write-failures
  (let [values [(byte-array [1]) (byte-array [2])]
        connection (atom {:store ::store})
        other-write-finished? (atom false)]
    (with-redefs [k/bassoc
                  (fn [_store _key value _opts]
                    (if (= 1 (aget ^bytes value 0))
                      (throw (ex-info "failed blob write" {}))
                      (do
                        (Thread/sleep 50)
                        (reset! other-write-finished? true))))]
      (testing "no store-ref manifest can be published after a failed write"
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"failed blob write"
                              (blob/put-many! connection values)))
        (is @other-write-finished?
            "all writes settle before the caller can close its GC guard")))))
