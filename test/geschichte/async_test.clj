(ns geschichte.async-test
  (:refer-clojure :exclude [await])
  (:require [clojure.test :refer [deftest is]]
            [geschichte.async :as execution]
            [geschichte.macros :refer [async+sync]]
            [is.simm.partial-cps.async :refer [async await]]))

(deftest sync-mode-erases-cps
  (is (= 42
         (async+sync true
                     (async
                      (inc (await 41))))))
  (is (= {:sync? true} execution/default-opts))
  (is (= 7 (execution/io-result 7 {:sync? true}))))
