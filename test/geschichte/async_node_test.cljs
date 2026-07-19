(ns geschichte.async-node-test
  (:refer-clojure :exclude [await])
  (:require [cljs.core.async :as core-async]
            [cljs.test :as test :refer-macros [deftest is]]
            [geschichte.async :as execution]
            [is.simm.partial-cps.async :refer [await]]
            [is.simm.partial-cps.core-async :as bridge])
  (:require-macros [geschichte.macros :refer [async+sync]]
                   [is.simm.partial-cps.async :refer [async]]))

(deftest node-cps-bridges-datahike-shaped-channel
  (test/async done
              (let [channel (core-async/chan 1)
                    computation
                    (async+sync false
                                (async
                                 (+ 1 (js/Number
                                       (await (execution/io-result
                                               channel {:sync? false}))))))]
                (core-async/put! channel 41)
                (computation
                 (fn [value]
                   (is (= 42 value))
                   (is (= {:sync? false} execution/default-opts))
                   (done))
                 (fn [error]
                   (is false (str "partial-cps bridge failed: " (.-message error)))
                   (done))))))

(deftest bridge-retains-errors
  (test/async done
              (let [channel (core-async/chan 1)
                    computation
                    (async
                     (await (bridge/chan->cps channel)))]
                (core-async/put! channel (js/Error. "expected"))
                (computation
                 (fn [_]
                   (is false "error channel unexpectedly resolved")
                   (done))
                 (fn [error]
                   (is (= "expected" (.-message error)))
                   (done))))))
