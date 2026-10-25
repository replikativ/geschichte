(ns geschichte.content-range-node-test
  (:require [cljs.core.async :refer [<!] :refer-macros [go]]
            [cljs.test :refer-macros [async deftest is]]
            [datahike.api :as d]
            [geschichte.async :as execution]
            [geschichte.bytes :as bytes]
            [geschichte.content :as content]
            [geschichte.schema :as schema]
            [geschichte.store.blob :as payload]
            [is.simm.partial-cps.core-async :as bridge]))

(deftest sparse-ranges-await-consumers-and-stop-before-the-next-fetch
  (async done
         (go
           (let [cfg {:store {:backend :memory :id (random-uuid)}
                      :schema-flexibility :write :keep-history? true}]
             (try
               (<! (d/create-database cfg))
               (let [conn (<! (d/connect cfg {:sync? false}))]
                 (try
                   (<! (d/transact! conn schema/schema))
                   (let [value (bytes/from-values (take 10000 (cycle (range 251))))
                         id (bridge/unwrap-result
                             (<! (bridge/->chan
                                  (content/transact-content! conn value nil (constantly [])
                                                             {:chunk-threshold 1024 :chunk-min-size 128
                                                              :chunk-size 257 :chunk-max-size 1024}))))
                         chunks (sort-by :geschichte.chunk/index (:geschichte.content/chunks (content/info conn id)))
                         last-chunk (last chunks)
                         offset (:geschichte.chunk/offset last-chunk)
                         n (min 50 (:geschichte.chunk/size last-chunk))
                         reads (atom []) parts (atom []) original payload/get-bytes]
                     (with-redefs [payload/get-bytes (fn
                                                       ([conn id] (swap! reads conj id) (original conn id))
                                                       ([conn id opts] (swap! reads conj id) (original conn id opts)))]
                       (bridge/unwrap-result
                        (<! (bridge/->chan (content/consume-range-by-id! conn id offset n #(swap! parts conj %)))))
                       (is (bytes/same-bytes? (bytes/slice value offset (+ offset n))
                                              (apply bytes/concat-bytes @parts)))
                       (is (= [(:geschichte.chunk/payload last-chunk)] @reads))
                       (reset! reads [])
                       (let [release (atom nil)
                             gate (js/Promise. (fn [resolve _] (reset! release resolve)))
                             entered (cljs.core.async/promise-chan)
                             result (bridge/->chan
                                     (content/consume-range-by-id!
                                      conn id 0 (bytes/length value)
                                      (fn [_]
                                        (cljs.core.async/put! entered true)
                                        (execution/promise->cps gate))
                                      {:async-consumer? true}))]
                         (<! entered)
                         (is (= 1 (count @reads)) "Consumer suspension prevents speculative payload reads")
                         (@release content/stop-consumption)
                         (is (true? (bridge/unwrap-result (<! result))))
                         (is (= 1 (count @reads))))
                       (reset! reads [])
                       (bridge/unwrap-result
                        (<! (bridge/->chan (content/consume-range-by-id! conn id 10000 0
                                                                         (fn [_] (is false "Empty range called consumer"))))))
                       (is (empty? @reads))
                       (is (thrown? js/Error (content/consume-range-by-id! conn id 9007199254740992 0 identity)))))
                   (finally (d/release conn))))
               (catch :default error (is false (str "Range consumption failed: " (.-message error))))
               (finally (<! (d/delete-database cfg)) (done)))))))
