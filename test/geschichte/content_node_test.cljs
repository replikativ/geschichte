(ns geschichte.content-node-test
  (:require [cljs.core.async :refer [<!] :refer-macros [go]]
            [cljs.test :refer-macros [async deftest is]]
            [datahike.api :as d]
            [datahike.blob :as blob]
            [datahike.gc-guard :as guard]
            [geschichte.bytes :as bytes]
            [geschichte.content :as content]
            [geschichte.schema :as schema]
            [geschichte.store.blob :as store-blob]
            [is.simm.partial-cps.core-async :as bridge]))

(defn- await-cps [computation]
  (bridge/->chan computation))

(deftest node-content-round-trip-and-line-delta
  (async done
         (go
           (let [store-id (random-uuid)
                 cfg {:store {:backend :memory :id store-id}
                      :schema-flexibility :write
                      :keep-history? true}
                 base (apply str
                             (map #(str "line " % " unchanged padding\n")
                                  (range 300)))
                 target (.replace base
                                  "line 150 unchanged"
                                  "line 150 changed")]
             (try
               (<! (d/create-database cfg))
               (let [conn (<! (d/connect cfg {:sync? false}))]
                 (<! (d/transact! conn schema/schema))
                 (let [raw-values [(bytes/utf8 "one") (bytes/utf8 "two")]
                       raw-ids
                       (bridge/unwrap-result
                        (<! (await-cps
                             (store-blob/put-many! conn raw-values))))
                       base-id
                       (bridge/unwrap-result
                        (<! (await-cps
                             (content/transact-content!
                              conn (bytes/utf8 base) nil (constantly [])))))
                       target-id
                       (bridge/unwrap-result
                        (<! (await-cps
                             (content/transact-content!
                              conn (bytes/utf8 target) base-id
                              (constantly [])))))
                       restored
                       (bridge/unwrap-result
                        (<! (await-cps (content/read-by-id conn target-id))))
                       chunked-value (bytes/from-values
                                      (take 10000 (cycle (range 251))))
                       chunked-id
                       (bridge/unwrap-result
                        (<! (await-cps
                             (content/transact-content!
                              conn chunked-value nil (constantly [])
                              {:chunk-threshold 1024
                               :chunk-min-size 128
                               :chunk-size 257
                               :chunk-max-size 1024}))))
                       seen (atom [])]
                   (is (= (mapv blob/blob-id raw-values) raw-ids))
                   (is (= :full
                          (:geschichte.content/kind
                           (content/info conn base-id))))
                   (is (= :line-delta
                          (:geschichte.content/kind
                           (content/info conn target-id))))
                   (is (= target (bytes/decode-utf8 restored)))
                   (bridge/unwrap-result
                    (<! (await-cps
                         (content/consume-by-id!
                          conn chunked-id
                          #(swap! seen conj (bytes/length %))))))
                   (is (= :chunks
                          (:geschichte.content/kind
                           (content/info conn chunked-id))))
                   (is (= :gear-32
                          (:geschichte.content/chunking-algorithm
                           (content/info conn chunked-id))))
                   (is (= (map :geschichte.chunk/size
                               (sort-by :geschichte.chunk/index
                                        (:geschichte.content/chunks
                                         (content/info conn chunked-id))))
                          @seen))
                   (is (not (guard/in-flight? store-id))))
                 (d/release conn))
               (catch :default error
                 (is false (str "Node content round trip failed: "
                                (.-message error))))
               (finally
                 (<! (d/delete-database cfg))
                 (done)))))))
