(ns geschichte.workspace-node-test
  (:require [cljs.core.async :refer [<!] :refer-macros [go]]
            [cljs.test :refer-macros [async deftest is]]
            [datahike.api :as d]
            [geschichte.bytes :as bytes]
            [geschichte.repo :as repo]
            [geschichte.workspace :as workspace]
            [is.simm.partial-cps.core-async :as bridge]))

(defn- take-cps [computation]
  (bridge/->chan computation))

(deftest node-workspace-fork-and-publish
  (async done
         (go
           (let [cfg {:store {:backend :memory :id (random-uuid)}
                      :schema-flexibility :write
                      :keep-history? true
                      :commit-graph? true}]
             (try
               (<! (d/create-database cfg))
               (let [canonical (<! (d/connect cfg {:sync? false}))]
                 (bridge/unwrap-result (<! (take-cps (repo/init! canonical))))
                 (bridge/unwrap-result
                  (<! (take-cps
                       (repo/write! canonical "base.txt"
                                    (bytes/utf8 "base\n")))))
                 (bridge/unwrap-result
                  (<! (take-cps (repo/stage-all! canonical))))
                 (bridge/unwrap-result
                  (<! (take-cps
                       (repo/commit! canonical {:message "base"}))))
                 (is (= :geschichte.workspace/node-agent
                        (bridge/unwrap-result
                         (<! (take-cps
                              (workspace/fork! canonical "node-agent"))))))
                 (let [worker (<! (d/connect
                                   (assoc cfg :branch
                                          :geschichte.workspace/node-agent)
                                   {:sync? false}))]
                   (bridge/unwrap-result
                    (<! (take-cps
                         (repo/write! worker "node.txt"
                                      (bytes/utf8 "node\n")))))
                   (bridge/unwrap-result
                    (<! (take-cps (repo/stage-all! worker))))
                   (let [commit (bridge/unwrap-result
                                 (<! (take-cps
                                      (repo/commit! worker
                                                    {:message "node"}))))
                         published (bridge/unwrap-result
                                    (<! (take-cps
                                         (workspace/publish! canonical worker))))
                         historical (bridge/unwrap-result
                                     (<! (take-cps
                                          (repo/read-at
                                           canonical
                                           (:geschichte.commit/id commit)
                                           "node.txt"))))]
                     (is (= (:geschichte.commit/id commit) (:new published)))
                     (is (= "node\n" (bytes/decode-utf8 historical))))
                   (d/release worker))
                 (d/release canonical))
               (catch :default error
                 (is false (str "Node workspace test failed: "
                                (.-message error))))
               (finally
                 (<! (d/delete-database cfg))
                 (done)))))))
