(ns geschichte.ignore-node-test
  (:require [cljs.core.async :refer [<!] :refer-macros [go]]
            [cljs.test :refer-macros [async deftest is]]
            [datahike.api :as d]
            [geschichte.bytes :as bytes]
            [geschichte.ignore :as ignore]
            [geschichte.repo :as repo]
            [is.simm.partial-cps.core-async :as bridge]))

(deftest node-ignore-rules
  (async done
         (go
           (let [config {:store {:backend :memory :id (random-uuid)}
                         :schema-flexibility :write}]
             (try
               (<! (d/create-database config))
               (let [conn (<! (d/connect config {:sync? false}))]
                 (bridge/unwrap-result
                  (<! (bridge/->chan (repo/init! conn))))
                 (bridge/unwrap-result
                  (<! (bridge/->chan
                       (repo/write! conn ".gitignore"
                                    (bytes/utf8 "*.log\n!keep.log\n")))))
                 (let [rules (bridge/unwrap-result
                              (<! (bridge/->chan (ignore/rules conn))))]
                   (is (ignore/ignored? rules "build.log"))
                   (is (not (ignore/ignored? rules "keep.log"))))
                 (d/release conn))
               (catch :default error
                 (is false (str "Node ignore test failed: " (.-message error))))
               (finally
                 (<! (d/delete-database config))
                 (done)))))))
