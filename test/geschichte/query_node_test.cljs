(ns geschichte.query-node-test
  (:require [cljs.core.async :refer [<!] :refer-macros [go]]
            [cljs.test :refer-macros [async deftest is]]
            [datahike.api :as d]
            [geschichte.query :as query]
            [geschichte.schema :as schema]))

(deftest node-datahike-query-views
  (async done
         (go
           (let [cfg {:store {:backend :memory :id (random-uuid)}
                      :schema-flexibility :write :keep-history? true}]
             (try
               (<! (d/create-database cfg))
               (let [conn (<! (d/connect cfg {:sync? false}))
                     repo-id (random-uuid)
                     commit-id (random-uuid)]
                 (<! (d/transact! conn schema/schema))
                 (<! (d/transact!
                      conn
                      [{:geschichte.repo/id repo-id
                        :geschichte.repo/name "node"
                        :geschichte.repo/head "refs/heads/main"}
                       {:geschichte.commit/id commit-id
                        :geschichte.commit/snapshot (random-uuid)
                        :geschichte.commit/message "from node"
                        :geschichte.commit/author "CLJS"
                        :geschichte.commit/time (js/Date.)}
                       {:geschichte.ref/name "refs/heads/main"
                        :geschichte.ref/target [:geschichte.commit/id commit-id]}]))
                 (is (query/initialized? @conn))
                 (is (= "refs/heads/main" (query/current-ref @conn)))
                 (is (= commit-id (get (query/refs @conn) "refs/heads/main")))
                 (is (= "from node"
                        (:geschichte.commit/message (query/commit @conn commit-id))))
                 (d/release conn))
               (catch :default error
                 (is false (str "Node Datahike query test failed: " (.-message error))))
               (finally
                 (<! (d/delete-database cfg))
                 (done)))))))
