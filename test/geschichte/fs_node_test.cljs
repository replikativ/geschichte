(ns geschichte.fs-node-test
  (:require [cljs.core.async :refer [<!] :refer-macros [go]]
            [cljs.test :refer-macros [async deftest is]]
            [datahike.api :as d]
            [geschichte.bytes :as bytes]
            [geschichte.fs :as fs]
            [geschichte.repo :as repo]
            [is.simm.partial-cps.core-async :as bridge]))

(defn- take-cps [computation]
  (bridge/->chan computation))

(deftest node-filesystem-facade
  (async done
         (go
           (let [cfg {:store {:backend :memory :id (random-uuid)}
                      :schema-flexibility :write
                      :keep-history? true}]
             (try
               (<! (d/create-database cfg))
               (let [conn (<! (d/connect cfg {:sync? false}))]
                 (bridge/unwrap-result (<! (take-cps (repo/init! conn))))
                 (bridge/unwrap-result
                  (<! (take-cps
                       (fs/write! conn "src/lib/a.txt"
                                  (bytes/utf8 "portable\n")))))
                 (is (= #{"" "src" "src/lib"} (set (fs/directories conn))))
                 (is (= [{:path "src/lib" :type :dir :size 0 :name "lib"}]
                        (fs/list-dir conn "src")))
                 (is (= "portable\n"
                        (bytes/decode-utf8
                         (bridge/unwrap-result
                          (<! (take-cps (fs/read conn "src/lib/a.txt")))))))
                 (is (= "src/lib/b.txt"
                        (bridge/unwrap-result
                         (<! (take-cps
                              (fs/rename! conn "src/lib/a.txt"
                                          "src/lib/b.txt"))))))
                 (is (true? (bridge/unwrap-result
                             (<! (take-cps
                                  (fs/delete! conn "src/lib/b.txt"))))))
                 (d/release conn))
               (catch :default error
                 (is false (str "Node filesystem test failed: "
                                (.-message error))))
               (finally
                 (<! (d/delete-database cfg))
                 (done)))))))
