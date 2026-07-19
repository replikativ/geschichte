(ns geschichte.file-store-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.repo :as repo]))

(deftest payload-survives-file-store-reconnect
  (let [id (random-uuid)
        cfg {:store {:backend :file
                     :path (str (System/getProperty "java.io.tmpdir")
                                "/geschichte-" id)
                     :id id}
             :schema-flexibility :write
             :keep-history? true
             :commit-graph? true}]
    (try
      (d/create-database cfg)
      (let [conn (d/connect cfg)]
        (repo/init! conn)
        (repo/write! conn "durable.txt" (.getBytes "durable\n" "UTF-8"))
        (repo/stage-all! conn)
        (repo/commit! conn {:message "persist"})
        (d/release conn))
      (let [conn (d/connect cfg)]
        (try
          (is (= "durable\n" (String. ^bytes (repo/read conn "durable.txt") "UTF-8")))
          (finally (d/release conn))))
      (finally
        (when (d/database-exists? cfg)
          (d/delete-database cfg))))))
