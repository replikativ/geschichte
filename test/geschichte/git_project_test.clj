(ns geschichte.git-project-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.git.client :as client]
            [geschichte.git.object :as object]
            [geschichte.git.project :as project]
            [geschichte.git.store :as git-store]
            [geschichte.repo :as repo]))

(deftest malformed-retained-tree-cannot-silently-lose-files-during-export
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? true :commit-graph? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (repo/init! conn)
        (let [{:keys [content size mode]} (repo/write! conn "a" (object/utf8 "bytes"))]
          (repo/stage-all! conn)
          ;; Simulate retained history from an older writer that allowed this
          ;; invalid tree. New public writes/commits reject it earlier.
          (d/transact conn [{:geschichte.stage/path "a/b" :geschichte.stage/state :present
                             :geschichte.stage/content [:geschichte.content/id content]
                             :geschichte.stage/size (long size) :geschichte.stage/mode (long mode)}])
          (let [id (random-uuid) snapshot (d/commit-id @conn)]
            (d/transact conn [{:geschichte.commit/id id :geschichte.commit/snapshot snapshot
                               :geschichte.commit/message "retained invalid tree"
                               :geschichte.commit/author "fixture" :geschichte.commit/time (java.util.Date.)}])
            (let [before @conn
                  failure (try (project/project conn id) nil (catch Exception error error))]
              (is (= :geschichte.tree/path-conflict (:type (ex-data failure))))
              (is (= before @conn)))))
        (finally (d/release conn) (d/delete-database cfg))))))

(deftest snapshots-project-to-a-complete-git-graph
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? true :commit-graph? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (repo/init! conn)
        (repo/write! conn "src/a.txt" (object/utf8 "one\n"))
        (repo/stage-all! conn)
        (let [c1 (repo/commit! conn {:message "first\n" :author "Ada"
                                     :time (java.util.Date. 0)})]
          (repo/write! conn "README.md" (object/utf8 "read me\n"))
          (repo/stage-all! conn)
          (let [c2 (repo/commit! conn {:message "second\n" :author "Ada"
                                       :time (java.util.Date. 1000)})
                graph (project/project conn (:geschichte.commit/id c2))
                objects (:objects graph)
                commits (:commits graph)]
            (is (= 2 (count commits)))
            (is (= :commit (get-in objects [(:oid graph) :type])))
            (is (= #{:blob :tree :commit} (set (map :type (vals objects)))))
            (is (contains? commits (:geschichte.commit/id c1)))
            (is (.contains
                 (String. ^bytes (get-in objects [(:oid graph) :payload]) "UTF-8")
                 (str "parent " (get commits (:geschichte.commit/id c1)))))
            (let [stored (git-store/persist-graph! conn graph)]
              (is (= (count objects) (:persisted stored)))
              (is (= 0 (:persisted (git-store/persist-graph! conn graph))))
              (is (= (seq (get-in objects [(:oid graph) :payload]))
                     (seq (git-store/read-payload conn (:oid graph))))))
            (let [push (client/prepare-push
                        conn (:geschichte.commit/id c2)
                        {:ref "refs/heads/main"})]
              (is (= (:oid graph) (:oid push)))
              (is (= (count objects) (:objects push)))
              (is (pos? (:pack-bytes push))))
            (let [deletion (client/prepare-push
                            conn nil {:old (:oid graph)
                                      :ref "refs/heads/obsolete"})]
              (is (= 0 (:objects deletion)))
              (is (= 0 (:pack-bytes deletion)))
              (is (= (apply str (repeat 40 "0")) (:oid deletion)))
              (is (.contains (String. ^bytes (:request deletion) "ISO-8859-1")
                             "refs/heads/obsolete")))))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))
