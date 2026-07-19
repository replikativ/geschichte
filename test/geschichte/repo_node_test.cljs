(ns geschichte.repo-node-test
  (:require [cljs.core.async :refer [<!] :refer-macros [go]]
            [cljs.test :refer-macros [async deftest is]]
            [datahike.api :as d]
            [geschichte.bytes :as bytes]
            [geschichte.repo :as repo]
            [is.simm.partial-cps.core-async :as bridge]))

(defn- take-cps [computation]
  (bridge/->chan computation))

(deftest node-worktree-stage-and-status
  (async done
         (go
           (let [cfg {:store {:backend :memory :id (random-uuid)}
                      :schema-flexibility :write
                      :keep-history? true
                      :commit-graph? true}]
             (try
               (<! (d/create-database cfg))
               (let [conn (<! (d/connect cfg {:sync? false}))
                     repo-id (bridge/unwrap-result
                              (<! (take-cps (repo/init! conn))))]
                 (is (uuid? repo-id))
                 (is (= "refs/heads/main" (repo/current-ref conn)))
                 (is (= {:branch "refs/heads/main"
                         :head nil
                         :staged []
                         :unstaged []
                         :untracked []
                         :clean? true}
                        (bridge/unwrap-result
                         (<! (take-cps (repo/status conn))))))

                 (let [written (bridge/unwrap-result
                                (<! (take-cps
                                     (repo/write! conn "src/a.txt"
                                                  (bytes/utf8 "hello\n")))))
                       restored (bridge/unwrap-result
                                 (<! (take-cps
                                      (repo/read conn "src/a.txt"))))]
                   (is (= "src/a.txt" (:path written)))
                   (is (= "hello\n" (bytes/decode-utf8 restored)))
                   (is (= ["src/a.txt"] (repo/files conn)))
                   (is (= ["src/a.txt"]
                          (:untracked
                           (bridge/unwrap-result
                            (<! (take-cps (repo/status conn))))))))

                 (is (= ["src/a.txt"]
                        (bridge/unwrap-result
                         (<! (take-cps (repo/stage! conn ["src/a.txt"]))))))
                 (let [staged (bridge/unwrap-result
                               (<! (take-cps (repo/status conn))))]
                   (is (= ["src/a.txt"] (:staged staged)))
                   (is (empty? (:untracked staged))))
                 (is (= [{:path "src/a.txt" :index :added}]
                        (bridge/unwrap-result
                         (<! (take-cps (repo/status-entries conn))))))
                 (let [changes (bridge/unwrap-result
                                (<! (take-cps
                                     (repo/changes conn :head :index))))
                       content (bridge/unwrap-result
                                (<! (take-cps
                                     (repo/read-entry conn
                                                      (:after (first changes))))))]
                   (is (= :added (:kind (first changes))))
                   (is (= "hello\n" (bytes/decode-utf8 content))))

                 (is (true? (bridge/unwrap-result
                             (<! (take-cps
                                  (repo/remove! conn "src/a.txt"))))))
                 (is (= ["src/a.txt"]
                        (:unstaged
                         (bridge/unwrap-result
                          (<! (take-cps (repo/status conn)))))))
                 (d/release conn))
               (catch :default error
                 (is false (str "Node repository test failed: "
                                (.-message error))))
               (finally
                 (<! (d/delete-database cfg))
                 (done)))))))

(deftest node-logical-commits-branches-and-checkout
  (async done
         (go
           (let [cfg {:store {:backend :memory :id (random-uuid)}
                      :schema-flexibility :write
                      :keep-history? true
                      :commit-graph? true}]
             (try
               (<! (d/create-database cfg))
               (let [conn (<! (d/connect cfg {:sync? false}))]
                 (bridge/unwrap-result (<! (take-cps (repo/init! conn))))
                 (bridge/unwrap-result
                  (<! (take-cps
                       (repo/write! conn "story.txt" (bytes/utf8 "one\n")))))
                 (bridge/unwrap-result (<! (take-cps (repo/stage-all! conn))))
                 (let [first-commit
                       (bridge/unwrap-result
                        (<! (take-cps
                             (repo/commit! conn {:message "first"
                                                 :author "Node"}))))]
                   (is (:clean? (bridge/unwrap-result
                                 (<! (take-cps (repo/status conn))))))
                   (is (= "refs/heads/feature"
                          (bridge/unwrap-result
                           (<! (take-cps (repo/branch! conn "feature"))))))

                   (bridge/unwrap-result
                    (<! (take-cps
                         (repo/write! conn "story.txt"
                                      (bytes/utf8 "two\n")))))
                   (bridge/unwrap-result
                    (<! (take-cps (repo/stage-all! conn))))
                   (let [second-commit
                         (bridge/unwrap-result
                          (<! (take-cps
                               (repo/commit! conn {:message "second"
                                                   :author "Node"}))))]
                     (is (= [(:geschichte.commit/id second-commit)
                             (:geschichte.commit/id first-commit)]
                            (mapv :geschichte.commit/id (repo/log conn))))
                     (is (= (:geschichte.commit/id second-commit)
                            (get (repo/refs conn) "refs/heads/main")))
                     (is (= (:geschichte.commit/id first-commit)
                            (get (repo/refs conn) "refs/heads/feature"))))

                   (is (= "refs/heads/feature"
                          (bridge/unwrap-result
                           (<! (take-cps (repo/checkout! conn "feature"))))))
                   (is (= "one\n"
                          (bytes/decode-utf8
                           (bridge/unwrap-result
                            (<! (take-cps (repo/read conn "story.txt")))))))
                   (is (:clean? (bridge/unwrap-result
                                 (<! (take-cps (repo/status conn))))))

                   (bridge/unwrap-result
                    (<! (take-cps (repo/checkout! conn "main"))))
                   (is (= "two\n"
                          (bytes/decode-utf8
                           (bridge/unwrap-result
                            (<! (take-cps
                                 (repo/read conn "story.txt"))))))))
                 (d/release conn))
               (catch :default error
                 (is false (str "Node history test failed: "
                                (.-message error))))
               (finally
                 (<! (d/delete-database cfg))
                 (done)))))))
