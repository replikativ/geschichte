(ns geschichte.merge-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.merge :as merge]
            [geschichte.query :as query]
            [geschichte.repo :as repo]))

(defn- utf8 [s] (.getBytes ^String s "UTF-8"))

(defn- pending-fixture []
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? true :commit-graph? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (repo/init! conn)
        (repo/write! conn "base.txt" (utf8 "base"))
        (repo/stage-all! conn)
        (let [base (:geschichte.commit/id (repo/commit! conn {:message "base"}))]
          (repo/branch! conn "base")
          (repo/branch! conn "feature")
          (repo/write! conn "main.txt" (utf8 "main"))
          (repo/stage-all! conn)
          (let [main (:geschichte.commit/id (repo/commit! conn {:message "main"}))]
            (repo/checkout! conn "feature")
            (repo/write! conn "feature.txt" (utf8 "feature"))
            (repo/stage-all! conn)
            (let [feature (:geschichte.commit/id (repo/commit! conn {:message "feature"}))]
              (repo/checkout! conn "main")
              (merge/prepare! conn main feature)
              {:cfg cfg :conn conn :base base :main main :feature feature})))
        (catch Exception error (d/release conn) (d/delete-database cfg) (throw error))))))

(defn- pending? [conn]
  (some? (:geschichte.repo/merge-parent (query/repository @conn))))

(deftest reset-cancels-or-preserves-merge-intent-like-native-git
  (doseq [mode [:soft :mixed :hard]]
    (let [{:keys [cfg conn main]} (pending-fixture)]
      (try
        (is (pending? conn))
        (let [before @conn work (repo/worktree conn)]
          (if (= :soft mode)
            (let [failure (try (repo/reset! conn main {:mode mode}) nil (catch Exception error error))]
              (is (= :geschichte.repo/pending-merge-soft-reset (:type (ex-data failure))))
              (is (= before @conn)))
            (do
              (repo/reset! conn main {:mode mode})
              (is (not (pending? conn)))
              (is (= (repo/tree-at conn main) (repo/tree conn :index)))
              (is (= (if (= :mixed mode) work (repo/tree-at conn main)) (repo/worktree conn)))
              (repo/write! conn "after.txt" (utf8 "after"))
              (repo/stage-all! conn)
              (let [commit (repo/commit! conn {:message "ordinary after reset"})]
                (is (= #{main} (set (map second (:geschichte.commit/parents commit)))))))))
        (finally (d/release conn) (d/delete-database cfg))))))

(deftest successful-checkout-clears-pending-merge
  (doseq [branch ["base" "main"]]
    (let [{:keys [cfg conn base main]} (pending-fixture)]
      (try
        (repo/checkout! conn branch {:force? true})
        (is (not (pending? conn)))
        (repo/write! conn "after.txt" (utf8 "after"))
        (repo/stage-all! conn)
        (let [commit (repo/commit! conn {:message "ordinary after checkout"})]
          (is (= #{(if (= branch "base") base main)}
                 (set (map second (:geschichte.commit/parents commit))))))
        (finally (d/release conn) (d/delete-database cfg))))))

(deftest failed-reset-and-checkout-preserve-the-pending-merge
  (let [{:keys [cfg conn]} (pending-fixture)]
    (try
      (let [before @conn]
        (doseq [operation [#(repo/checkout! conn "missing" {:force? true})
                           #(repo/checkout! conn "base")
                           #(repo/reset! conn (random-uuid) {:mode :hard})]]
          (is (thrown? Exception (operation)))
          (is (= before @conn))
          (is (pending? conn))))
      (finally (d/release conn) (d/delete-database cfg)))))

(deftest merge-base-clean-merge-and-conflict-plan
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? true :commit-graph? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (repo/init! conn)
        (repo/write! conn "a" (utf8 "base a\n"))
        (repo/write! conn "b" (utf8 "base b\n"))
        (repo/stage-all! conn)
        (let [base (:geschichte.commit/id
                    (repo/commit! conn {:message "base"}))]
          (repo/branch! conn "feature")
          (repo/checkout! conn "feature")
          (repo/write! conn "a" (utf8 "feature a\n"))
          (repo/stage-all! conn)
          (let [feature (:geschichte.commit/id
                         (repo/commit! conn {:message "feature"}))]
            (repo/checkout! conn "main")
            (repo/write! conn "b" (utf8 "main b\n"))
            (repo/stage-all! conn)
            (let [main (:geschichte.commit/id
                        (repo/commit! conn {:message "main"}))
                  clean (merge/plan conn main feature)]
              (is (= base (merge/merge-base conn main feature)))
              (is (:clean? clean))
              (is (= #{"a" "b"} (set (keys (:tree clean)))))

              (merge/prepare! conn main feature)
              (let [before @conn
                    failure (try (repo/commit! conn {:message "Cannot amend during merge" :amend? true})
                                 nil (catch Exception error error))]
                (is (= :geschichte.repo/amend-during-merge (:type (ex-data failure))))
                (is (= before @conn)))
              (let [merged (repo/commit! conn {:message "merge feature"})
                    parents (:geschichte.commit/parents
                             (repo/commit-by-id conn
                                                (:geschichte.commit/id merged)))]
                (is (= #{main feature}
                       (set (map :geschichte.commit/id parents))))
                (let [amended (repo/commit! conn {:message "merge amended" :amend? true})]
                  (is (= #{main feature}
                         (set (map :geschichte.commit/id
                                   (:geschichte.commit/parents
                                    (repo/commit-by-id conn (:geschichte.commit/id amended)))))))))

              (repo/checkout! conn "feature")
              (repo/write! conn "b" (utf8 "feature b conflict\n"))
              (repo/stage-all! conn)
              (let [feature-2 (:geschichte.commit/id
                               (repo/commit! conn {:message "feature conflict"}))
                    conflict (merge/plan conn main feature-2)]
                (is (not (:clean? conflict)))
                (is (= #{"b"} (set (keys (:conflicts conflict)))))))))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))
