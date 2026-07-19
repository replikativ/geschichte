(ns geschichte.merge-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.merge :as merge]
            [geschichte.repo :as repo]))

(defn- utf8 [s] (.getBytes ^String s "UTF-8"))

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
              (let [merged (repo/commit! conn {:message "merge feature"})
                    parents (:geschichte.commit/parents
                             (repo/commit-by-id conn
                                                (:geschichte.commit/id merged)))]
                (is (= #{main feature}
                       (set (map :geschichte.commit/id parents)))))

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
