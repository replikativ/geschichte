(ns geschichte.git-pull-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.git.client :as client]
            [geschichte.git.pack :as pack]
            [geschichte.git.project :as project]
            [geschichte.git.store :as store]
            [geschichte.repo :as repo]))

(defn- config []
  {:store {:backend :memory :id (random-uuid)}
   :schema-flexibility :write :keep-history? true :commit-graph? true})

(defn- utf8 [text] (.getBytes ^String text "UTF-8"))

(defn- fetched [oid]
  {:refs [{:ref "HEAD" :oid oid
           :attributes {:symref-target "refs/heads/main"}}
          {:ref "refs/heads/main" :oid oid :attributes {}}]})

(deftest pull-fast-forward-rejection-and-merge-policy
  (let [source-cfg (config)
        ff-cfg (config)
        merge-cfg (config)]
    (doseq [cfg [source-cfg ff-cfg merge-cfg]] (d/create-database cfg))
    (let [source (d/connect source-cfg)
          ff-target (d/connect ff-cfg)
          merge-target (d/connect merge-cfg)]
      (try
        (repo/init! source)
        (repo/write! source "base.txt" (utf8 "base\n"))
        (repo/stage-all! source)
        (let [base (repo/commit! source {:message "base" :author "Ada"
                                         :time (java.util.Date. 0)})
              base-graph (project/project source (:geschichte.commit/id base))
              base-oid (:oid base-graph)]
          (repo/write! source "remote.txt" (utf8 "remote\n"))
          (repo/stage-all! source)
          (let [remote (repo/commit! source {:message "remote" :author "Ada"
                                             :time (java.util.Date. 1000)})
                graph (project/project source (:geschichte.commit/id remote))
                remote-oid (:oid graph)
                encoded (pack/encode (:objects graph))]
            (doseq [target [ff-target merge-target]]
              (repo/init! target)
              (store/import-pack! target encoded)
              (client/checkout-fetched! target (fetched base-oid)))

            (let [result (client/apply-pull! ff-target "origin"
                                             (fetched remote-oid))]
              (is (= :fast-forward (:pull/status result)))
              (is (= "remote\n" (String. ^bytes (repo/read ff-target "remote.txt")
                                         "UTF-8"))))

            (repo/write! merge-target "local.txt" (utf8 "local\n"))
            (repo/stage-all! merge-target)
            (repo/commit! merge-target {:message "local" :author "Ada"
                                        :time (java.util.Date. 2000)})
            (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not a fast-forward"
                                  (client/apply-pull! merge-target "origin"
                                                      (fetched remote-oid))))
            (is (= "local\n" (String. ^bytes (repo/read merge-target "local.txt")
                                      "UTF-8")))
            (let [result (client/apply-pull! merge-target "origin"
                                             (fetched remote-oid)
                                             {:policy :merge})]
              (is (= :merge-prepared (:pull/status result)))
              (is (= #{"remote.txt"}
                     (set (:staged (repo/status merge-target)))))
              (let [commit (repo/commit! merge-target {:message "merge"})]
                (is (= 2 (count (:geschichte.commit/parents
                                 (repo/commit-by-id
                                  merge-target (:geschichte.commit/id commit))))))))))
        (finally
          (doseq [conn [source ff-target merge-target]] (d/release conn))
          (doseq [cfg [source-cfg ff-cfg merge-cfg]]
            (d/delete-database cfg)))))))
