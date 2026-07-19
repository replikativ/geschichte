(ns geschichte.git-checkout-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.git.client :as client]
            [geschichte.git.checkout :as checkout]
            [geschichte.git.object :as object]
            [geschichte.git.pack :as pack]
            [geschichte.git.project :as project]
            [geschichte.git.store :as store]
            [geschichte.repo :as repo]))

(defn- config []
  {:store {:backend :memory :id (random-uuid)}
   :schema-flexibility :write :keep-history? true :commit-graph? true})

(defn- utf8 [s] (.getBytes ^String s "UTF-8"))

(deftest imported-history-materializes-only-selected-snapshot
  (let [source-cfg (config)
        target-cfg (config)]
    (d/create-database source-cfg)
    (d/create-database target-cfg)
    (let [source (d/connect source-cfg)
          target (d/connect target-cfg)]
      (try
        (repo/init! source)
        (repo/write! source "README.md" (utf8 "one\n"))
        (repo/stage-all! source)
        (repo/commit! source {:message "first" :author "Ada"
                              :time (java.util.Date. 0)})
        (repo/write! source "README.md" (utf8 "two\n"))
        (repo/write! source "src/a.txt" (utf8 "a\n"))
        (repo/stage-all! source)
        (let [second (repo/commit! source {:message "second" :author "Ada"
                                           :time (java.util.Date. 1000)})
              graph (project/project source (:geschichte.commit/id second))
              oid (:oid graph)]
          (repo/init! target)
          (store/import-pack! target (pack/encode (:objects graph)))
          (let [transactions (atom [])
                transact d/transact
                result (with-redefs [d/transact
                                     (fn [conn tx-data]
                                       (swap! transactions conj tx-data)
                                       (transact conn tx-data))]
                         (client/checkout-fetched!
                          target
                          {:refs [{:ref "HEAD" :oid oid
                                   :attributes
                                   {:symref-target "refs/heads/main"}}
                                  {:ref "refs/heads/from-git" :oid oid}]}
                          {:branch "from-git"}))
                graph-transactions
                (filterv #(some :geschichte.commit/git-oid %) @transactions)
                history (repo/log target)]
            (is (= 2 (:files result)))
            (is (= 2 (:imported-commits result)))
            (is (= 1 (count graph-transactions)))
            (is (some :geschichte.ref/name (first graph-transactions))
                "commit graph and selected ref publish atomically")
            (is (= "refs/heads/from-git" (repo/current-ref target)))
            (is (= "two\n" (String. ^bytes (repo/read target "README.md")
                                    "UTF-8")))
            (is (= ["second" "first"]
                   (mapv :geschichte.commit/message history)))
            (is (:clean? (repo/status target)))
            (is (nil? (:geschichte.commit/snapshot (second history))))
            (repo/write! target "README.md" (utf8 "three\n"))
            (repo/stage-all! target)
            (let [local (repo/commit! target {:message "local third"
                                              :author "Ada"
                                              :time (java.util.Date. 2000)})
                  projected (project/project
                             target (:geschichte.commit/id local))
                  root-payload (get-in projected
                                       [:objects (:oid projected) :payload])]
              (is (= oid (get-in projected
                                 [:commits (:commit result)]))
                  "the imported tip remains an exact Git-OID boundary")
              (is (.contains (String. ^bytes root-payload "UTF-8")
                             (str "parent " oid)))
              (is (contains? (:objects projected) oid)
                  "the exact imported graph is included for a new remote")
              (is (every? (fn [[object-oid {:keys [type payload]}]]
                            (= object-oid (object/object-id type payload)))
                          (:objects projected))))))
        (finally
          (d/release source)
          (d/release target)
          (d/delete-database source-cfg)
          (d/delete-database target-cfg))))))
