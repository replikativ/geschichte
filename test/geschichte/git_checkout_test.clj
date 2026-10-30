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

(defn- graph-for-root [root-payload objects]
  (let [tree-oid (object/object-id :tree root-payload)
        commit-payload (object/commit-payload
                        {:tree tree-oid :author "Fixture <fixture@example.test> 0 +0000"
                         :committer "Fixture <fixture@example.test> 0 +0000" :message "fixture"})
        commit-oid (object/object-id :commit commit-payload)]
    {:oid commit-oid :commits {}
     :objects (assoc objects tree-oid {:type :tree :payload root-payload}
                     commit-oid {:type :commit :payload commit-payload})}))

(defn- raw-leaf-tree [name-bytes blob-oid]
  (object/concat-bytes (utf8 "100644 ") name-bytes (byte-array [0]) (object/hex->bytes blob-oid)))

(deftest unsupported-git-components-refuse-instead-of-being-normalized
  (doseq [name-bytes [(utf8 (str "a" (char 92) "b")) (utf8 "a/b") (utf8 " ") (utf8 "")
                      (utf8 ".") (utf8 "..") (utf8 ".git") (utf8 ".Geschichte")
                      (byte-array [(unchecked-byte 0x80)]) (byte-array [(unchecked-byte 0x81)])]]
    (let [cfg (config)]
      (d/create-database cfg)
      (let [conn (d/connect cfg)]
        (try
          (repo/init! conn)
          (let [payload (utf8 "original bytes") oid (object/object-id :blob payload)
                graph (graph-for-root (raw-leaf-tree name-bytes oid) {oid {:type :blob :payload payload}})]
            (store/persist-graph! conn graph)
            (let [failure (try (checkout/materialize! conn (:oid graph)) nil (catch Exception error error))]
              (is (= ::checkout/unsupported-entry-name (:type (ex-data failure))))
              (is (empty? (repo/files conn)))
              (is (nil? (repo/head-commit conn)))))
          (finally (d/release conn) (d/delete-database cfg)))))))

(deftest supported-git-component-spelling-is-preserved
  (doseq [name ["研究.md" " readable .md " "replacement-�.md"]]
    (let [cfg (config)]
      (d/create-database cfg)
      (let [conn (d/connect cfg)]
        (try
          (repo/init! conn)
          (let [payload (utf8 "faithful") oid (object/object-id :blob payload)
                graph (graph-for-root (object/tree-payload [{:mode "100644" :name name :oid oid}])
                                      {oid {:type :blob :payload payload}})]
            (store/persist-graph! conn graph)
            (checkout/materialize! conn (:oid graph))
            (is (= [name] (repo/files conn)))
            (is (= "faithful" (String. ^bytes (repo/read conn name) "UTF-8"))))
          (finally (d/release conn) (d/delete-database cfg)))))))

(deftest literal-backslash-entry-cannot-overwrite-a-nested-file
  (let [cfg (config)]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (repo/init! conn)
        (let [literal (utf8 "literal filename") nested (utf8 "nested filename")
              literal-oid (object/object-id :blob literal) nested-oid (object/object-id :blob nested)
              subtree (object/tree-payload [{:mode "100644" :name "b" :oid nested-oid}])
              subtree-oid (object/object-id :tree subtree)
              root (object/tree-payload [{:mode "40000" :name "a" :oid subtree-oid}
                                         {:mode "100644" :name (str "a" (char 92) "b") :oid literal-oid}])
              graph (graph-for-root root {literal-oid {:type :blob :payload literal}
                                          nested-oid {:type :blob :payload nested}
                                          subtree-oid {:type :tree :payload subtree}})]
          (store/persist-graph! conn graph)
          (let [failure (try (checkout/materialize! conn (:oid graph)) nil (catch Exception error error))]
            (is (= ::checkout/unsupported-entry-name (:type (ex-data failure))))
            ;; Private streamed preparation may contain earlier valid entries;
            ;; none may be overwritten by a normalized alias or published tip.
            (when-let [actual (repo/read conn "a/b")]
              (is (= "nested filename" (String. ^bytes actual "UTF-8"))))
            (is (nil? (repo/head-commit conn)))))
        (finally (d/release conn) (d/delete-database cfg))))))

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

(deftest requested-logical-ids-ignore-unrelated-oid-mappings
  (let [cfg (config)]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (repo/init! conn)
        (let [rows (mapv (fn [n] {:geschichte.commit/id (random-uuid)
                                  :geschichte.commit/git-oid (format "%040x" n)}) (range 120))
              _ (d/transact conn rows)
              requested [(:geschichte.commit/git-oid (first rows)) (format "%040x" 999)]
              db @conn reads (atom 0) calls (atom []) query d/q
              source (reify clojure.lang.IDeref (deref [_] (swap! reads inc) db))
              result (with-redefs [d/q (fn [form snapshot oid]
                                         (swap! calls conj oid)
                                         (query form snapshot oid))]
                       (#'checkout/logical-ids source requested))]
          (is (= (:geschichte.commit/id (first rows)) (get result (first requested))))
          (is (= (java.util.UUID/nameUUIDFromBytes
                  (.getBytes (str "geschichte:git:sha1:" (second requested)) "UTF-8"))
                 (get result (second requested))))
          (is (= 1 @reads))
          (is (= requested @calls))
          (is (= (set requested) (set (keys result)))))
        (finally (d/release conn) (d/delete-database cfg))))))
