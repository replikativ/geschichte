(ns geschichte.git-metadata-budget-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.git.budget :as budget]
            [geschichte.git.object :as object]
            [geschichte.git.pack :as pack]
            [geschichte.git.store :as store]
            [geschichte.repo :as repo]
            [geschichte.store.blob :as blob]))

(defn- refusal [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest pack-and-index-lookups-have-cold-and-warm-admission
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
             :keep-history? true :commit-graph? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (repo/init! conn)
        (let [oids (mapv (fn [n]
                           (let [value (object/utf8 (str "payload " n)) oid (object/object-id :blob value)]
                             (store/import-pack! conn (pack/encode {oid {:type :blob :payload value}}))
                             oid)) (range 12))
              missing (apply str (repeat 40 "0"))]
          (with-open [objects (store/open conn)]
            (doseq [warm? [false true]]
              (when warm? (doseq [oid oids] (store/read-object objects oid)))
              (doseq [[metric cap] [[:pack-visits 0] [:pack-metadata-parts 0]
                                    [:index-bytes 1] [:index-read-bytes 1]]]
                (let [operation (budget/create (assoc-in (store/metadata-budget-policy) [:limits metric] cap))
                      result (refusal #(store/read-object objects (first oids) {:import-budget operation}))]
                  (is (= ::budget/exceeded (:type result)))
                  (is (= metric (:metric result)))))
              (let [operation (budget/create (store/metadata-budget-policy))]
                (is (nil? (store/read-object objects missing {:import-budget operation})))
                (is (= 12 (get-in (budget/snapshot operation) [:used :pack-visits])))))))
        (finally (d/release conn) (d/delete-database cfg))))))

(deftest exact-index-byte-admission-is-identical-cold-and-warm
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
             :keep-history? true :commit-graph? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (repo/init! conn)
        (let [value (object/utf8 "one object") oid (object/object-id :blob value)]
          (store/import-pack! conn (pack/encode {oid {:type :blob :payload value}}))
          (with-open [objects (store/open conn)]
            (let [baseline (budget/create (store/metadata-budget-policy))]
              (store/read-object objects oid {:import-budget baseline})
              (let [size (alength ^bytes (blob/get-bytes conn
                                                         (d/q '[:find ?payload . :where [_ :geschichte.git.index/payload ?payload]] @conn)))]
                (is (= size (get-in (budget/snapshot baseline) [:used :index-read-bytes])))
                (doseq [warm? [false true] cap [size (dec size)]]
                  (with-open [selected (if warm? (store/open conn {:cache (:cache objects)}) (store/open conn))]
                    (let [operation (budget/create (assoc-in (store/metadata-budget-policy) [:limits :index-read-bytes] cap))
                          result (refusal #(store/read-object selected oid {:import-budget operation}))]
                      (if (= cap size)
                        (do (is (nil? result))
                            (is (= size (get-in (budget/snapshot operation) [:used :index-read-bytes]))))
                        (is (= :index-read-bytes (:metric result)))))))))))
        (finally (d/release conn) (d/delete-database cfg))))))
