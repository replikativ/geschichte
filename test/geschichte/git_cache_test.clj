(ns geschichte.git-cache-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.git.cache :as cache]
            [geschichte.git.object :as object]
            [geschichte.git.pack :as pack]
            [geschichte.git.store :as store]
            [geschichte.repo :as repo]))

(deftest chunk-caches-are-weighted-and-isolated
  (let [a (cache/create {:chunk-bytes 5})
        b (cache/create {:chunk-bytes 5})
        loads (atom 0)
        load (fn [value]
               #(do (swap! loads inc) (.getBytes ^String value "UTF-8")))]
    (try
      (cache/chunk! a [:store :a] (load "aaa"))
      (cache/chunk! a [:store :a] (load "unused"))
      (is (= 1 @loads))
      (cache/chunk! a [:store :b] (load "bbb"))
      (is (= {:chunk-bytes 3 :chunks 1}
             (select-keys (cache/stats a) [:chunk-bytes :chunks])))
      (is (= 0 (:chunks (cache/stats b))))
      (finally
        (cache/close! a)
        (cache/close! b)))))

(deftest pack-cache-is-bounded-and-close-is-final
  (let [service (cache/create {:packs 2})]
    (cache/pack! service :a #(identity {:id :a}))
    (cache/pack! service :b #(identity {:id :b}))
    (cache/pack! service :c #(identity {:id :c}))
    (is (= 2 (:packs (cache/stats service))))
    (cache/close! service)
    (is (:closed? (cache/stats service)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"closed"
                          (cache/pack! service :d #(identity {:id :d}))))))

(deftest cache-limits-must-be-positive
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"positive"
                        (cache/create {:packs 0}))))

(deftest object-store-supports-concurrent-bounded-reads
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? true :commit-graph? true}
        payload (object/utf8 (apply str (repeat 4096 "abc")))
        oid (object/object-id :blob payload)
        encoded (pack/encode {oid {:type :blob :payload payload}})]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          object-store (store/open conn {:limits {:chunk-bytes 8192
                                                  :resolved-bytes 16384
                                                  :packs 2}})]
      (try
        (repo/init! conn)
        (store/import-pack! object-store encoded)
        (let [reads (doall
                     (for [_ (range 16)]
                       (future
                         (dotimes [_ 25]
                           (is (= (seq payload)
                                  (seq (store/read-payload object-store oid))))))))]
          (doseq [read reads] @read))
        (is (<= (:chunk-bytes (store/cache-stats object-store)) 8192))
        (is (<= (:packs (store/cache-stats object-store)) 2))
        (store/close! object-store)
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"closed"
                              (store/read-object object-store oid)))
        (finally
          (store/close! object-store)
          (d/release conn)
          (d/delete-database cfg))))))

(deftest shared-cache-is-explicit-and-not-owned-by-handles
  (let [service (cache/create)
        cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          a (store/open conn {:cache service})
          b (store/open conn {:cache service})]
      (try
        (store/close! a)
        (is (false? (cache/closed? service)))
        (store/close! b)
        (is (false? (cache/closed? service)))
        (finally
          (cache/close! service)
          (d/release conn)
          (d/delete-database cfg))))))
