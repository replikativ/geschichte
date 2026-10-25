(ns geschichte.content-range-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [geschichte.bytes :as bytes]
            [geschichte.content :as content]
            [geschichte.repo :as repo]
            [geschichte.store.blob :as payload]))

(defn- with-content [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? true :commit-graph? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (repo/init! conn)
        (let [value (bytes/from-values (take 25000 (cycle (range 251))))
              id (:content (repo/write! conn "recording.bin" value
                                        {:chunk-threshold 1 :chunk-min-size 512
                                         :chunk-size 2048 :chunk-max-size 8192}))]
          (f conn id value))
        (finally (d/release conn) (d/delete-database cfg))))))

(defn- read-range [conn id offset length]
  (let [parts (atom [])]
    (content/consume-range-by-id! conn id offset length #(swap! parts conj %))
    (apply bytes/concat-bytes @parts)))

(deftest sparse-ranges-fetch-only-overlapping-chunks
  (with-content
    (fn [conn id value]
      (let [chunks (sort-by :geschichte.chunk/index (:geschichte.content/chunks (content/info conn id)))
            last-chunk (last chunks)
            reads (atom []) original payload/get-bytes
            offset (:geschichte.chunk/offset last-chunk)
            length (min 100 (:geschichte.chunk/size last-chunk))]
        (with-redefs [payload/get-bytes (fn [& args] (swap! reads conj (second args)) (apply original args))]
          (is (bytes/same-bytes? (bytes/slice value offset (+ offset length))
                                 (read-range conn id offset length)))
          (is (= [(:geschichte.chunk/payload last-chunk)] @reads))
          (reset! reads [])
          (let [offset (- (:geschichte.chunk/size (first chunks)) 3)]
            (is (bytes/same-bytes? (bytes/slice value offset (+ offset 6))
                                   (read-range conn id offset 6)))
            (is (= (mapv :geschichte.chunk/payload (take 2 chunks)) @reads)))
          (reset! reads [])
          (is (zero? (bytes/length (read-range conn id (bytes/length value) 0))))
          (is (empty? @reads)))))))

(deftest invalid-ranges-and-invalid-identity-are-refused
  (with-content
    (fn [conn id value]
      (with-redefs [payload/get-bytes (fn [& _] (throw (AssertionError. "Invalid range read a payload")))]
        (doseq [[offset length] [[-1 1] [0 -1] [0 1.5] [0 (bigint Long/MAX_VALUE)]
                                 [(inc (bytes/length value)) 0] [(bytes/length value) 1]]]
          (is (thrown? clojure.lang.ExceptionInfo (read-range conn id offset length))))
        (is (thrown? clojure.lang.ExceptionInfo (read-range conn nil 0 0)))
        (is (thrown? clojure.lang.ExceptionInfo (read-range conn (random-uuid) 0 0)))))))

(deftest complete-manifest-is-validated-before-the-first-payload
  (with-content
    (fn [conn id _]
      (let [entity (#'content/content-entity @conn id)
            chunks (vec (sort-by :geschichte.chunk/index (:geschichte.content/chunks entity)))
            invalid [(assoc-in chunks [0 :geschichte.chunk/offset] 1)
                     (assoc-in chunks [0 :geschichte.chunk/index] 1)
                     (assoc-in chunks [0 :geschichte.chunk/size] 0)
                     (assoc-in chunks [0 :geschichte.chunk/payload] nil)
                     (assoc-in chunks [(dec (count chunks)) :geschichte.chunk/index] 0)
                     (pop chunks)]]
        (doseq [manifest invalid length [0 1]]
          (with-redefs [content/content-entity (fn [_ _] (assoc entity :geschichte.content/chunks manifest))
                        payload/get-bytes (fn [& _] (throw (AssertionError. "Invalid manifest read a payload")))]
            (is (thrown? clojure.lang.ExceptionInfo (read-range conn id 0 length)))))))))

(deftest early-stop-and-consumer-failure-do-not-read-later-chunks
  (with-content
    (fn [conn id value]
      (let [reads (atom 0) original payload/get-bytes
            failure (ex-info "Consumer failed" {})]
        (with-redefs [payload/get-bytes (fn [& args] (swap! reads inc) (apply original args))]
          (is (true? (content/consume-range-by-id! conn id 0 (bytes/length value)
                                                   (constantly content/stop-consumption))))
          (is (= 1 @reads))
          (reset! reads 0)
          (is (identical? failure (try (content/consume-range-by-id! conn id 0 (bytes/length value)
                                                                     (fn [_] (throw failure)))
                                       (catch Exception error error))))
          (is (= 1 @reads)))))))

(deftest full-content-size-mismatch-cannot-zero-pad-a-range
  (with-content
    (fn [conn _ _]
      (let [value (bytes/utf8 "small text")
            id (:content (repo/write! conn "note.txt" value))
            entity (#'content/content-entity @conn id)]
        (is (bytes/same-bytes? (bytes/utf8 "mall") (read-range conn id 1 4)))
        (with-redefs [content/content-entity (fn [_ _] (update entity :geschichte.content/size inc))]
          (is (thrown? clojure.lang.ExceptionInfo (read-range conn id 0 (inc (bytes/length value))))))))))

(deftest selected-chunk-hash-and-size-are-checked-before-consumption
  (with-content
    (fn [conn id _]
      (let [entity (#'content/content-entity @conn id)
            chunks (vec (sort-by :geschichte.chunk/index (:geschichte.content/chunks entity)))
            consumed (atom 0)]
        (with-redefs [payload/get-bytes (fn [& _] (bytes/utf8 "wrong payload"))]
          (is (thrown? clojure.lang.ExceptionInfo
                       (content/consume-range-by-id! conn id 0 1 (fn [_] (swap! consumed inc)))))
          (is (zero? @consumed)))
        (let [shifted (-> chunks
                          (update-in [0 :geschichte.chunk/size] inc)
                          (update-in [1 :geschichte.chunk/offset] inc)
                          (update-in [1 :geschichte.chunk/size] dec))]
          (with-redefs [content/content-entity
                        (fn [_ _] (assoc entity :geschichte.content/chunks shifted))]
            (is (thrown? clojure.lang.ExceptionInfo
                         (content/consume-range-by-id! conn id 0 1 (fn [_] (swap! consumed inc)))))
            (is (zero? @consumed))))))))

(deftest line-delta-ranges-reconstruct-the-pinned-base-and-check-size
  (with-content
    (fn [conn _ _]
      (let [base (apply str (map #(str "line " % " unchanged padding\n") (range 300)))
            target (.replace base "line 150 unchanged" "line 150 changed")
            base-id (:content (repo/write! conn "delta.txt" (bytes/utf8 base)))
            id (:content (repo/write! conn "delta.txt" (bytes/utf8 target)))
            offset (.indexOf target "line 150 changed")
            original @#'content/content-entity]
        (is (= :line-delta (:geschichte.content/kind (content/info conn id))))
        (is (= base-id (get-in (content/info conn id) [:geschichte.content/base :geschichte.content/id])))
        (is (bytes/same-bytes? (bytes/utf8 (subs target offset (+ offset 16)))
                               (read-range conn id offset 16)))
        (with-redefs [content/content-entity
                      (fn [db selected]
                        (cond-> (original db selected)
                          (= selected id) (update :geschichte.content/size inc)))]
          (is (thrown? clojure.lang.ExceptionInfo
                       (read-range conn id 0 (inc (bytes/length (bytes/utf8 target)))))))))))
