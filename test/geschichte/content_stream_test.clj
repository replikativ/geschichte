(ns geschichte.content-stream-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.blob :as blob]
            [datahike.gc-guard :as guard]
            [geschichte.chunk :as chunk]
            [geschichte.content :as content]
            [geschichte.repo :as repo])
  (:import [java.io ByteArrayInputStream IOException]))

(defn- with-connection [f]
  (let [config {:store {:backend :memory :id (random-uuid)}
                :schema-flexibility :write :keep-history? true :commit-graph? true}]
    (d/create-database config)
    (let [conn (d/connect config)]
      (try (repo/init! conn) (f conn)
           (finally (d/release conn) (d/delete-database config))))))

(def opts {:chunk-min-size 128 :chunk-size 257 :chunk-max-size 1024})
(defn- source [value counters]
  (proxy [ByteArrayInputStream] [value]
    (read
      ([] (let [n (proxy-super read)] (when-not (neg? n) (swap! counters update :read inc)) n))
      ([buffer offset length]
       (let [n (proxy-super read buffer offset length)]
         (when (pos? n) (swap! counters update :read + n)) n)))
    (close [] (swap! counters update :closed inc))))

(deftest streaming-identity-size-and-boundaries-match-byte-ingestion
  (with-connection
    (fn [conn]
      (doseq [size [0 1 127 128 1024 10000]]
        (let [value (byte-array (map unchecked-byte (take size (cycle (range 251)))))
              counters (atom {:read 0 :closed 0}) calls (atom [])
              id (content/transact-stream! conn (source value counters) size
                                           (fn [id] (swap! calls conj id) [])
                                           (assoc opts :expected-content (blob/blob-id value)))
              metadata (content/info conn id)]
          (is (= (blob/blob-id value) id))
          (is (= [id] @calls))
          (is (= {:read size :closed 0} @counters))
          (is (= size (:geschichte.content/size metadata)))
          (is (= :chunks (:geschichte.content/kind metadata)))
          (is (= (mapv (fn [[a b]] (- b a)) (chunk/ranges value opts))
                 (mapv :geschichte.chunk/size (sort-by :geschichte.chunk/index (:geschichte.content/chunks metadata)))))
          (is (= (seq value) (seq (content/read-by-id conn id)))))))))

(deftest invalid-size-and-content-never-invoke-publication
  (with-connection
    (fn [conn]
      (doseq [[actual expected expected-id reason]
              [[7 8 nil :geschichte.content/stream-size-mismatch]
               [9 8 nil :geschichte.content/stream-size-mismatch]
               [1 0 nil :geschichte.content/stream-size-mismatch]
               [8 8 (random-uuid) :geschichte.content/stream-content-mismatch]]]
        (let [counters (atom {:read 0 :closed 0}) calls (atom 0)
              failure (try (content/transact-stream! conn (source (byte-array actual) counters) expected
                                                     (fn [_] (swap! calls inc) [])
                                                     (cond-> opts expected-id (assoc :expected-content expected-id)))
                           nil (catch clojure.lang.ExceptionInfo error error))]
          (is (= reason (:type (ex-data failure))))
          (is (zero? @calls))
          (is (<= (:read @counters) (inc expected)))
          (is (zero? (:closed @counters))))))))

(deftest existing-content-still-verifies-the-whole-borrowed-source
  (with-connection
    (fn [conn]
      (let [value (.getBytes "known content" "UTF-8") id (:content (repo/write! conn "known.bin" value))
            counters (atom {:read 0 :closed 0}) calls (atom 0)]
        (is (= id (content/transact-stream! conn (source value counters) (alength value)
                                            (fn [_] (swap! calls inc) []) {:expected-content id})))
        (is (= 1 @calls))
        (is (= (alength value) (:read @counters)))
        (is (zero? (:closed @counters)))))))

(deftest validation-precedes-any-source-read
  (with-connection
    (fn [conn]
      (doseq [[size options] [[-1 nil] [(inc (bigint Long/MAX_VALUE)) nil]
                              [0 {:expected-content nil}] [0 {:expected-content "not-a-uuid"}]
                              [0 {:sync? false}]]]
        (let [counters (atom {:read 0 :closed 0})]
          (is (thrown? clojure.lang.ExceptionInfo
                       (content/transact-stream! conn (source (byte-array 1) counters) size (constantly []) options)))
          (is (= {:read 0 :closed 0} @counters)))))))

(deftest checked-wrapper-preserves-read-contract-and-borrowed-ownership
  (let [counters (atom {:read 0 :closed 0})
        [in counted] (#'content/checked-input-stream (source (byte-array [1 2]) counters) Long/MAX_VALUE)
        buffer (byte-array 4)]
    (is (= 0 (.read in buffer 4 0)))
    (is (thrown? IndexOutOfBoundsException (.read in buffer 4 1)))
    (is (= 2 (.read in buffer)))
    (is (= 2 @counted))
    (is (= -1 (.read in)))
    (is (false? (.markSupported in)))
    (is (thrown? IOException (.skip in 1)))
    (is (thrown? IOException (.reset in)))
    (.close in)
    (is (zero? (:closed @counters)))))

(deftest callback-or-source-failure-does-not-publish-metadata
  (with-connection
    (fn [conn]
      (let [value (.getBytes "not published" "UTF-8") id (blob/blob-id value)
            failure (IOException. "source failed")]
        (is (thrown-with-msg? IOException #"source failed"
                              (content/transact-stream! conn
                                                        (proxy [ByteArrayInputStream] [value]
                                                          (read
                                                            ([] (throw failure))
                                                            ([buffer offset length] (throw failure))))
                                                        (alength value) (fn [_] (throw (AssertionError. "publication"))))))
        (is (nil? (content/info conn id)))
        (is (thrown-with-msg? Exception #"callback failed"
                              (content/transact-stream! conn (ByteArrayInputStream. value) (alength value)
                                                        (fn [_] (throw (ex-info "callback failed" {}))))))
        (is (nil? (content/info conn id)))))))

(deftest every-acquired-guard-is-released-on-refusal-and-callback-failure
  (with-connection
    (fn [conn]
      (let [writing! guard/writing! done! guard/done!
            acquired (atom []) released (atom [])]
        (with-redefs [guard/writing! (fn [sid] (let [token (writing! sid)]
                                                 (swap! acquired conj [sid token]) token))
                      guard/done! (fn [sid token] (swap! released conj [sid token]) (done! sid token))]
          (doseq [operation [(fn [] (content/transact-stream! conn (ByteArrayInputStream. (byte-array 0)) 1 (constantly [])))
                             (fn [] (content/transact-stream! conn (ByteArrayInputStream. (byte-array 1)) 0 (constantly [])))
                             (fn [] (content/transact-stream! conn (ByteArrayInputStream. (byte-array 0)) 0
                                                              (constantly []) {:expected-content (random-uuid)}))
                             (fn [] (content/transact-stream! conn (ByteArrayInputStream. (byte-array 0)) 0
                                                              (fn [_] (throw (ex-info "callback failed" {})))))]]
            (is (thrown? Exception (operation)))))
        (is (= 4 (count @acquired)))
        (is (= (frequencies @acquired) (frequencies @released)))))))
