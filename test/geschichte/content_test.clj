(ns geschichte.content-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.blob :as blob]
            [geschichte.chunk :as chunk]
            [geschichte.content :as content]
            [geschichte.repo :as repo])
  (:import [java.io ByteArrayOutputStream File FileOutputStream]))

(defn- fixture []
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write
             :keep-history? true
             :commit-graph? true}]
    (d/create-database cfg)
    {:cfg cfg :conn (d/connect cfg)}))

(defn- cleanup [{:keys [cfg conn]}]
  (d/release conn)
  (d/delete-database cfg))

(defn- utf8 [s] (.getBytes ^String s "UTF-8"))
(defn- text [bytes] (String. ^bytes bytes "UTF-8"))

(deftest text-deltas-are-transparent-and-bounded
  (let [{:keys [conn] :as f} (fixture)
        base (apply str (map #(str "line " % " unchanged padding\n") (range 300)))
        target (.replace base "line 150 unchanged" "line 150 changed")]
    (try
      (repo/init! conn)
      (let [base-id (:content (repo/write! conn "large.txt" (utf8 base)))
            target-id (:content (repo/write! conn "large.txt" (utf8 target)))
            target-info (content/info conn target-id)]
        (is (= :full (:geschichte.content/kind (content/info conn base-id))))
        (is (= :line-delta (:geschichte.content/kind target-info)))
        (is (= base-id
               (get-in target-info
                       [:geschichte.content/base :geschichte.content/id])))
        (is (= target (text (repo/read conn "large.txt"))))
        (is (= base (text (content/read-by-id conn base-id)))))

      (testing "long edit sequences never exceed the configured depth"
        (dotimes [i 12]
          (repo/write! conn "large.txt"
                       (utf8 (str target "revision " i "\n"))))
        (is (<= (apply max
                       (d/q '[:find [?depth ...]
                              :where [_ :geschichte.content/depth ?depth]] @conn))
                8)))
      (finally (cleanup f)))))

(deftest physical-file-cdc-matches-in-memory-boundaries
  (let [{:keys [conn] :as f} (fixture)
        value (byte-array
               (map unchecked-byte
                    (take 100000 (cycle (concat (range 251) [19 83 7])))))
        ^File file (File/createTempFile "geschichte-cdc" ".bin")
        opts {:chunk-threshold 1
              :chunk-min-size 512
              :chunk-size 2048
              :chunk-max-size 8192}]
    (try
      (with-open [out (FileOutputStream. file)]
        (.write out value))
      (repo/init! conn)
      (let [id (:content (repo/write-file! conn "stream.bin" file opts))
            info (content/info conn id)
            actual-sizes (mapv :geschichte.chunk/size
                               (sort-by :geschichte.chunk/index
                                        (:geschichte.content/chunks info)))
            expected-sizes (mapv (fn [[start end]] (- end start))
                                 (chunk/ranges value opts))]
        (is (= (blob/blob-id value) id))
        (is (= expected-sizes actual-sizes))
        (is (= (seq value) (seq (repo/read conn "stream.bin")))))
      (finally
        (.delete file)
        (cleanup f)))))

(deftest binary-content-stays-full
  (let [{:keys [conn] :as f} (fixture)]
    (try
      (repo/init! conn)
      (let [first-id (:content (repo/write! conn "image.bin"
                                            (byte-array [(unchecked-byte 0xff) 0 1 2])))
            second-id (:content (repo/write! conn "image.bin"
                                             (byte-array [(unchecked-byte 0xff) 0 1 3])))]
        (is (= :full (:geschichte.content/kind (content/info conn first-id))))
        (is (= :full (:geschichte.content/kind (content/info conn second-id))))
        (is (= [255 0 1 3]
               (mapv #(bit-and 0xff %) (repo/read conn "image.bin")))))
      (finally (cleanup f)))))

(deftest large-content-is-ordered-and-chunked
  (let [{:keys [conn] :as f} (fixture)
        value (byte-array (map unchecked-byte (take 10000 (cycle (range 251)))))]
    (try
      (repo/init! conn)
      (let [id (:content (repo/write! conn "large.bin" value
                                      {:chunk-threshold 1024
                                       :chunk-min-size 128
                                       :chunk-size 257
                                       :chunk-max-size 1024}))
            info (content/info conn id)
            chunks (sort-by :geschichte.chunk/index
                            (:geschichte.content/chunks info))
            payload-ids (set (map :geschichte.chunk/payload chunks))]
        (is (= :chunks (:geschichte.content/kind info)))
        (is (= :gear-32 (:geschichte.content/chunking-algorithm info)))
        (is (= 1 (:geschichte.content/chunking-version info)))
        (is (= (range (count chunks))
               (map :geschichte.chunk/index chunks)))
        (is (= (butlast (reductions + 0 (map :geschichte.chunk/size chunks)))
               (map :geschichte.chunk/offset chunks)))
        (is (= 10000 (reduce + (map :geschichte.chunk/size chunks))))
        (is (= (count chunks)
               (d/q '[:find (count ?chunk) .
                      :in $ ?id
                      :where
                      [?content :geschichte.content/id ?id]
                      [?content :geschichte.content/chunks ?chunk]]
                    @conn id)))
        (is (= payload-ids
               (set (d/q '[:find [?payload ...]
                           :in $ ?id
                           :where
                           [?content :geschichte.content/id ?id]
                           [?content :geschichte.content/chunks ?chunk]
                           [?chunk :geschichte.chunk/payload ?payload]]
                         @conn id))))
        (is (= (seq value) (seq (repo/read conn "large.bin"))))
        (let [out (ByteArrayOutputStream.)]
          (repo/copy-to! conn "large.bin" out)
          (is (= (seq value) (seq (.toByteArray out)))))
        (let [seen (atom 0)]
          (content/consume-by-id!
           conn id (fn [_]
                     (swap! seen inc)
                     content/stop-consumption))
          (is (= 1 @seen))))
      (finally (cleanup f)))))
