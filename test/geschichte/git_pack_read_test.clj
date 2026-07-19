(ns geschichte.git-pack-read-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [geschichte.git.object :as object]
            [geschichte.git.pack :as pack]
            [geschichte.git.pack-index :as pack-index]
            [geschichte.git.pack-read :as pack-read]
            [geschichte.git.pack-source :as pack-source]
            [geschichte.git.store :as git-store]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [geschichte.store.blob :as blob])
  (:import [java.io ByteArrayOutputStream]
           [java.nio ByteBuffer ByteOrder]
           [java.security MessageDigest]
           [java.util.zip DeflaterOutputStream]))

(defn- int32 [value]
  (-> (ByteBuffer/allocate 4)
      (.order ByteOrder/BIG_ENDIAN)
      (.putInt (int value))
      .array))

(defn- entry-header [code size]
  (loop [remaining (unsigned-bit-shift-right (long size) 4)
         out [(bit-or (bit-shift-left code 4) (bit-and size 0x0f))]]
    (if (zero? remaining)
      (byte-array (map unchecked-byte out))
      (recur (unsigned-bit-shift-right remaining 7)
             (conj (update out (dec (count out)) #(bit-or % 0x80))
                   (bit-and remaining 0x7f))))))

(defn- deflate-bytes [payload]
  (let [out (ByteArrayOutputStream.)]
    (with-open [stream (DeflaterOutputStream. out)]
      (.write stream ^bytes payload))
    (.toByteArray out)))

(defn- thin-ref-pack [base-oid delta]
  (let [body (object/concat-bytes
              (object/utf8 "PACK") (int32 2) (int32 1)
              (entry-header 7 (alength ^bytes delta))
              (object/hex->bytes base-oid)
              (deflate-bytes delta))]
    (object/concat-bytes
     body (.digest (MessageDigest/getInstance "SHA-1") body))))

(deftest writer-reader-roundtrip
  (let [payloads [(object/utf8 "hello\n")
                  (object/utf8 "a somewhat larger second blob\n")]
        objects (into {}
                      (map (fn [payload]
                             [(object/object-id :blob payload)
                              {:type :blob :payload payload}]))
                      payloads)
        encoded (pack/encode objects)
        decoded (pack-read/scan
                 (pack-source/byte-array-source encoded)
                 {:primitive-index-threshold Integer/MAX_VALUE})]
    (is (= 2 (:version decoded)))
    (is (= (set (keys objects)) (set (map :oid (:objects decoded)))))
    (doseq [[oid {:keys [payload]}] objects
            :let [offset (:offset (some #(when (= oid (:oid %)) %)
                                        (:objects decoded)))
                  resolved (pack-read/resolve-at
                            (pack-source/byte-array-source encoded) offset {})]]
      (is (= (seq payload) (seq (:payload resolved)))))))

(deftest primitive-index-roundtrip
  (let [payloads [(object/utf8 "alpha\n")
                  (object/utf8 "beta\n")
                  (object/utf8 "gamma\n")]
        objects (into {}
                      (map (fn [payload]
                             [(object/object-id :blob payload)
                              {:type :blob :payload payload}]))
                      payloads)
        encoded (pack/encode objects)
        decoded (pack-read/scan
                 (pack-source/byte-array-source encoded)
                 {:primitive-index-threshold 0})
        shards (into {}
                     (map (fn [{:keys [prefix bytes encode]}]
                            [prefix (pack-index/open (or bytes (encode)))]))
                     (:index-shards decoded))]
    (is (= (count objects) (:count decoded)))
    (is (nil? (:objects decoded)))
    (doseq [[oid {:keys [payload]}] objects
            :let [prefix (Integer/parseInt (subs oid 0 1) 16)
                  {:keys [offset type]} (pack-index/lookup (get shards prefix) oid)
                  resolved (pack-read/resolve-at
                            (pack-source/byte-array-source encoded) offset {})]]
      (is (= :blob type))
      (is (= (seq payload) (seq (:payload resolved)))))))

(deftest primitive-thin-pack-resolves-an-external-ref-base
  (let [base (object/utf8 "hello world\n")
        result (object/utf8 "hello brave world\n")
        base-oid (object/object-id :blob base)
        result-oid (object/object-id :blob result)
        ;; base-size=12, result-size=18; COPY 6, INSERT 6, COPY 6@6.
        delta (byte-array
               (concat [12 18 (unchecked-byte 0x90) 6 6]
                       (seq (object/utf8 "brave "))
                       [(unchecked-byte 0x91) 6 6]))
        encoded (thin-ref-pack base-oid delta)
        resolve-ref (fn [oid]
                      (when (= oid base-oid) {:type :blob :payload base}))
        decoded (pack-read/scan
                 (pack-source/byte-array-source encoded)
                 {:primitive-index-threshold 0 :resolve-ref resolve-ref})
        shard (first (:index-shards decoded))
        index (pack-index/open ((:encode shard)))
        {:keys [offset type]} (pack-index/lookup index result-oid)
        resolved (pack-read/resolve-at
                  (pack-source/byte-array-source encoded) offset
                  {:resolve-ref resolve-ref})]
    (is (= {:ref-delta 1} (:storage-types decoded)))
    (is (= :blob type))
    (is (= (seq result) (seq (:payload resolved))))))

(deftest git-copy-insert-delta-instructions
  (let [base (object/utf8 "hello world\n")
        ;; base-size=12, result-size=18; COPY 6, INSERT 6, COPY offset 6 size 6.
        delta (byte-array
               (concat [12 18 (unchecked-byte 0x90) 6 6]
                       (seq (object/utf8 "brave "))
                       [(unchecked-byte 0x91) 6 6]))]
    (is (= "hello brave world\n"
           (String. ^bytes (pack-read/apply-delta base delta) "UTF-8")))
    (testing "declared base size is enforced"
      (is (thrown? clojure.lang.ExceptionInfo
                   (pack-read/apply-delta (object/utf8 "wrong") delta))))))

(deftest checksum-corruption-is-rejected
  (let [payload (object/utf8 "hello\n")
        oid (object/object-id :blob payload)
        encoded (pack/encode {oid {:type :blob :payload payload}})]
    (aset-byte encoded 15 (unchecked-byte (bit-xor 1 (aget encoded 15))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"checksum"
                          (pack-read/scan
                           (pack-source/byte-array-source encoded))))))

(deftest imported-pack-objects-persist-in-datahike
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? true :commit-graph? true}
        payload (object/utf8 "imported\n")
        oid (object/object-id :blob payload)
        encoded (pack/encode {oid {:type :blob :payload payload}})]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (repo/init! conn)
        (is (= 1 (:persisted (git-store/import-pack! conn encoded))))
        (is (= 0 (:persisted (git-store/import-pack! conn encoded))))
        (let [metadata (git-store/object conn oid)]
          (is (uuid? (get-in metadata
                             [:geschichte.git.object/pack
                              :geschichte.git.pack/id])))
          (is (integer? (:geschichte.git.object/offset metadata))))
        (is (= 1 (count (get-in (git-store/object conn oid)
                                [:geschichte.git.object/pack
                                 :geschichte.git.pack/chunks]))))
        (let [manifest (first (query/git-packs @conn))
              shard (first (:geschichte.git.pack/index-shards manifest))
              index-id (:geschichte.git.index/payload shard)
              lookup (fn [store-ref requested-oid]
                       (pack-index/lookup
                        (pack-index/open (blob/get-bytes conn store-ref))
                        requested-oid))]
          (is (uuid? index-id))
          (is (= [12 :blob]
                 (d/q '[:find [?offset ?type]
                        :in $ ?lookup ?index-id ?oid
                        :where
                        [(?lookup ?index-id ?oid) ?metadata]
                        [(get ?metadata :offset) ?offset]
                        [(get ?metadata :type) ?type]]
                      @conn lookup index-id oid)))
          (is (empty? (d/q '[:find ?object
                             :where
                             [?object :geschichte.git.object/oid]]
                           @conn))))
        (is (= (seq payload) (seq (git-store/read-payload conn oid))))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))
