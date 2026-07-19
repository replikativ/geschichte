(ns geschichte.git-pack-index-test
  (:require [geschichte.bytes :as bytes]
            [geschichte.git.object :as object]
            [geschichte.git.pack-index :as pack-index]
            #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])))

(def objects
  [{:oid "ffffffffffffffffffffffffffffffffffffffff"
    :type :tag :offset 4294967313}
   {:oid "0000000000000000000000000000000000000000"
    :type :commit :offset 12}
   {:oid "80abcdef0123456789abcdef0123456789abcdef"
    :type :blob :offset 2147483647}
   {:oid "7fabcdef0123456789abcdef0123456789abcdef"
    :type :tree :offset 2147483648}])

(deftest compact-index-roundtrip
  (let [encoded (pack-index/encode objects :sha1)
        index (pack-index/open encoded)]
    (testing "the common SHA-1 case costs 24.25 bytes per object plus fanout"
      (is (= (+ 1040 (* 4 20) (* 4 4) 1 (* 2 8))
             (bytes/length encoded))))
    (doseq [{:keys [oid type offset]} objects]
      (is (= {:type type :offset offset}
             (pack-index/lookup index oid))))
    (is (nil? (pack-index/lookup
               index "1111111111111111111111111111111111111111")))
    (is (= (sort-by :oid objects)
           (pack-index/entries index)))))

(deftest sha256-index-roundtrip
  (let [oid (apply str (repeat 64 "a"))
        index (pack-index/open
               (pack-index/encode [{:oid oid :type :blob :offset 42}]
                                  :sha256))]
    (is (= {:type :blob :offset 42} (pack-index/lookup index oid)))
    (is (nil? (pack-index/lookup
               index "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")))))

#?(:clj
   (deftest primitive-shards-preserve-large-offsets
     (let [count (count objects)
           oid-buffer (byte-array (* count 20))
           offsets (long-array (map :offset objects))
           types (byte-array [4 1 3 2])]
       (doseq [[index {:keys [oid]}] (map-indexed vector objects)]
         (System/arraycopy (object/hex->bytes oid) 0 oid-buffer (* index 20) 20))
       (let [indices
             (into {}
                   (map (fn [{:keys [prefix encode]}]
                          [prefix (pack-index/open (encode))]))
                   (pack-index/encode-primitive-shards
                    oid-buffer offsets types :sha1 count))]
         (doseq [{:keys [oid type offset]} objects]
           (is (= {:type type :offset offset}
                  (pack-index/lookup
                   (get indices (Integer/parseInt (subs oid 0 1) 16))
                   oid))))))))
