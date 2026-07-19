(ns geschichte.chunk-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [clojure.set :as set]
            [datahike.blob :as blob]
            [geschichte.bytes :as bytes]
            [geschichte.chunk :as chunk]))

(def test-options
  {:chunk-min-size 512
   :chunk-size 2048
   :chunk-max-size 8192})

(defn- sample-bytes [length]
  (bytes/from-values
   (map (fn [index]
          (mod (+ (* index 131)
                  (* (quot index 17) 29)
                  (* (quot index 4093) 47))
               256))
        (range length))))

(defn- segmented-cuts [input read-sizes opts]
  (let [compiled (chunk/options opts)
        length (bytes/length input)]
    (loop [position 0
           state chunk/initial-state
           sizes (cycle read-sizes)
           cuts []]
      (if (= position length)
        cuts
        (let [size (min (first sizes) (- length position))
              result (chunk/scan state input position size compiled)]
          (recur (+ position size)
                 (:state result)
                 (next sizes)
                 (into cuts (:cuts result))))))))

(deftest boundaries-ignore-input-delivery
  (let [input (sample-bytes 100000)
        compiled (chunk/options test-options)
        direct (:cuts (chunk/scan chunk/initial-state input 0
                                  (bytes/length input) compiled))]
    (is (= [2302 10494 16493 18669 24652 26828 35020 43212 51404
            59596 61818 63994 66307 68483 76675 82130 84306 90289
            92465]
           direct))
    (is (= direct (segmented-cuts input [1 7 31 1024 3 8192] test-options)))
    (is (= direct (segmented-cuts input [4096] test-options)))
    (is (bytes/same-bytes?
         input
         (apply bytes/concat-bytes (chunk/values input test-options))))))

(deftest insertion-retains-identical-chunks
  (let [base (sample-bytes 500000)
        insertion (bytes/from-values (map #(bit-xor 0xff %)
                                          (range 997)))
        changed (bytes/concat-bytes (bytes/slice base 0 100003)
                                    insertion
                                    (bytes/slice base 100003
                                                 (bytes/length base)))
        base-chunks (chunk/values base test-options)
        changed-chunks (chunk/values changed test-options)
        base-ids (set (map blob/blob-id base-chunks))
        changed-ids (set (map blob/blob-id changed-chunks))
        reused (count (set/intersection base-ids changed-ids))]
    (testing "exact reconstruction remains bit-identical"
      (is (bytes/same-bytes?
           changed (apply bytes/concat-bytes changed-chunks))))
    (testing "boundaries realign after a localized insertion"
      (is (> reused (* 0.8 (count base-ids)))))))
