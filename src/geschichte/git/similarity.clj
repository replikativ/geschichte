(ns geschichte.git.similarity
  "Bounded port of Git's diffcore span-hash similarity estimator."
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def ^:private hash-base 107927)

(defn- u32 [value]
  (bit-and (long value) 0xffffffff))

(defn- text? [^bytes value]
  (not-any? zero? (take (min 8000 (alength value)) value)))

(defn- add-span [counts accum1 accum2 length]
  (let [hash-value (mod (u32 (+ accum1 (* accum2 0x61))) hash-base)]
    (update counts hash-value (fnil + 0) length)))

(defn- span-counts [^bytes value]
  (let [length (alength value)
        text (text? value)]
    (loop [offset 0, span-length 0, accum1 0, accum2 0, counts {}]
      (if (= offset length)
        (if (pos? span-length)
          (add-span counts accum1 accum2 span-length)
          counts)
        (let [character (bit-and 0xff (aget value offset))]
          (if (and text (= character 13) (< (inc offset) length)
                   (= 10 (bit-and 0xff (aget value (inc offset)))))
            (recur (inc offset) span-length accum1 accum2 counts)
            (let [old-accum1 accum1
                  accum1 (u32 (bit-xor (u32 (bit-shift-left (long accum1) 7))
                                       (unsigned-bit-shift-right (long accum2) 25)))
                  accum2 (u32 (bit-xor (u32 (bit-shift-left (long accum2) 7))
                                       (unsigned-bit-shift-right (long old-accum1)
                                                                 25)))
                  accum1 (u32 (+ accum1 character))
                  span-length (inc span-length)]
              (if (or (= span-length 64) (= character 10))
                (recur (inc offset) 0 0 0
                       (add-span counts accum1 accum2 span-length))
                (recur (inc offset) span-length (long accum1) (long accum2)
                       counts)))))))))

(defn percentage
  "Return Git's integer rename similarity percentage for two byte arrays, or
  zero when their size difference cannot meet `minimum` percent."
  [^bytes source ^bytes destination minimum]
  (let [source-size (alength source)
        destination-size (alength destination)
        maximum (max source-size destination-size)
        base (min source-size destination-size)
        delta (- maximum base)]
    (if (or (zero? maximum)
            (< (* maximum (- 100 minimum)) (* delta 100)))
      0
      (let [source-counts (span-counts source)
            destination-counts (span-counts destination)
            copied (reduce-kv
                    (fn [total hash count]
                      (+ total (min count (get destination-counts hash 0))))
                    0 source-counts)]
        (quot (* copied 100) maximum)))))

(defn basename [path]
  (last (str/split path #"/")))
