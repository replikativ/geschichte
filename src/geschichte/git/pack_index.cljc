(ns geschichte.git.pack-index
  "Compact, portable lookup index for exact objects in an immutable Git pack.

  OIDs are sorted for binary search, common offsets use 32 bits, and resolved
  Git types use two bits. Large packs are persisted as first-OID-nibble shards."
  (:refer-clojure :exclude [contains?])
  (:require [geschichte.bytes :as bytes]
            [geschichte.git.object :as object])
  #?(:clj (:import [java.util Arrays])))

(def ^:private header-size 16)
(def ^:private fanout-size (* 256 4))
(def ^:private entries-start (+ header-size fanout-size))
(def ^:private large-offset-flag 2147483648)
(def ^:private u32-base 4294967296)
(def ^:private magic [0x47 0x53 0x49 0x58]) ; GSIX

(def ^:private type->code {:commit 0 :tree 1 :blob 2 :tag 3})
(def ^:private code->type [:commit :tree :blob :tag])

(defn- allocate [size]
  #?(:clj (byte-array (int size))
     :cljs (js/Uint8Array. size)))

(defn- unsigned-byte [buffer position]
  #?(:clj (bit-and 0xff (aget ^bytes buffer (int position)))
     :cljs (aget buffer position)))

(defn- put-byte! [buffer position value]
  #?(:clj (aset-byte ^bytes buffer (int position) (unchecked-byte value))
     :cljs (aset buffer position value)))

(defn- write-u32! [buffer position value]
  (let [value (long value)]
    (put-byte! buffer position (quot value 16777216))
    (put-byte! buffer (inc position) (quot (mod value 16777216) 65536))
    (put-byte! buffer (+ position 2) (quot (mod value 65536) 256))
    (put-byte! buffer (+ position 3) (mod value 256))))

(defn- read-u32 [buffer position]
  (+ (* (unsigned-byte buffer position) 16777216)
     (* (unsigned-byte buffer (inc position)) 65536)
     (* (unsigned-byte buffer (+ position 2)) 256)
     (unsigned-byte buffer (+ position 3))))

(defn- write-u64! [buffer position value]
  (write-u32! buffer position (quot value u32-base))
  (write-u32! buffer (+ position 4) (mod value u32-base)))

(defn- read-u64 [buffer position]
  (+ (* (read-u32 buffer position) u32-base)
     (read-u32 buffer (+ position 4))))

(defn- oid-size-for [object-format]
  (case object-format
    :sha1 20
    :sha256 32
    (throw (ex-info "Unsupported Git object format"
                    {:object-format object-format}))))

(defn- type-bytes [count]
  (quot (+ count 3) 4))

(defn- put-type! [buffer start index type]
  (let [code (or (type->code type)
                 (throw (ex-info "Unsupported resolved Git object type"
                                 {:type type})))
        position (+ start (quot index 4))
        shift (* 2 (mod index 4))]
    (put-byte! buffer position
               (bit-or (unsigned-byte buffer position)
                       (bit-shift-left code shift)))))

(defn- read-type [buffer start index]
  (nth code->type
       (bit-and 3
                (unsigned-bit-shift-right
                 (unsigned-byte buffer (+ start (quot index 4)))
                 (* 2 (mod index 4))))))

(defn encode
  "Encode scanned pack object metadata into a compact lookup index."
  [objects object-format]
  (let [objects (vec (sort-by :oid objects))
        object-count (count objects)
        oid-size (oid-size-for object-format)
        large (filterv #(>= (long (:offset %)) large-offset-flag) objects)
        oids-start entries-start
        offsets-start (+ oids-start (* object-count oid-size))
        types-start (+ offsets-start (* object-count 4))
        large-start (+ types-start (type-bytes object-count))
        result (allocate (+ large-start (* (count large) 8)))
        large-index (volatile! 0)
        fanout #?(:clj (long-array 256)
                  :cljs (js/Uint32Array. 256))]
    (doseq [[index {:keys [oid type offset]}] (map-indexed vector objects)]
      (let [oid-bytes (object/hex->bytes oid)]
        (when-not (= oid-size (bytes/length oid-bytes))
          (throw (ex-info "Git OID has the wrong length"
                          {:oid oid :object-format object-format})))
        (aset fanout (unsigned-byte oid-bytes 0)
              (inc (aget fanout (unsigned-byte oid-bytes 0))))
        (doseq [position (range oid-size)]
          (put-byte! result (+ oids-start (* index oid-size) position)
                     (unsigned-byte oid-bytes position)))
        (if (< (long offset) large-offset-flag)
          (write-u32! result (+ offsets-start (* index 4)) offset)
          (let [slot @large-index]
            (write-u32! result (+ offsets-start (* index 4))
                        (+ large-offset-flag slot))
            (write-u64! result (+ large-start (* slot 8)) offset)
            (vswap! large-index inc)))
        (put-type! result types-start index type)))
    (doseq [[position value] (map-indexed vector magic)]
      (put-byte! result position value))
    (put-byte! result 4 1)
    (put-byte! result 5 oid-size)
    (put-byte! result 6 1) ; two-bit resolved-type table
    (write-u32! result 8 object-count)
    (write-u32! result 12 (count large))
    (loop [index 0 cumulative 0]
      (when (< index 256)
        (let [cumulative (+ cumulative (aget fanout index))]
          (write-u32! result (+ header-size (* index 4)) cumulative)
          (recur (inc index) cumulative))))
    result))

#?(:clj
   (defn- radix-order-from
     [^bytes oid-buffer oid-size ^ints initial first-position]
     (let [object-count (alength initial)
           right (int-array object-count)]
       (loop [position first-position source initial target right]
         (if (neg? position)
           source
           (let [counts (int-array 256)]
             (dotimes [index object-count]
               (let [object-index (aget ^ints source index)
                     value (unsigned-byte oid-buffer
                                          (+ (* object-index oid-size) position))]
                 (aset-int counts value (inc (aget counts value)))))
             (loop [index 0 offset 0]
               (when (< index 256)
                 (let [count (aget counts index)]
                   (aset-int counts index offset)
                   (recur (inc index) (+ offset count)))))
             (dotimes [index object-count]
               (let [object-index (aget ^ints source index)
                     value (unsigned-byte oid-buffer
                                          (+ (* object-index oid-size) position))
                     target-index (aget counts value)]
                 (aset-int target target-index object-index)
                 (aset-int counts value (inc target-index))))
             (recur (dec position) target source)))))))

#?(:clj
   (defn- encode-order
     [^bytes oid-buffer ^longs offsets ^bytes types object-format ^ints order]
     (let [object-count (alength order)
           oid-size (oid-size-for object-format)
           large-count (loop [sorted-index 0 count 0]
                         (if (= sorted-index object-count)
                           count
                           (let [object-index (aget order sorted-index)]
                             (recur (inc sorted-index)
                                    (if (>= (aget offsets object-index)
                                            large-offset-flag)
                                      (inc count) count)))))
           oids-start entries-start
           offsets-start (+ oids-start (* object-count oid-size))
           types-start (+ offsets-start (* object-count 4))
           large-start (+ types-start (type-bytes object-count))
           result (allocate (+ large-start (* large-count 8)))
           large-index (volatile! 0)
           fanout (long-array 256)]
       (dotimes [sorted-index object-count]
         (let [object-index (aget ^ints order sorted-index)
               source-start (* object-index oid-size)
               target-start (+ oids-start (* sorted-index oid-size))
               offset (aget offsets object-index)
               pack-type (bit-and 0xff (aget types object-index))
               type (case pack-type 1 :commit 2 :tree 3 :blob 4 :tag
                          (throw (ex-info "Unresolved Git pack object type"
                                          {:index object-index
                                           :type-code pack-type})))]
           (System/arraycopy oid-buffer source-start result target-start oid-size)
           (aset fanout (unsigned-byte oid-buffer source-start)
                 (inc (aget fanout (unsigned-byte oid-buffer source-start))))
           (if (< offset large-offset-flag)
             (write-u32! result (+ offsets-start (* sorted-index 4)) offset)
             (let [slot @large-index]
               (write-u32! result (+ offsets-start (* sorted-index 4))
                           (+ large-offset-flag slot))
               (write-u64! result (+ large-start (* slot 8)) offset)
               (vswap! large-index inc)))
           (put-type! result types-start sorted-index type)))
       (doseq [[position value] (map-indexed vector magic)]
         (put-byte! result position value))
       (put-byte! result 4 1)
       (put-byte! result 5 oid-size)
       (put-byte! result 6 1)
       (write-u32! result 8 object-count)
       (write-u32! result 12 large-count)
       (loop [index 0 cumulative 0]
         (when (< index 256)
           (let [cumulative (+ cumulative (aget fanout index))]
             (write-u32! result (+ header-size (* index 4)) cumulative)
             (recur (inc index) cumulative))))
       result)))

#?(:clj
   (defn encode-primitive
     "Encode flat primitive scan results without allocating one map per object."
     [^bytes oid-buffer ^longs offsets ^bytes types object-format object-count]
     (let [object-count (int object-count)
           oid-size (oid-size-for object-format)
           initial (int-array object-count)]
       (dotimes [index object-count] (aset-int initial index index))
       (encode-order oid-buffer offsets types object-format
                     (radix-order-from oid-buffer oid-size initial
                                       (dec oid-size))))))

#?(:clj
   (defn encode-primitive-shards
     "Lazily encode one compact shard per populated first-OID-nibble bucket.

     Only one bucket's radix workspace and output bytes exist at a time. The
     returned sequence must be consumed before the primitive scan arrays are
     released."
     [^bytes oid-buffer ^longs offsets ^bytes types object-format object-count]
     (let [object-count (int object-count)
           oid-size (oid-size-for object-format)
           counts (int-array 16)
           grouped (int-array object-count)]
       (dotimes [object-index object-count]
         (let [prefix (unsigned-bit-shift-right
                       (unsigned-byte oid-buffer (* object-index oid-size)) 4)]
           (aset-int counts prefix (inc (aget counts prefix)))))
       (let [starts (int-array 16)
             cursors (int-array 16)]
         (loop [prefix 0 offset 0]
           (when (< prefix 16)
             (aset-int starts prefix offset)
             (aset-int cursors prefix offset)
             (recur (inc prefix) (+ offset (aget counts prefix)))))
         (dotimes [object-index object-count]
           (let [prefix (unsigned-bit-shift-right
                         (unsigned-byte oid-buffer (* object-index oid-size)) 4)
                 position (aget cursors prefix)]
             (aset-int grouped position object-index)
             (aset-int cursors prefix (inc position))))
         (keep
          (fn [prefix]
            (let [count (aget counts prefix)]
              (when (pos? count)
                (let [start (aget starts prefix)
                      encode
                      (fn []
                        (let [initial (Arrays/copyOfRange grouped start
                                                          (+ start count))
                              order (radix-order-from oid-buffer oid-size initial
                                                      (dec oid-size))]
                          (encode-order oid-buffer offsets types object-format
                                        order)))]
                  {:prefix prefix :count count :encode encode}))))
          (range 16))))))

(defn open
  "Validate and parse an encoded index once for repeated lookup."
  [buffer]
  (when (< (bytes/length buffer) entries-start)
    (throw (ex-info "Truncated Geschichte Git pack index" {})))
  (when-not (every? true?
                    (map-indexed #(= %2 (unsigned-byte buffer %1)) magic))
    (throw (ex-info "Invalid Geschichte Git pack index magic" {})))
  (when-not (= 1 (unsigned-byte buffer 4))
    (throw (ex-info "Unsupported Geschichte Git pack index version"
                    {:version (unsigned-byte buffer 4)})))
  (let [oid-size (unsigned-byte buffer 5)
        count (read-u32 buffer 8)
        large-count (read-u32 buffer 12)
        oids-start entries-start
        offsets-start (+ oids-start (* count oid-size))
        types-start (+ offsets-start (* count 4))
        large-start (+ types-start (type-bytes count))
        expected-size (+ large-start (* large-count 8))]
    (when-not (= expected-size (bytes/length buffer))
      (throw (ex-info "Invalid Geschichte Git pack index size"
                      {:expected expected-size
                       :actual (bytes/length buffer)})))
    {:buffer buffer :oid-size oid-size :count count
     :oids-start oids-start :offsets-start offsets-start
     :types-start types-start :large-start large-start}))

(defn- compare-oid [index position target]
  (loop [byte-index 0]
    (if (= byte-index (:oid-size index))
      0
      (let [left (unsigned-byte (:buffer index) (+ position byte-index))
            right (unsigned-byte target byte-index)]
        (if (= left right)
          (recur (inc byte-index))
          (compare left right))))))

(defn lookup
  "Return `{:offset n :type kw}` for OID, or nil when it is not in the pack."
  [index oid]
  (let [target (object/hex->bytes oid)]
    (when (= (:oid-size index) (bytes/length target))
      (let [first-byte (unsigned-byte target 0)
            low (if (zero? first-byte)
                  0
                  (read-u32 (:buffer index)
                            (+ header-size (* (dec first-byte) 4))))
            high (read-u32 (:buffer index)
                           (+ header-size (* first-byte 4)))]
        (loop [low low high (dec high)]
          (when (<= low high)
            (let [middle (quot (+ low high) 2)
                  comparison (compare-oid
                              index
                              (+ (:oids-start index) (* middle (:oid-size index)))
                              target)]
              (cond
                (neg? comparison) (recur (inc middle) high)
                (pos? comparison) (recur low (dec middle))
                :else
                (let [encoded (read-u32
                               (:buffer index)
                               (+ (:offsets-start index) (* middle 4)))
                      offset (if (< encoded large-offset-flag)
                               encoded
                               (read-u64 (:buffer index)
                                         (+ (:large-start index)
                                            (* (- encoded large-offset-flag) 8))))]
                  {:offset offset
                   :type (read-type (:buffer index) (:types-start index)
                                    middle)})))))))))

(defn contains? [index oid]
  (boolean (lookup index oid)))

(defn entries
  "Decode all index entries. Intended for imports and diagnostics, not reads."
  [index]
  (mapv (fn [position]
          (let [oid-start (+ (:oids-start index) (* position (:oid-size index)))
                oid (object/bytes->hex
                     (bytes/slice (:buffer index) oid-start
                                  (+ oid-start (:oid-size index))))
                encoded (read-u32 (:buffer index)
                                  (+ (:offsets-start index) (* position 4)))]
            {:oid oid
             :offset (if (< encoded large-offset-flag)
                       encoded
                       (read-u64 (:buffer index)
                                 (+ (:large-start index)
                                    (* (- encoded large-offset-flag) 8))))
             :type (read-type (:buffer index) (:types-start index) position)}))
        (range (:count index))))
