(ns geschichte.chunk
  "Portable content-defined chunk boundaries over byte arrays.

  The rolling Gear hash is deliberately non-cryptographic: it only chooses
  boundaries. Chunk and whole-file identities remain cryptographic store refs."
  (:require [geschichte.bytes :as bytes]))

(def algorithm :gear-32)
(def version 1)

(def default-options
  {:chunk-min-size (* 1024 1024)
   :chunk-size (* 4 1024 1024)
   :chunk-max-size (* 16 1024 1024)})

(defn- int32 [value]
  #?(:clj (unchecked-int value)
     :cljs (bit-or value 0)))

(defn- xorshift32 [value]
  (let [value (int32 (bit-xor value (bit-shift-left value 13)))
        ;; Clojure promotes bit operations to signed 64-bit longs; mask before
        ;; the logical shift to retain JavaScript's uint32 semantics exactly.
        value (int32
               (bit-xor value
                        (unsigned-bit-shift-right
                         #?(:clj (bit-and 0xffffffff value) :cljs value)
                         17)))]
    (int32 (bit-xor value (bit-shift-left value 5)))))

(def ^:private gear-table
  (int-array
   (take 256
         (rest (iterate xorshift32 (int32 0x9e3779b9))))))

(defn- boundary-bits [target]
  (loop [power 1 bits 0]
    (if (>= power target)
      bits
      (recur (* 2 power) (inc bits)))))

(defn options
  "Validate and compile public chunk sizes into boundary masks."
  [provided]
  (let [{:keys [chunk-min-size chunk-size chunk-max-size] :as opts}
        (merge default-options provided)]
    (when-not (and (pos? chunk-min-size)
                   (<= chunk-min-size chunk-size chunk-max-size)
                   (< chunk-max-size 0x40000000))
      (throw (ex-info "Invalid CDC chunk sizes"
                      (select-keys opts
                                   [:chunk-min-size :chunk-size
                                    :chunk-max-size]))))
    (let [bits (boundary-bits chunk-size)
          early-bits (min 30 (inc bits))
          late-bits (max 1 (dec bits))]
      (assoc opts
             :chunking-algorithm algorithm
             :chunking-version version
             :early-mask (dec (bit-shift-left 1 early-bits))
             :late-mask (dec (bit-shift-left 1 late-bits))))))

(def initial-state {:hash 0 :size 0})

#?(:clj
   (defn scan
     "Scan one byte-array region from detector `state`.

     Returns `{:state next-state :cuts [absolute-end-offset ...]}`. State may be
     passed to the next call, making results independent of input read sizes."
     [state input offset length compiled-options]
     ;; Keep the hot loop entirely in primitive longs. Only the low 32 bits are
     ;; significant, so this is exactly the same recurrence as CLJS int32 math.
     (let [min-size (long (:chunk-min-size compiled-options))
           target-size (long (:chunk-size compiled-options))
           max-size (long (:chunk-max-size compiled-options))
           early-mask (long (:early-mask compiled-options))
           late-mask (long (:late-mask compiled-options))
           end (unchecked-add (long offset) (long length))]
       (loop [position (long offset)
              hash-value (long (bit-and 0xffffffff (long (:hash state))))
              current-size (long (:size state))
              cuts []]
         (if (= position end)
           {:state {:hash hash-value :size current-size}
            :cuts cuts}
           (let [byte-value (bit-and 0xff (aget ^bytes input (int position)))
                 gear-value (long (aget ^ints gear-table (int byte-value)))
                 next-hash (bit-and 0xffffffff
                                    (unchecked-add
                                     (bit-shift-left hash-value 1)
                                     gear-value))
                 next-size (unchecked-inc current-size)
                 mask (if (< next-size target-size) early-mask late-mask)
                 cut? (or (>= next-size max-size)
                          (and (>= next-size min-size)
                               (zero? (bit-and next-hash mask))))
                 next-position (unchecked-inc position)]
             (if cut?
               (recur next-position 0 0 (conj cuts next-position))
               (recur next-position next-hash next-size cuts)))))))

   :cljs
   (defn scan
     "Scan one byte-array region from detector `state`.

     Returns `{:state next-state :cuts [absolute-end-offset ...]}`. State may be
     passed to the next call, making results independent of input read sizes."
     [state input offset length compiled-options]
     (let [{:keys [chunk-min-size chunk-size chunk-max-size
                   early-mask late-mask]} compiled-options
           end (+ offset length)]
       (loop [position offset
              hash-value (int32 (:hash state))
              current-size (:size state)
              cuts []]
         (if (= position end)
           {:state {:hash hash-value :size current-size}
            :cuts cuts}
           (let [byte-value (aget input position)
                 next-hash (int32 (+ (bit-shift-left hash-value 1)
                                     (aget gear-table byte-value)))
                 next-size (inc current-size)
                 mask (if (< next-size chunk-size) early-mask late-mask)
                 cut? (or (>= next-size chunk-max-size)
                          (and (>= next-size chunk-min-size)
                               (zero? (bit-and next-hash mask))))]
             (if cut?
               (recur (inc position) 0 0 (conj cuts (inc position)))
               (recur (inc position) next-hash next-size cuts))))))))

(defn ranges
  "Return exact `[start end]` CDC ranges for an in-memory byte buffer."
  ([input] (ranges input nil))
  ([input provided-options]
   (let [length (bytes/length input)]
     (if (zero? length)
       []
       (let [compiled (options provided-options)
             cuts (:cuts (scan initial-state input 0 length compiled))
             ends (cond-> cuts
                    (not= length (peek cuts)) (conj length))]
         (mapv vector (cons 0 (butlast ends)) ends))))))

(defn values
  "Return exact byte slices in CDC manifest order."
  [input provided-options]
  (mapv (fn [[start end]] (bytes/slice input start end))
        (ranges input provided-options)))
