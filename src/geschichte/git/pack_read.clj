(ns geschichte.git.pack-read
  "Range-based Git pack-v2/v3 scanner and delta resolver."
  (:require [geschichte.git.object :as object]
            [geschichte.git.pack-index :as pack-index]
            [geschichte.git.pack-source :as source])
  (:import [java.io ByteArrayOutputStream File RandomAccessFile]
           [java.nio ByteBuffer ByteOrder]
           [java.security MessageDigest]
           [java.util Arrays Iterator LinkedHashMap Map$Entry]
           [java.util.concurrent Callable ExecutionException Executors Future]
           [java.util.concurrent.atomic AtomicInteger AtomicLong]
           [java.util.zip DataFormatException Inflater]))

(def ^:private code-type
  {1 :commit 2 :tree 3 :blob 4 :tag 6 :ofs-delta 7 :ref-delta})

(defn- resolved-type-code [type]
  (case type :commit 1 :tree 2 :blob 3 :tag 4
        (throw (ex-info "Unsupported resolved Git object type" {:type type}))))

(defn- elapsed-ms [started]
  (/ (double (- (System/nanoTime) started)) 1000000.0))

(defn- phase-start [phase-fn phase data]
  (let [started (System/nanoTime)]
    (when phase-fn
      (phase-fn (merge {:event :start :phase phase} data)))
    started))

(defn- phase-complete [phase-fn phase started data]
  (let [duration-ms (elapsed-ms started)]
    (when phase-fn
      (phase-fn (merge {:event :complete :phase phase
                        :duration-ms duration-ms}
                       data)))
    duration-ms))

(defn- future-value [^Future future]
  (try
    (.get future)
    (catch ExecutionException error
      (throw (.getCause error)))))

(defn- digest-name [object-format]
  (case object-format :sha1 "SHA-1" :sha256 "SHA-256"
        (throw (ex-info "Unsupported Git object format"
                        {:object-format object-format}))))

(defn- checksum-length [object-format]
  (case object-format :sha1 20 :sha256 32
        (throw (ex-info "Unsupported Git object format"
                        {:object-format object-format}))))

(defn- unsigned-byte [^bytes bytes index]
  (bit-and 0xff (aget bytes (int index))))

(defn- ensure-available! [limit position count context]
  (when (> (+ position count) limit)
    (throw (ex-info "Truncated Git pack" {:position position
                                          :needed count
                                          :limit limit
                                          :context context}))))

(defn- read-size-varint [^bytes bytes position limit]
  (loop [position position shift 0 value 0]
    (ensure-available! limit position 1 :delta-size)
    (let [byte (unsigned-byte bytes position)
          value (bit-or value (bit-shift-left (bit-and byte 0x7f) shift))]
      (if (zero? (bit-and byte 0x80))
        [value (inc position)]
        (recur (inc position) (+ shift 7) value)))))

(defn apply-delta
  "Apply Git's byte-oriented COPY/INSERT delta instruction stream."
  ^bytes [^bytes base ^bytes delta]
  (let [limit (alength delta)
        [base-size position] (read-size-varint delta 0 limit)
        [result-size position] (read-size-varint delta position limit)]
    (when-not (= base-size (alength base))
      (throw (ex-info "Git delta base size mismatch"
                      {:declared base-size :actual (alength base)})))
    (let [out (ByteArrayOutputStream. result-size)]
      (loop [position position]
        (if (= position limit)
          (let [result (.toByteArray out)]
            (when-not (= result-size (alength result))
              (throw (ex-info "Git delta result size mismatch"
                              {:declared result-size :actual (alength result)})))
            result)
          (let [opcode (unsigned-byte delta position)
                position (inc position)]
            (cond
              (zero? opcode)
              (throw (ex-info "Reserved zero opcode in Git delta"
                              {:position (dec position)}))

              (zero? (bit-and opcode 0x80))
              (do
                (ensure-available! limit position opcode :delta-insert)
                (.write out delta position opcode)
                (recur (+ position opcode)))

              :else
              (let [[offset position]
                    (loop [bit 0x01 shift 0 value 0 position position]
                      (if (> bit 0x08)
                        [value position]
                        (if (zero? (bit-and opcode bit))
                          (recur (bit-shift-left bit 1) (+ shift 8)
                                 value position)
                          (do
                            (ensure-available! limit position 1 :delta-copy-offset)
                            (recur (bit-shift-left bit 1) (+ shift 8)
                                   (bit-or value
                                           (bit-shift-left
                                            (unsigned-byte delta position) shift))
                                   (inc position))))))
                    [size position]
                    (loop [bit 0x10 shift 0 value 0 position position]
                      (if (> bit 0x40)
                        [(if (zero? value) 0x10000 value) position]
                        (if (zero? (bit-and opcode bit))
                          (recur (bit-shift-left bit 1) (+ shift 8)
                                 value position)
                          (do
                            (ensure-available! limit position 1 :delta-copy-size)
                            (recur (bit-shift-left bit 1) (+ shift 8)
                                   (bit-or value
                                           (bit-shift-left
                                            (unsigned-byte delta position) shift))
                                   (inc position))))))]
                (ensure-available! (alength base) offset size :delta-copy)
                (.write out base offset size)
                (recur position)))))))))

(defn- entry-prefix [pack offset limit object-format]
  (let [available (int (min 80 (- limit offset)))
        _ (ensure-available! limit offset 1 :object-header)
        ^bytes bytes (source/read-range pack offset available)
        first-byte (unsigned-byte bytes 0)
        type-code (bit-and 0x07 (unsigned-bit-shift-right first-byte 4))
        [type size position]
        (loop [position 1 byte first-byte shift 4
               size (bit-and first-byte 0x0f)]
          (if (zero? (bit-and byte 0x80))
            [(or (code-type type-code)
                 (throw (ex-info "Unsupported Git pack object type"
                                 {:type-code type-code :offset offset})))
             size position]
            (do
              (ensure-available! available position 1 :object-size)
              (let [byte (unsigned-byte bytes position)]
                (recur (inc position) byte (+ shift 7)
                       (bit-or size
                               (bit-shift-left (bit-and byte 0x7f) shift)))))))
        [base position]
        (case type
          :ofs-delta
          (let [[distance position]
                (loop [position position
                       byte (unsigned-byte bytes position)
                       distance (bit-and (unsigned-byte bytes position) 0x7f)]
                  (let [position (inc position)]
                    (if (zero? (bit-and byte 0x80))
                      [distance position]
                      (let [byte (unsigned-byte bytes position)]
                        (recur position byte
                               (bit-or (bit-shift-left (inc distance) 7)
                                       (bit-and byte 0x7f)))))))]
            [{:offset (- offset distance)} position])

          :ref-delta
          (let [length (checksum-length object-format)]
            (ensure-available! available position length :ref-delta-base)
            [{:oid (object/bytes->hex
                    (Arrays/copyOfRange bytes (int position)
                                        (int (+ position length))))}
             (+ position length)])

          [nil position])]
    {:offset offset :storage-type type :size (long size) :base base
     :data-offset (+ offset position)}))

(defn- inflate-at
  [pack position limit expected-size retain? digest]
  (let [inflater (Inflater.)
        input-size (* 64 1024)
        output (byte-array (* 64 1024))
        out (when retain? (ByteArrayOutputStream. (int (min expected-size 1048576))))]
    (try
      (loop [cursor (long position) total 0]
        (let [cursor
              (if (.needsInput inflater)
                (do
                  (when (>= cursor limit)
                    (throw (ex-info "Truncated Git pack zlib stream"
                                    {:position position})))
                  (let [{:keys [bytes offset length]}
                        (source/read-window pack cursor
                                            (min input-size (- limit cursor)))]
                    (.setInput inflater ^bytes bytes (int offset) (int length))
                    (+ cursor length)))
                cursor)
              n (.inflate inflater output)]
          (when (pos? n)
            (when digest (.update ^MessageDigest digest output 0 n))
            (when out (.write ^ByteArrayOutputStream out output 0 n)))
          (let [total (+ total n)]
            (cond
              (.finished inflater)
              (do
                (when-not (= expected-size total)
                  (throw (ex-info "Git pack object size mismatch"
                                  {:declared expected-size :actual total
                                   :position position})))
                {:payload (when out (.toByteArray ^ByteArrayOutputStream out))
                 :end (- cursor (.getRemaining inflater))})

              (.needsDictionary inflater)
              (throw (ex-info "Git pack zlib stream needs a dictionary"
                              {:position position}))

              (and (zero? n) (.needsInput inflater))
              (recur (long cursor) total)

              (zero? n)
              (throw (ex-info "Stalled Git pack zlib stream"
                              {:position position}))

              :else (recur (long cursor) total)))))
      (catch DataFormatException error
        (throw (ex-info "Invalid Git pack zlib stream"
                        {:position position} error)))
      (finally (.end inflater)))))

(defn- object-digest [object-format type size]
  (doto (MessageDigest/getInstance (digest-name object-format))
    (.update ^bytes (object/utf8 (str (name type) " " size "\u0000")))))

(defn- digest-range [pack start end object-format]
  (let [digest (MessageDigest/getInstance (digest-name object-format))]
    (loop [position (long start)]
      (if (= position end)
        (.digest digest)
        (let [length (int (min (* 1024 1024) (- end position)))
              ^bytes value (source/read-range pack position length)]
          (.update digest value 0 length)
          (recur (+ position length)))))))

(defn- byte-cache [limit]
  ;; A byte budget alone is insufficient for repositories dominated by tiny
  ;; trees and commits: the LinkedHashMap nodes can otherwise cost more than
  ;; their payloads. Bound both retained payload bytes and entry count.
  {:values (LinkedHashMap. 16 0.75 true)
   :weight (atom 0) :limit limit :entry-limit 65536})

(defn resolution-cache
  "Create a byte-budgeted LRU for lazily reconstructed pack objects."
  ([] (resolution-cache (* 96 1024 1024)))
  ([limit] (byte-cache limit)))

(defn- cache-value [cache key]
  ;; Access-order LinkedHashMap.get mutates the map. Resolution caches may be
  ;; shared by concurrent readers, so both reads and weighted writes use the
  ;; same monitor.
  (locking (:values cache)
    (.get ^LinkedHashMap (:values cache) key)))

(defn- cache-put! [cache key value]
  (let [^bytes payload (if (bytes? value) value (:payload value))
        size (alength payload)]
    (locking (:values cache)
      (when (<= size (:limit cache))
        (when-let [previous (.put ^LinkedHashMap (:values cache) key value)]
          (let [^bytes previous-payload (if (bytes? previous)
                                          previous (:payload previous))]
            (swap! (:weight cache) - (alength previous-payload))))
        (swap! (:weight cache) + size)
        (while (or (> @(:weight cache) (:limit cache))
                   (> (.size ^LinkedHashMap (:values cache))
                      (:entry-limit cache)))
          (let [^Iterator iterator (.iterator (.entrySet ^LinkedHashMap (:values cache)))
                ^Map$Entry entry (.next iterator)
                evicted (.getValue entry)
                ^bytes evicted-payload (if (bytes? evicted)
                                         evicted (:payload evicted))]
            (.remove iterator)
            (swap! (:weight cache) - (alength evicted-payload))))))
    value))

(defn- find-offset-index [^longs offsets high target]
  (loop [low 0 high (dec (int high))]
    (when (<= low high)
      (let [middle (unsigned-bit-shift-right (+ low high) 1)
            value (aget offsets middle)]
        (cond
          (< value target) (recur (inc middle) high)
          (> value target) (recur low (dec middle))
          :else middle)))))

(defn- oid-segment= [^bytes left left-offset ^bytes right right-offset length]
  (loop [index 0]
    (or (= index length)
        (and (= (aget left (+ left-offset index))
                (aget right (+ right-offset index)))
             (recur (inc index))))))

(defn- oid-hash [^bytes buffer offset length]
  (loop [index 0 hash (long 2166136261)]
    (if (= index length)
      hash
      (recur (inc index)
             (unchecked-multiply
              (bit-xor hash (long (unsigned-byte buffer (+ offset index))))
              (long 16777619))))))

(defn- ref-table [^bytes ref-bases oid-size ref-count]
  (when (pos? ref-count)
    (let [wanted (* 2 (long ref-count))
          capacity (loop [capacity 16]
                     (if (>= capacity wanted) capacity
                         (recur (* 2 capacity))))
          _ (when (> capacity Integer/MAX_VALUE)
              (throw (ex-info "Git REF_DELTA table is too large"
                              {:ref-deltas ref-count})))
          slots (int-array (int capacity))
          values (int-array (int capacity))
          counts (int-array (int capacity))
          mask (dec (int capacity))]
      (Arrays/fill slots -1)
      (Arrays/fill values -1)
      (dotimes [ref-index ref-count]
        (let [offset (* ref-index oid-size)
              start (bit-and (oid-hash ref-bases offset oid-size) mask)]
          (loop [slot (int start)]
            (let [present (aget slots slot)]
              (cond
                (= -1 present)
                (do (aset-int slots slot ref-index)
                    (aset-int counts slot 1))
                (oid-segment= ref-bases (* present oid-size)
                              ref-bases offset oid-size)
                (aset-int counts slot (inc (aget counts slot)))
                :else (recur (bit-and (inc slot) mask)))))))
      {:slots slots :values values :counts counts :mask mask :keys ref-bases
       :oid-size oid-size})))

(defn- ref-slot [table ^bytes buffer offset]
  (when table
    (let [{:keys [^ints slots mask ^bytes keys oid-size]} table
          start (bit-and (oid-hash buffer offset oid-size) mask)]
      (loop [slot (int start)]
        (let [present (aget slots slot)]
          (cond
            (= -1 present) nil
            (oid-segment= keys (* present oid-size) buffer offset oid-size) slot
            :else (recur (bit-and (inc slot) mask))))))))

(defn- publish-ref-base!
  [table ^bytes oid-buffer oid-offset object-index ^ints dependent-counts]
  (when-let [slot (ref-slot table oid-buffer oid-offset)]
    (let [^ints values (:values table)]
      (when (= -1 (aget values slot))
        (aset-int values slot (int object-index))
        (aset-int dependent-counts object-index
                  (+ (aget dependent-counts object-index)
                     (aget ^ints (:counts table) slot)))))))

(defn- ref-base-index [table ^bytes ref-bases ref-index oid-size]
  (when-let [slot (ref-slot table ref-bases (* ref-index oid-size))]
    (let [value (aget ^ints (:values table) slot)]
      (when-not (= -1 value) value))))

(defn- resolved-oid! [^bytes target object-index oid-size ^bytes oid]
  (System/arraycopy oid 0 target (* object-index oid-size) oid-size))

(defn- reverse-int-chain! [^ints heads ^ints next head-index]
  (loop [current (aget heads head-index) previous -1]
    (if (= -1 current)
      (aset-int heads head-index previous)
      (let [following (aget next current)]
        (aset-int next current previous)
        (recur following current)))))

(defn- payload-oid [object-format type ^bytes payload]
  (let [digest (object-digest object-format type (alength payload))]
    (.update ^MessageDigest digest payload 0 (alength payload))
    (.digest ^MessageDigest digest)))

(defn- scan-primitive
  [pack {:keys [object-format resolve-ref delta-cache-bytes max-pack-objects
                delta-frontier-bytes delta-spill-directory
                max-pack-index-memory-bytes delta-parallelism phase-fn]
         :or {object-format :sha1 delta-cache-bytes (* 96 1024 1024)
              delta-frontier-bytes (* 256 1024 1024)
              max-pack-index-memory-bytes (* 1024 1024 1024)
              delta-parallelism 4}}
   version object-count limit expected length]
  (when (and max-pack-objects (> object-count max-pack-objects))
    (throw (ex-info "Git pack exceeds configured object limit"
                    {:objects object-count :limit max-pack-objects})))
  (let [estimated-index-bytes (* 58 (long object-count))]
    (when (and max-pack-index-memory-bytes
               (> estimated-index-bytes max-pack-index-memory-bytes))
      (throw (ex-info "Git pack compact-index workspace exceeds configured memory budget"
                      {:objects object-count
                       :estimated-bytes estimated-index-bytes
                       :limit max-pack-index-memory-bytes
                       :option :max-pack-index-memory-bytes}))))
  (let [oid-size (checksum-length object-format)
        offsets (long-array object-count)
        base-indices (int-array object-count)
        ref-state (volatile! {:bytes (byte-array (* 1024 oid-size)) :count 0})
        oid-buffer (byte-array (* object-count oid-size))
        storage-types (byte-array object-count)
        resolved-types (byte-array object-count)
        child-heads (int-array object-count)
        child-next (int-array object-count)
        dependent-counts (int-array object-count)
        retained-payloads (object-array object-count)
        frontier-weight (volatile! 0)
        frontier-peak (volatile! 0)
        spill-state (volatile! nil)
        spill-live (volatile! 0)
        spill-peak (volatile! 0)
        retention-lock (Object.)
        frequencies (long-array 8)
        discovery-start (phase-start phase-fn :discover-objects
                                     {:objects object-count
                                      :pack-bytes length})]
    (Arrays/fill base-indices -1)
    (Arrays/fill child-heads -1)
    (Arrays/fill child-next -1)
    (let [{:keys [position pending-count ref-count]}
          (loop [index 0 position 12 pending-count 0 ref-count 0]
            (if (= index object-count)
              {:position position :pending-count pending-count
               :ref-count ref-count}
              (let [{:keys [storage-type type size data-offset base] :as entry}
                    (entry-prefix pack position limit object-format)
                    code (case storage-type
                           :commit 1 :tree 2 :blob 3 :tag 4
                           :ofs-delta 6 :ref-delta 7)
                    object-type (when (<= code 4) storage-type)
                    digest (when object-type
                             (object-digest object-format object-type size))
                    inflated (inflate-at pack data-offset limit size false digest)
                    end (long (:end inflated))]
                (aset-long offsets index (long position))
                (aset-byte storage-types index (unchecked-byte code))
                (aset-long frequencies code (inc (aget frequencies code)))
                (cond
                  object-type
                  (do (aset-byte resolved-types index (unchecked-byte code))
                      (resolved-oid! oid-buffer index oid-size
                                     (.digest ^MessageDigest digest)))

                  (= code 6)
                  (let [base-index (find-offset-index offsets index
                                                      (:offset base))]
                    (when-not (some? base-index)
                      (throw (ex-info "Missing OFS_DELTA base offset"
                                      {:offset position :base base})))
                    (aset-int base-indices index base-index)
                    (aset-int dependent-counts base-index
                              (inc (aget dependent-counts base-index)))
                    (aset-int child-next index (aget child-heads base-index))
                    (aset-int child-heads base-index index))

                  :else
                  (let [{:keys [^bytes bytes count]} @ref-state
                        required (* (inc count) oid-size)
                        bytes (if (<= required (alength bytes))
                                bytes
                                (Arrays/copyOf
                                 bytes (int (max required (* 2 (alength bytes))))))]
                    (System/arraycopy ^bytes (object/hex->bytes (:oid base)) 0
                                      bytes (* count oid-size) oid-size)
                    (vreset! ref-state {:bytes bytes :count (inc count)})
                    (aset-int base-indices index count)))
                (recur (inc index) end
                       (if object-type pending-count (inc pending-count))
                       (if (= code 7) (inc ref-count) ref-count)))))
          discovery-ms (phase-complete phase-fn :discover-objects
                                       discovery-start
                                       {:objects object-count
                                        :pending-deltas pending-count})]
      (when-not (= position limit)
        (throw (ex-info "Trailing data before Git pack checksum"
                        {:position position :limit limit})))
      (let [ref-bases (Arrays/copyOf ^bytes (:bytes @ref-state)
                                     (int (* ref-count oid-size)))
            refs (ref-table ref-bases oid-size ref-count)
            ref-child-heads (int-array (if refs
                                         (alength ^ints (:values refs))
                                         0))]
        (Arrays/fill ref-child-heads -1)
        (dotimes [index object-count]
          (when (pos? (bit-and 0xff (aget resolved-types index)))
            (publish-ref-base! refs oid-buffer (* index oid-size) index
                               dependent-counts)))
        (dotimes [index object-count]
          (when (= 7 (bit-and 0xff (aget storage-types index)))
            (let [ref-index (aget base-indices index)
                  slot (or (ref-slot refs ref-bases (* ref-index oid-size))
                           (throw (ex-info "Missing REF_DELTA base slot"
                                           {:index index
                                            :ref-index ref-index})))]
              (aset-int child-next index (aget ref-child-heads slot))
              (aset-int ref-child-heads slot index))))
        ;; Discovery prepends without allocation; reverse once so subtree reads
        ;; follow pack order and preserve stored-chunk locality.
        (dotimes [index object-count]
          (reverse-int-chain! child-heads child-next index))
        (dotimes [slot (alength ref-child-heads)]
          (reverse-int-chain! ref-child-heads child-next slot))
        (letfn [(spill-file []
                  (or @spill-state
                      (let [directory (when delta-spill-directory
                                        (File. (str delta-spill-directory)))
                            _ (when directory (.mkdirs directory))
                            file (File/createTempFile "geschichte-delta-" ".spill"
                                                      directory)
                            state {:file file
                                   :raf (RandomAccessFile. file "rw")
                                   ;; Power-of-two free lists avoid one boxed
                                   ;; offset/map node per spilled object.
                                   :free-offsets (object-array 32)
                                   :free-counts (int-array 32)}]
                        (vreset! spill-state state)
                        state)))

                (spill-size-class [size]
                  (loop [index 0 capacity 1]
                    (if (>= capacity size)
                      [index capacity]
                      (recur (inc index) (* 2 capacity)))))

                (spill-slot! [size]
                  (let [{:keys [^RandomAccessFile raf ^objects free-offsets
                                ^ints free-counts]}
                        (spill-file)
                        [index capacity] (spill-size-class size)
                        count (aget free-counts index)]
                    (if (pos? count)
                      (let [^longs offsets (aget free-offsets index)
                            next-count (dec count)
                            offset (aget offsets next-count)]
                        (aset-int free-counts index next-count)
                        [raf offset index capacity])
                      (let [offset (.length raf)]
                        (.setLength raf (+ offset capacity))
                        [raf offset index capacity]))))

                (release-spill! [^longs location]
                  (let [{:keys [^objects free-offsets ^ints free-counts]}
                        @spill-state
                        offset (aget location 0)
                        length (aget location 1)
                        index (int (aget location 2))
                        count (aget free-counts index)
                        ^longs current (aget free-offsets index)
                        offsets (if (and current (< count (alength current)))
                                  current
                                  (let [grown (if current
                                                (Arrays/copyOf
                                                 current
                                                 (max 16 (* 2 (alength current))))
                                                (long-array 16))]
                                    (aset free-offsets index grown)
                                    grown))]
                    (aset-long offsets count offset)
                    (aset-int free-counts index (inc count))
                    (vswap! spill-live - length)))

                (retain! [object-index ^bytes payload]
                  (locking retention-lock
                    (if (<= (+ (long @frontier-weight) (alength payload))
                            (long delta-frontier-bytes))
                      (do (aset retained-payloads object-index payload)
                          (vswap! frontier-weight + (alength payload))
                          (vswap! frontier-peak max @frontier-weight))
                      (let [[^RandomAccessFile raf offset class _capacity]
                            (spill-slot! (alength payload))]
                        (.seek raf offset)
                        (.write raf payload)
                        (vswap! spill-live + (alength payload))
                        (vswap! spill-peak max @spill-live)
                        (aset retained-payloads object-index
                              (long-array [offset (alength payload) class])))))
                  payload)

                (retained [object-index]
                  (locking retention-lock
                    (let [value (aget ^objects retained-payloads object-index)]
                      (if (bytes? value)
                        value
                        (when value
                          (let [^longs location value
                                payload (byte-array (int (aget location 1)))
                                ^RandomAccessFile raf (:raf @spill-state)]
                            (.seek raf (aget location 0))
                            (.readFully raf payload)
                            payload))))))

                (base-payload [base-index]
                  (or (retained base-index)
                      (let [code (bit-and 0xff (aget storage-types base-index))]
                        (when (> code 4)
                          (throw (ex-info "Resolved delta base payload was released early"
                                          {:index base-index :storage-type code
                                           :dependents (aget dependent-counts
                                                             base-index)})))
                        (let [{:keys [data-offset size]}
                              (entry-prefix pack (aget offsets base-index)
                                            limit object-format)
                              payload (:payload
                                       (inflate-at pack data-offset limit size
                                                   true nil))]
                          (retain! base-index payload)))))

                (release-base! [base-index]
                  (locking retention-lock
                    (let [remaining (dec (aget dependent-counts base-index))]
                      (when (neg? remaining)
                        (throw (ex-info "Git pack base dependency underflow"
                                        {:index base-index})))
                      (aset-int dependent-counts base-index remaining)
                      (when (zero? remaining)
                        (when-let [value (aget ^objects retained-payloads base-index)]
                          (if (bytes? value)
                            (vswap! frontier-weight - (alength ^bytes value))
                            (release-spill! value)))
                        (aset retained-payloads base-index nil)))))]
          (try
            (let [parallelism (max 1 (int delta-parallelism))
                  root-cursor (AtomicInteger. 0)
                  resolved-progress (AtomicLong. 0)
                  ref-locks (object-array 256)
                  _ (dotimes [index (alength ref-locks)]
                      (aset ref-locks index (Object.)))
                  executor (when (> parallelism 1)
                             (Executors/newFixedThreadPool parallelism))
                  resolution-start
                  (phase-start phase-fn :resolve-deltas
                               {:deltas pending-count
                                :parallelism parallelism})]
              (try
                (letfn [(base-index-of [object-index]
                          (let [code (bit-and
                                      0xff
                                      (aget storage-types object-index))]
                            (if (= code 6)
                              (aget base-indices object-index)
                              (ref-base-index
                               refs ref-bases
                               (aget base-indices object-index)
                               oid-size))))

                        (ref-lock [slot]
                          (aget ref-locks (bit-and (int slot)
                                                   (dec (alength ref-locks)))))

                        (publish-resolved-ref-base! [object-index]
                          (when-let [slot (ref-slot
                                           refs oid-buffer
                                           (* object-index oid-size))]
                            (locking (ref-lock slot)
                              (let [^ints values (:values refs)]
                                (when (= -1 (aget values slot))
                                  (aset-int values slot object-index)
                                  (aset-int
                                   dependent-counts object-index
                                   (+ (aget dependent-counts object-index)
                                      (aget ^ints (:counts refs) slot))))))))

                        (claim-ref-head! [slot]
                          (locking (ref-lock slot)
                            (let [head (aget ref-child-heads slot)]
                              (aset-int ref-child-heads slot -1)
                              head)))

                        (job [object-index base-index external-base]
                          (let [base (if (some? base-index)
                                       (base-payload base-index)
                                       (:payload external-base))
                                type-code (if (some? base-index)
                                            (bit-and
                                             0xff
                                             (aget resolved-types base-index))
                                            (resolved-type-code
                                             (:type external-base)))
                                type (code-type type-code)]
                            (fn []
                              (let [{:keys [data-offset size]}
                                    (entry-prefix
                                     pack (aget offsets object-index)
                                     limit object-format)
                                    delta
                                    (:payload
                                     (inflate-at pack data-offset limit
                                                 size true nil))
                                    payload (apply-delta base delta)]
                                {:object-index object-index
                                 :base-index base-index
                                 :type-code type-code
                                 :payload payload
                                 :oid (payload-oid object-format
                                                   type payload)}))))

                        (run-jobs! [jobs]
                          (let [results (mapv #(%1) jobs)]
                            (doseq [{:keys [object-index base-index
                                            type-code payload oid]}
                                    results]
                              (aset-byte resolved-types object-index
                                         (unchecked-byte type-code))
                              (resolved-oid! oid-buffer object-index
                                             oid-size oid)
                              (publish-resolved-ref-base! object-index)
                              (when (pos? (aget dependent-counts object-index))
                                (retain! object-index payload))
                              (when (some? base-index)
                                (release-base! base-index)))
                            (let [completed (.addAndGet
                                             resolved-progress
                                             (long (count results)))
                                  previous (- completed (count results))]
                              (when (and phase-fn
                                         (or (= completed pending-count)
                                             (not= (quot previous 250000)
                                                   (quot completed 250000))))
                                (phase-fn
                                 {:event :progress :phase :resolve-deltas
                                  :completed completed :total pending-count
                                  :round 1})))
                            results))

                        (process-chain! [head depth]
                          (loop [cursor head resolved-count 0]
                            (if (= -1 cursor)
                              resolved-count
                              (let [[next-cursor jobs]
                                    (loop [current cursor jobs []]
                                      (if (or (= -1 current)
                                              (= 1 (count jobs)))
                                        [current jobs]
                                        (let [following
                                              (aget child-next current)]
                                          (if (pos? (bit-and
                                                     0xff
                                                     (aget resolved-types
                                                           current)))
                                            (recur following jobs)
                                            (let [base-index
                                                  (base-index-of current)]
                                              (when-not
                                               (and (some? base-index)
                                                    (pos?
                                                     (bit-and
                                                      0xff
                                                      (aget resolved-types
                                                            base-index))))
                                                (throw
                                                 (ex-info
                                                  "Delta child was scheduled before its base"
                                                  {:index current
                                                   :base-index base-index})))
                                              (recur following
                                                     (conj jobs
                                                           (job current
                                                                base-index
                                                                nil))))))))
                                    results (when (seq jobs)
                                              (run-jobs! jobs))
                                    descendants
                                    (reduce
                                     (fn [count {:keys [object-index]}]
                                       (+ count
                                          (process-object-children!
                                           object-index (inc depth))))
                                     0 results)
                                    completed (+ (count results) descendants)]
                                (recur next-cursor
                                       (+ resolved-count completed))))))

                        (process-object-children! [object-index depth]
                          (when (> depth 128)
                            (throw
                             (ex-info "Git pack delta chain is too deep"
                                      {:index object-index :depth depth})))
                          (let [ofs-count
                                (process-chain!
                                 (aget child-heads object-index) depth)
                                slot (ref-slot refs oid-buffer
                                               (* object-index oid-size))
                                ref-count
                                (if (some? slot)
                                  (process-chain!
                                   (claim-ref-head! slot) depth)
                                  0)]
                            (+ ofs-count ref-count)))

                        (process-external-chain! [head base depth]
                          (loop [cursor head resolved-count 0]
                            (if (= -1 cursor)
                              resolved-count
                              (let [following (aget child-next cursor)]
                                (if (pos? (bit-and
                                           0xff
                                           (aget resolved-types cursor)))
                                  (recur following resolved-count)
                                  (let [results
                                        (run-jobs! [(job cursor nil base)])
                                        descendants
                                        (process-object-children!
                                         cursor (inc depth))]
                                    (recur following
                                           (+ resolved-count
                                              (count results)
                                              descendants))))))))

                        (process-external-bases! []
                          (if-not refs
                            0
                            (let [^ints slots (:slots refs)
                                  ^ints values (:values refs)
                                  ^bytes keys (:keys refs)]
                              (loop [slot 0 resolved-count 0]
                                (if (= slot (alength values))
                                  resolved-count
                                  (let [head (when (= -1 (aget values slot))
                                               (claim-ref-head! slot))]
                                    (if (or (nil? head) (= -1 head))
                                      (recur (inc slot) resolved-count)
                                      (let [ref-index (aget slots slot)
                                            oid (object/bytes->hex
                                                 (Arrays/copyOfRange
                                                  keys (* ref-index oid-size)
                                                  (* (inc ref-index) oid-size)))
                                            base (when resolve-ref
                                                   (resolve-ref oid))]
                                        (when-not base
                                          (throw
                                           (ex-info
                                            "Unresolved thin pack requires an external REF_DELTA base"
                                            {:base oid :slot slot})))
                                        (recur
                                         (inc slot)
                                         (+ resolved-count
                                            (process-external-chain!
                                             head base 0)))))))))))

                        (process-roots! []
                          (loop [resolved-count 0]
                            (let [index (.getAndIncrement root-cursor)]
                              (if (>= index object-count)
                                resolved-count
                                (let [code (bit-and
                                            0xff
                                            (aget storage-types index))]
                                  (recur
                                   (if (<= code 4)
                                     (+ resolved-count
                                        (process-object-children! index 0))
                                     resolved-count)))))))]
                  (let [resolved-count
                        (+ (if executor
                             (let [workers
                                   (mapv
                                    (fn [_]
                                      (.submit
                                       executor
                                       ^Callable
                                       (reify Callable
                                         (call [_] (process-roots!)))))
                                    (range parallelism))]
                               (reduce + (map future-value workers)))
                             (process-roots!))
                           (process-external-bases!))
                        unresolved
                        (loop [index 0 count 0 sample []]
                          (if (= index object-count)
                            {:count count :sample sample}
                            (if (and (> (bit-and
                                         0xff
                                         (aget storage-types index))
                                        4)
                                     (zero? (bit-and
                                             0xff
                                             (aget resolved-types index))))
                              (recur (inc index) (inc count)
                                     (if (< (count sample) 32)
                                       (conj sample index)
                                       sample))
                              (recur (inc index) count sample))))
                        _ (when (pos? (:count unresolved))
                            (throw
                             (ex-info
                              "Unresolved thin pack requires external large-pack bases"
                              {:remaining (:count unresolved)
                               :sample (:sample unresolved)
                               :resolve-ref? (boolean resolve-ref)})))
                        _ (when-not (= resolved-count pending-count)
                            (throw
                             (ex-info "Resolved Git delta count mismatch"
                                      {:expected pending-count
                                       :actual resolved-count})))
                        resolution-ms
                        (phase-complete phase-fn :resolve-deltas
                                        resolution-start
                                        {:deltas pending-count
                                         :rounds 1
                                         :parallelism parallelism})]
                    {:version version :count object-count :size length
                     :checksum (object/bytes->hex expected)
                     :storage-types
                     (into {}
                           (keep (fn [[code type]]
                                   (let [count (aget frequencies code)]
                                     (when (pos? count) [type count]))))
                           code-type)
                     :delta-frontier-peak-bytes @frontier-peak
                     :delta-spilled? (boolean @spill-state)
                     :delta-spill-peak-live-bytes @spill-peak
                     :delta-spill-file-bytes
                     (if-let [{:keys [^RandomAccessFile raf]} @spill-state]
                       (.length raf)
                       0)
                     :delta-parallelism parallelism
                     :timings-ms {:discover-objects discovery-ms
                                  :resolve-deltas resolution-ms}
                     :index-shards
                     (pack-index/encode-primitive-shards
                      oid-buffer offsets resolved-types
                      object-format object-count)}))
                (finally
                  (when executor (.shutdownNow executor)))))
            (finally
              (when-let [{:keys [^RandomAccessFile raf ^File file]} @spill-state]
                (.close raf)
                (.delete file)))))))))

(defn- resolve-entries
  [pack entries limit object-format resolve-ref cache-bytes]
  (let [by-offset (into {} (map (juxt :offset identity)) entries)
        resolved (atom (into {}
                             (keep (fn [{:keys [offset oid type]}]
                                     (when oid [offset {:oid oid :type type}])))
                             entries))
        oid-offset (atom (into {}
                               (keep (fn [{:keys [offset oid]}]
                                       (when oid [oid offset])))
                               entries))
        external (atom {})
        cache (byte-cache cache-bytes)]
    (letfn [(materialize [offset depth]
              (when (> depth 64)
                (throw (ex-info "Git pack delta chain is too deep"
                                {:offset offset :max-depth 64})))
              (or (cache-value cache offset)
                  (let [{:keys [storage-type base data-offset size] :as entry}
                        (get by-offset offset)
                        result
                        (if (#{:ofs-delta :ref-delta} storage-type)
                          (let [base-value
                                (case storage-type
                                  :ofs-delta (materialize (:offset base) (inc depth))
                                  :ref-delta
                                  (if-let [base-offset (get @oid-offset (:oid base))]
                                    (materialize base-offset (inc depth))
                                    (or (:payload (get @external (:oid base)))
                                        (some-> (when resolve-ref
                                                  (resolve-ref (:oid base)))
                                                :payload)
                                        (throw (ex-info "Missing Git REF_DELTA base"
                                                        {:offset offset :base base})))))]
                            (apply-delta base-value
                                         (:payload (inflate-at pack data-offset limit
                                                               size true nil))))
                          (:payload (inflate-at pack data-offset limit size true nil)))]
                    (cache-put! cache offset result))))

            (resolve-entry [entry]
              (let [{:keys [offset storage-type base data-offset size]} entry
                    base-info
                    (case storage-type
                      :ofs-delta (get @resolved (:offset base))
                      :ref-delta
                      (or (when-let [base-offset (get @oid-offset (:oid base))]
                            (get @resolved base-offset))
                          (get @external (:oid base))
                          (when resolve-ref
                            (when-let [resolved-base (resolve-ref (:oid base))]
                              (swap! external assoc (:oid base) resolved-base)
                              resolved-base)))
                      nil)]
                (when (or (not (#{:ofs-delta :ref-delta} storage-type)) base-info)
                  (if-not (#{:ofs-delta :ref-delta} storage-type)
                    (get @resolved offset)
                    (let [base-value
                          (if-let [base-offset (and (= storage-type :ofs-delta)
                                                    (:offset base))]
                            (materialize base-offset 0)
                            (if-let [internal-offset (get @oid-offset (:oid base))]
                              (materialize internal-offset 0)
                              (:payload base-info)))
                          delta (:payload (inflate-at pack data-offset limit size true nil))
                          payload (apply-delta base-value delta)
                          type (:type base-info)
                          oid (object/object-id object-format type payload)
                          info {:oid oid :type type}]
                      (swap! resolved assoc offset info)
                      (swap! oid-offset assoc oid offset)
                      (cache-put! cache offset payload)
                      info)))))]
      (loop [pending (vec (filter #(#{:ofs-delta :ref-delta} (:storage-type %)) entries))]
        (if (empty? pending)
          (mapv (fn [entry] (merge entry (get @resolved (:offset entry)))) entries)
          (let [[remaining progress]
                (reduce (fn [[remaining progress] entry]
                          (if (resolve-entry entry)
                            [remaining true]
                            [(conj remaining entry) progress]))
                        [[] false] pending)]
            (if progress
              (recur remaining)
              (throw (ex-info "Unresolved thin-pack or cyclic delta bases"
                              {:missing-bases (mapv :base remaining)})))))))))

(defn scan
  "Validate and index a pack through positional reads. Inflated bodies are not
  retained. `:resolve-ref` may supply `{ :type keyword :payload bytes }` for a
  thin-pack base."
  ([pack] (scan pack nil))
  ([pack {:keys [object-format resolve-ref delta-cache-bytes
                 primitive-index-threshold phase-fn]
          :or {object-format :sha1 delta-cache-bytes (* 96 1024 1024)
               primitive-index-threshold 0}
          :as opts}]
   (let [length (source/source-size pack)
         checksum-size (checksum-length object-format)
         _ (ensure-available! length 0 (+ 12 checksum-size) :pack-envelope)
         ^bytes header (source/read-range pack 0 12)]
     (when-not (= "PACK" (String. header 0 4 "US-ASCII"))
       (throw (ex-info "Invalid Git pack signature" {})))
     (let [buffer (doto (ByteBuffer/wrap header 4 8) (.order ByteOrder/BIG_ENDIAN))
           version (.getInt buffer)
           count (.getInt buffer)
           limit (- length checksum-size)
           expected (source/read-range pack limit checksum-size)
           checksum-start (phase-start phase-fn :verify-checksum
                                       {:pack-bytes length :objects count})
           actual (digest-range pack 0 limit object-format)
           checksum-ms (phase-complete phase-fn :verify-checksum checksum-start
                                       {:pack-bytes length :objects count})]
       (when-not (#{2 3} version)
         (throw (ex-info "Unsupported Git pack version" {:version version})))
       (when-not (Arrays/equals ^bytes expected ^bytes actual)
         (throw (ex-info "Git pack checksum mismatch" {})))
       (let [result
             (if (>= count primitive-index-threshold)
               (scan-primitive pack (assoc (or opts {})
                                           :object-format object-format
                                           :resolve-ref resolve-ref
                                           :delta-cache-bytes delta-cache-bytes)
                               version count limit expected length)
               (loop [index 0 position 12 entries []]
                 (if (= index count)
                   (do
                     (when-not (= position limit)
                       (throw (ex-info "Trailing data before Git pack checksum"
                                       {:position position :limit limit})))
                     (let [resolved
                           (resolve-entries pack entries limit object-format
                                            resolve-ref delta-cache-bytes)]
                       {:version version :count count
                        :size length
                        :checksum (object/bytes->hex expected)
                        :storage-types
                        (frequencies (map :storage-type entries))
                        :objects resolved}))
                   (let [{:keys [storage-type size data-offset] :as entry}
                         (entry-prefix pack position limit object-format)
                         object-type
                         (when-not (#{:ofs-delta :ref-delta} storage-type)
                           storage-type)
                         digest (when object-type
                                  (object-digest object-format object-type size))
                         inflated
                         (inflate-at pack data-offset limit size false digest)
                         entry
                         (cond-> (assoc entry :end (:end inflated))
                           object-type
                           (assoc :type object-type
                                  :oid (object/bytes->hex
                                        (.digest ^MessageDigest digest))))]
                     (recur (inc index) (long (:end inflated))
                            (conj entries entry))))))]
         (assoc result :timings-ms
                (assoc (:timings-ms result) :verify-checksum checksum-ms)))))))

(defn resolve-at
  "Resolve one object at a stored pack offset with bounded caller cache."
  [pack offset {:keys [object-format resolve-ref max-depth cache cache-bytes]
                :or {object-format :sha1 max-depth 64
                     cache-bytes (* 96 1024 1024)}}]
  (let [limit (- (source/source-size pack) (checksum-length object-format))
        cache (or cache (resolution-cache cache-bytes))]
    (letfn [(resolve-offset [offset depth]
              (when (> depth max-depth)
                (throw (ex-info "Git pack delta chain is too deep"
                                {:offset offset :max-depth max-depth})))
              (or (cache-value cache offset)
                  (let [{:keys [storage-type base data-offset size]}
                        (entry-prefix pack offset limit object-format)
                        base-value
                        (case storage-type
                          :ofs-delta (resolve-offset (:offset base) (inc depth))
                          :ref-delta
                          (or (when resolve-ref (resolve-ref (:oid base)))
                              (throw (ex-info "Missing Git REF_DELTA base"
                                              {:offset offset :base base})))
                          nil)
                        delta-or-payload
                        (:payload (inflate-at pack data-offset limit size true nil))
                        result {:type (if base-value (:type base-value) storage-type)
                                :payload (if base-value
                                           (apply-delta (:payload base-value)
                                                        delta-or-payload)
                                           delta-or-payload)}]
                    (cache-put! cache offset result))))]
      (resolve-offset (long offset) 0))))
