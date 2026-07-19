(ns geschichte.git.pack-source
  "Positional access to immutable Git pack streams.

  Packs are stored as ordered, content-addressed chunks. Readers request only
  the chunks intersecting a byte range; the exact original pack stream remains
  reconstructible without a second whole-pack payload."
  (:require [geschichte.git.cache :as cache]
            [geschichte.store.blob :as blob])
  (:import [java.util Arrays]))

;; Four MiB keeps manifests and sequential Konserve lookups compact. Delta
;; resolution preserves pack order and releases dependency subtrees eagerly,
;; avoiding the random-read amplification of breadth-first traversal.
(def default-chunk-size (* 4 1024 1024))

(defprotocol PackSource
  (source-size [source])
  (read-range [source offset length])
  (read-window [source offset max-length]
    "Borrow a contiguous backing-array window beginning at offset."))

(defrecord ByteArraySource [^bytes bytes]
  PackSource
  (source-size [_] (alength bytes))
  (read-range [_ offset length]
    (let [offset (long offset)
          end (+ offset (long length))]
      (when (or (neg? offset) (> end (alength bytes)))
        (throw (ex-info "Pack range is outside the source"
                        {:offset offset :length length
                         :source-size (alength bytes)})))
      (Arrays/copyOfRange bytes (int offset) (int end))))
  (read-window [_ offset max-length]
    (let [length (int (min (long max-length) (- (alength bytes) (long offset))))]
      (when (or (neg? offset) (neg? length))
        (throw (ex-info "Pack window is outside the source"
                        {:offset offset :max-length max-length
                         :source-size (alength bytes)})))
      {:bytes bytes :offset (int offset) :length length})))

(defn byte-array-source [^bytes bytes]
  (->ByteArraySource bytes))

(defn- load-chunk [conn cache-service store-id {:keys [payload]}]
  (cache/chunk! cache-service [store-id payload]
                #(blob/get-bytes conn payload)))

(defn- chunk-index-at [chunks position]
  (loop [low 0 high (dec (count chunks))]
    (when (<= low high)
      (let [middle (unsigned-bit-shift-right (+ low high) 1)
            {:keys [offset size]} (nth chunks middle)]
        (cond
          (< position offset) (recur low (dec middle))
          (>= position (+ offset size)) (recur (inc middle) high)
          :else middle)))))

(defrecord StoredSource [conn cache-service store-id chunks size]
  PackSource
  (source-size [_] size)
  (read-range [_ requested-offset requested-length]
    (let [requested-offset (long requested-offset)
          requested-length (long requested-length)
          requested-end (+ requested-offset requested-length)]
      (when (or (neg? requested-offset) (neg? requested-length)
                (> requested-end size))
        (throw (ex-info "Pack range is outside the stored source"
                        {:offset requested-offset :length requested-length
                         :source-size size})))
      (let [out (byte-array (int requested-length))]
        (loop [index (when (pos? requested-length)
                       (chunk-index-at chunks requested-offset))]
          (when (some? index)
            (let [{chunk-offset :offset chunk-size :size :as chunk}
                  (nth chunks index)
                  next-index (when (< (inc index) (count chunks)) (inc index))]
              (let [chunk-end (+ chunk-offset chunk-size)]
                (cond
                  (>= chunk-offset requested-end) nil
                  :else
                  (let [^bytes bytes (load-chunk conn cache-service store-id chunk)
                        from (max requested-offset chunk-offset)
                        to (min requested-end chunk-end)
                        source-position (- from chunk-offset)
                        target-position (- from requested-offset)
                        count (- to from)]
                    (System/arraycopy bytes (int source-position) out
                                      (int target-position) (int count))
                    (recur next-index)))))))
        out)))
  (read-window [_ requested-offset max-length]
    (when (or (neg? requested-offset) (>= requested-offset size))
      (throw (ex-info "Pack window is outside the stored source"
                      {:offset requested-offset :max-length max-length
                       :source-size size})))
    (let [index (or (chunk-index-at chunks requested-offset)
                    (throw (ex-info "Pack manifest has a gap"
                                    {:offset requested-offset})))
          {chunk-offset :offset chunk-size :size :as chunk} (nth chunks index)
          ^bytes bytes (load-chunk conn cache-service store-id chunk)
          local-offset (- requested-offset chunk-offset)
          length (int (min (long max-length) (- chunk-size local-offset)))]
      {:bytes bytes :offset (int local-offset) :length length})))

(defn stored-source [conn pack cache-service]
  (let [store-id (:id (:store (:config @conn)))
        chunks (->> (:geschichte.git.pack/chunks pack)
                    (map (fn [chunk]
                           {:index (:geschichte.chunk/index chunk)
                            :offset (:geschichte.chunk/offset chunk)
                            :size (:geschichte.chunk/size chunk)
                            :payload (:geschichte.chunk/payload chunk)}))
                    (sort-by :index)
                    vec)]
    (->StoredSource conn cache-service store-id chunks
                    (:geschichte.git.pack/size pack))))

(defn descriptor-source
  "Open a range-readable source from already-written pack chunk descriptors.

  Unlike `stored-source`, this does not require a published Datahike pack
  entity. Fetch ingestion uses it to validate and index orphan-protected chunks
  before the single transaction that makes their manifest reachable."
  [conn chunks size cache-service]
  (->StoredSource conn cache-service (:id (:store (:config @conn)))
                  (vec (sort-by :index chunks)) (long size)))

(defn streaming-sink
  "Return a bounded writer for an exact pack byte stream.

  `:write!` accepts a byte array plus optional offset/length. Complete fixed
  chunks are content-addressed and written immediately through Konserve;
  `:finish!` flushes the tail and returns `{:chunks ... :size ...}`. The caller
  must hold Datahike's unreferenced-write guard until the resulting manifest is
  transacted."
  ([conn] (streaming-sink conn nil))
  ([conn opts]
   (let [chunk-size (long (or (:pack-chunk-size opts) default-chunk-size))
         _ (when-not (and (pos? chunk-size) (<= chunk-size Integer/MAX_VALUE))
             (throw (ex-info "Invalid Git pack chunk size"
                             {:chunk-size chunk-size})))
         state (volatile! {:buffer (byte-array (int chunk-size))
                           :used 0 :index 0 :offset 0 :chunks []
                           :finished? false})
         publish!
         (fn []
           (let [{:keys [^bytes buffer used index offset finished?]} @state]
             (when finished?
               (throw (ex-info "Git pack sink is already finished" {})))
             (when (pos? used)
               (let [value (if (= used (alength buffer))
                             buffer
                             (Arrays/copyOf buffer (int used)))
                     id (blob/put! conn value opts)]
                 (vreset! state
                          {:buffer (byte-array (int chunk-size))
                           :used 0 :index (inc index) :offset (+ offset used)
                           :chunks (conj (:chunks @state)
                                         {:index (long index)
                                          :offset (long offset)
                                          :size (long used)
                                          :payload id})
                           :finished? false})))))
         write!
         (fn write!
           ([value] (write! value 0 (alength ^bytes value)))
           ([^bytes value source-offset length]
            (when (:finished? @state)
              (throw (ex-info "Cannot write a finished Git pack sink" {})))
            (loop [source-offset (int source-offset) remaining (int length)]
              (when (pos? remaining)
                (let [{:keys [^bytes buffer used]} @state
                      available (- (alength buffer) (int used))
                      n (min available remaining)]
                  (System/arraycopy value source-offset buffer (int used) n)
                  (vswap! state update :used + n)
                  (when (= (alength ^bytes (:buffer @state)) (:used @state))
                    (publish!))
                  (recur (+ source-offset n) (- remaining n)))))))
         finish!
         (fn []
           (when (:finished? @state)
             (throw (ex-info "Git pack sink is already finished" {})))
           (publish!)
           (vswap! state assoc :finished? true)
           {:chunks (:chunks @state) :size (:offset @state)})]
     {:write! write! :finish! finish!})))

(defn chunks
  "Split an in-memory exact pack stream into fixed positional chunks."
  ([bytes] (chunks bytes default-chunk-size))
  ([^bytes bytes chunk-size]
   (mapv (fn [index offset]
           (let [end (min (alength bytes) (+ offset chunk-size))]
             {:index index :offset offset :size (- end offset)
              :bytes (Arrays/copyOfRange bytes (int offset) (int end))}))
         (range)
         (range 0 (alength bytes) chunk-size))))
