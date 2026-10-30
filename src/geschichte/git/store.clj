(ns geschichte.git.store
  "Persistence boundary for exact Git objects in a Geschichte database."
  (:require [datahike.api :as d]
            [datahike.gc-guard :as guard]
            [geschichte.git.budget :as budget]
            [geschichte.git.cache :as cache]
            [geschichte.git.pack :as pack]
            [geschichte.git.pack-index :as pack-index]
            [geschichte.git.pack-read :as pack-read]
            [geschichte.git.pack-source :as pack-source]
            [geschichte.store.blob :as payload])
  (:import [java.io Closeable InputStream]
           [java.util.concurrent Callable CancellationException
            ExecutionException Executors Future]))

(defrecord ObjectStore [conn cache manifest owner? closed?]
  Closeable
  (close [_]
    (when (compare-and-set! closed? false true)
      (reset! manifest nil)
      (when owner? (cache/close! cache)))))

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

(defn open
  "Open an exact-object store over `conn`. By default it owns an independent,
  bounded cache service. Supply `:cache` to deliberately share a tenant cache;
  the caller then owns that service's lifecycle."
  ([conn] (open conn nil))
  ([conn {:keys [limits] cache-service :cache}]
   (let [owner? (nil? cache-service)]
     (->ObjectStore conn (or cache-service (cache/create limits))
                    (atom nil) owner? (atom false)))))

(defn close!
  "Release references held by an object store and close its owned cache."
  [store]
  (.close ^Closeable store)
  nil)

(defn cache-stats [store]
  (cache/stats (:cache store)))

(defn- object-store? [value]
  (instance? ObjectStore value))

(defn- ensure-open! [store]
  (when @(:closed? store)
    (throw (ex-info "Git object store is closed" {}))))

(defn with-store
  "Call `f` with an ObjectStore. Existing stores are borrowed; bare Datahike
  connections receive an operation-local store that is always closed."
  [value f]
  (if (object-store? value)
    (do (ensure-open! value) (f value))
    ;; Connection acceptance keeps the ordinary Clojure API convenient while
    ;; making its cache operation-local rather than process-global. Long-lived
    ;; callers use `open` and own the returned handle explicitly.
    (let [store (open value)]
      (try (f store) (finally (close! store))))))

(def ^:private pack-selector
  [:geschichte.git.pack/id
   :geschichte.git.pack/size
   :geschichte.git.pack/checksum
   {:geschichte.git.pack/index-shards
    [:geschichte.git.index/prefix
     :geschichte.git.index/count
     :geschichte.git.index/payload]}
   {:geschichte.git.pack/chunks
    [:geschichte.chunk/index
     :geschichte.chunk/offset
     :geschichte.chunk/size
     :geschichte.chunk/payload]}
   :geschichte.git.pack/object-format])

(defn- store-id [conn]
  (:id (:store (:config @conn))))

(defn metadata-budget-policy
  "Finite pack lookup admission limits. Merge into the caller's shared import
  policy; omitted counters in legacy partial policies remain unbounded."
  ([] (metadata-budget-policy 30000))
  ([timeout-ms]
   {:timeout-ms timeout-ms
    :limits {:pack-visits 4096 :pack-metadata-parts 1000000
             :index-bytes (* 16 1024 1024) :index-read-bytes (* 128 1024 1024)}}))

(defn- captured-packs
  "Admit pack and child metadata before pulling it. A bounded lookup scans
  lazily against one captured catalog and never builds a full manifest."
  [db operation]
  (letfn [(step [rows]
            (lazy-seq
             (when-let [rows (seq rows)]
               (budget/charge! operation :pack-visits 1)
               (let [eid (:e (first rows))]
                 (doseq [attribute [:geschichte.git.pack/chunks :geschichte.git.pack/index-shards]
                         _ (d/datoms db :eavt eid attribute)]
                   (budget/charge! operation :pack-metadata-parts 1))
                 (cons (d/pull db pack-selector eid) (step (rest rows)))))))]
    (step (d/datoms db :avet :geschichte.git.pack/id))))

(defn- refresh-packs! [{:keys [conn manifest]}]
  (let [packs (vec (captured-packs @conn nil))]
    (reset! manifest packs)
    packs))

(defn- known-packs [{:keys [manifest] :as store}]
  (or @manifest (refresh-packs! store)))

(defn- cached-pack [{:keys [conn cache] :as _store} pack]
  (let [store-id (store-id conn)
        pack-id (:geschichte.git.pack/id pack)
        key [store-id pack-id]]
    (assoc (cache/pack!
            cache key
            #(hash-map :objects (pack-read/resolution-cache
                                 (cache/resolution-limit cache))))
           ;; Cached entries never retain a Datahike connection. The positional
           ;; source belongs only to this operation/handle.
           :source (pack-source/stored-source conn pack cache))))

(defn- oid-prefix [oid]
  (Integer/parseInt (subs oid 0 1) 16))

(defn- index-shard [store pack prefix operation]
  (when-let [shard (some #(when (= prefix (:geschichte.git.index/prefix %)) %)
                         (:geschichte.git.pack/index-shards pack))]
    (let [{:keys [conn cache]} store
          payload-id (:geschichte.git.index/payload shard)
          _ (budget/check! operation)
          loaded? (volatile! false)
          bytes (cache/chunk!
                 cache [(store-id conn) :index payload-id]
                 #(do (vreset! loaded? true)
                      (payload/get-bytes conn payload-id
                                         {:merge-budget operation :byte-limit-metric :index-bytes
                                          :read-metric :index-read-bytes})))]
      ;; Charge actual bytes even on a warm cache hit; cache capacity is not
      ;; an operation limit. The bounded payload reader also checks each read.
      (when-not @loaded? (budget/charge! operation :index-read-bytes (alength ^bytes bytes)))
      (budget/check! operation)
      (budget/bound! operation :index-bytes (alength ^bytes bytes))
      (budget/check! operation)
      (pack-index/open bytes))))

(defn- object-in-packs [store packs oid operation]
  (some (fn [pack]
          (when-let [index (index-shard store pack (oid-prefix oid) operation)]
            (when-let [{:keys [type offset]} (pack-index/lookup index oid)]
              {:geschichte.git.object/oid oid
               :geschichte.git.object/type type
               :geschichte.git.object/offset offset
               :geschichte.git.object/pack pack})))
        packs))

(defn object
  "Return Git object metadata by OID. The lookup table is loaded through the
  pack's store-ref and cached outside Datahike's indices."
  [store-or-conn oid]
  (with-store
    store-or-conn
    (fn [store]
      (or (object-in-packs store (known-packs store) oid nil)
         ;; A peer may have published another pack since this handle populated
         ;; its manifest. A miss is the inexpensive invalidation boundary.
          (object-in-packs store (refresh-packs! store) oid nil)))))

(declare read-object*)

(defn- read-object*
  ([store oid] (read-object* store oid nil))
  ([store oid opts]
   (budget/check! (:import-budget opts))
   (when (contains? (:import-seen opts #{}) oid)
     (throw (ex-info "Cyclic external Git delta base" {:oid oid})))
   (when-let [metadata (if-let [operation (:import-budget opts)]
                         ;; One exact immutable catalog, one admitted scan,
                         ;; including misses; no eager full manifest or retry.
                         (let [db @(:conn store)]
                           (object-in-packs store (captured-packs db operation) oid operation))
                         (or (object-in-packs store (known-packs store) oid nil)
                             (object-in-packs store (refresh-packs! store) oid nil)))]
     (let [pack (:geschichte.git.object/pack metadata)
           {pack-source :source resolution :objects} (cached-pack store pack)
           resolved
           (try
             (pack-read/resolve-at
              pack-source (:geschichte.git.object/offset metadata)
              {:object-format (:geschichte.git.pack/object-format pack)
               :import-budget (:import-budget opts)
               :import-depth (:import-depth opts 0)
               :cache (when-not (:import-budget opts) resolution)
               :resolve-ref-context
               (fn [base-oid depth]
                 (when-let [base (read-object* store base-oid
                                               (assoc opts :import-depth depth
                                                      :import-seen (conj (:import-seen opts #{}) oid)))]
                   {:type (:geschichte.git.object/type base)
                    :delta-depth (:delta-depth base 0)
                    :payload (:payload base)}))
               :resolve-ref
               (fn [base-oid]
                 (when-let [base (read-object* store base-oid)]
                   {:type (:geschichte.git.object/type base)
                    :payload (:payload base)}))})
             (catch clojure.lang.ExceptionInfo error
               (throw
                (ex-info
                 (ex-message error)
                 (merge (ex-data error)
                        {:oid oid
                         :indexed-offset (:geschichte.git.object/offset metadata)
                         :pack-id (:geschichte.git.pack/id pack)})
                 error))))]
       (when-not (= (:geschichte.git.object/type metadata) (:type resolved))
         (throw (ex-info "Packed Git object type mismatch"
                         {:oid oid
                          :expected (:geschichte.git.object/type metadata)
                          :actual (:type resolved)})))
       (budget/bound! (:import-budget opts) :object-bytes (alength ^bytes (:payload resolved)))
       (assoc metadata :payload (:payload resolved) :delta-depth (:delta-depth resolved 0))))))

(defn read-object
  "Return exact Git object metadata with its lazily resolved payload."
  ([store-or-conn oid] (read-object store-or-conn oid nil))
  ([store-or-conn oid opts]
   (with-store store-or-conn #(read-object* % oid opts))))

(defn read-payload
  ([store-or-conn oid] (read-payload store-or-conn oid nil))
  ([store-or-conn oid opts] (:payload (read-object store-or-conn oid opts))))

(defn- existing-oids [store]
  (into #{}
        (mapcat (fn [pack]
                  (mapcat (fn [{:geschichte.git.index/keys [prefix]}]
                            (map :oid
                                 (pack-index/entries
                                  (index-shard store pack prefix nil))))
                          (:geschichte.git.pack/index-shards pack))))
        (known-packs store)))

(declare persist-pack! tracking-name)

(defn- persist-graph*!
  [store {:keys [objects commits] :as graph}]
  (let [conn (:conn store)
        present (existing-oids store)
        missing (into (sorted-map)
                      (remove (fn [[oid _]] (contains? present oid)))
                      objects)
        persisted
        (if (seq missing)
          (let [bytes (pack/encode missing)
                indexed (pack-read/scan (pack-source/byte-array-source bytes))]
            (persist-pack! store bytes indexed :sha1))
          0)
        mapping-tx
        (mapv (fn [[commit-id oid]]
                {:geschichte.commit/id commit-id
                 :geschichte.commit/git-oid oid})
              commits)]
    (when (seq mapping-tx) (d/transact conn mapping-tx))
    (assoc graph :persisted persisted)))

(defn persist-graph!
  "Persist a graph returned by `geschichte.git.project/project` and record each
  Geschichte commit's corresponding Git OID. Existing OIDs deduplicate."
  [store-or-conn graph]
  (with-store store-or-conn #(persist-graph*! % graph)))

(defn project!
  "Project a Geschichte commit and persist the resulting Git graph."
  ([conn commit-id] (project! conn commit-id nil))
  ([conn commit-id opts]
   ;; Resolve lazily to keep the exact-object storage boundary independent of
   ;; the higher-level Geschichte-to-Git projector. The projector itself reads
   ;; through this namespace, including pack-backed objects.
   (let [project (requiring-resolve 'geschichte.git.project/project)]
     (with-store
       (or (:object-store opts) conn)
       (fn [object-store]
         (persist-graph!
          object-store
          (project conn commit-id
                   (assoc (or opts {}) :object-store object-store))))))))

(defn- stored-base
  ([store oid] (stored-base store oid nil))
  ([store oid opts]
   (when-let [metadata (read-object* store oid opts)]
     {:type (:geschichte.git.object/type metadata)
      :delta-depth (:delta-depth metadata 0)
      :payload (:payload metadata)})))

(defn- ref-tx-data [remote refs]
  (mapv (fn [{:keys [oid ref attributes]}]
          (cond-> {:geschichte.git.ref/name (tracking-name remote ref)
                   :geschichte.git.ref/source ref}
            oid (assoc :geschichte.git.ref/oid oid)
            (:peeled attributes)
            (assoc :geschichte.git.ref/peeled (:peeled attributes))
            (:symref-target attributes)
            (assoc :geschichte.git.ref/symref-target
                   (tracking-name remote (:symref-target attributes)))))
        refs))

(defn- same-pack? [store checksum object-format]
  (some #(and (= checksum (:geschichte.git.pack/checksum %))
              (= object-format (:geschichte.git.pack/object-format %)))
        (known-packs store)))

(defn- scanned-index-shards [indexed object-format]
  (or (:index-shards indexed)
      (->> (:objects indexed)
           (group-by #(oid-prefix (:oid %)))
           (sort-by key)
           (map (fn [[prefix objects]]
                  {:prefix prefix :count (count objects)
                   :bytes (pack-index/encode objects object-format)})))))

(defn- settled-future [^Future future]
  (loop [interrupted? false]
    (let [[status value]
          (try
            [:value (.get future)]
            (catch InterruptedException error [:interrupted error])
            (catch ExecutionException error [:error (.getCause error)])
            (catch CancellationException error [:error error]))]
      (if (= :interrupted status)
        (recur true)
        (cond-> {:interrupted? interrupted?}
          (= :value status) (assoc :value value)
          (= :error status) (assoc :error value))))))

(defn- publish-index-shards! [conn shards opts]
  (let [parallelism (max 1 (long (or (:index-parallelism opts) 4)))
        executor (Executors/newFixedThreadPool (int parallelism))]
    (try
      (let [futures
            (mapv
             (fn [{:keys [prefix count bytes encode]}]
               (.submit executor
                        ^Callable
                        (fn []
                          (let [bytes (or bytes (encode))]
                            {:geschichte.git.index/prefix (long prefix)
                             :geschichte.git.index/count (long count)
                             :geschichte.git.index/payload
                             (payload/put! conn bytes opts)}))))
             shards)
            settled (mapv settled-future futures)
            interrupted? (some :interrupted? settled)
            error (some :error settled)]
        (when interrupted? (.interrupt (Thread/currentThread)))
        (cond
          error (throw error)
          interrupted? (throw (InterruptedException.
                               "Interrupted while publishing Git index shards"))
          :else (mapv :value settled)))
      (finally (.shutdown executor)))))

(defn- publish-pack-source!
  [store {:keys [chunks size]} indexed object-format {:keys [remote refs] :as opts}]
  (let [conn (:conn store)
        duplicate? (same-pack? store (:checksum indexed) object-format)
        index-tx
        (when-not duplicate?
          (publish-index-shards! conn
                                 (scanned-index-shards indexed object-format)
                                 opts))
        pack-id (when-not duplicate?
                  (payload/content-id
                   (.getBytes (str (name object-format) ":" (:checksum indexed))
                              "UTF-8")))
        chunk-tx
        (mapv (fn [{:keys [index offset size payload]}]
                {:geschichte.chunk/index (long index)
                 :geschichte.chunk/offset (long offset)
                 :geschichte.chunk/size (long size)
                 :geschichte.chunk/payload payload})
              chunks)
        pack-tx (when-not duplicate?
                  [{:geschichte.git.pack/id pack-id
                    :geschichte.git.pack/size (long size)
                    :geschichte.git.pack/checksum (:checksum indexed)
                    :geschichte.git.pack/index-shards index-tx
                    :geschichte.git.pack/chunks chunk-tx
                    :geschichte.git.pack/object-format object-format}])
        refs-tx (when (and remote (seq refs)) (ref-tx-data remote refs))
        tx-data (into (vec pack-tx) refs-tx)]
    (budget/check! (:import-budget opts))
    (when (seq tx-data) (d/transact conn tx-data))
    (when-not duplicate? (reset! (:manifest store) nil))
    (if duplicate? 0 (:count indexed))))

(defn import-pack-producer!
  "Import a pack supplied incrementally by `producer`.

  `producer` receives a `(write! bytes offset length)` callback and may return
  transport metadata. Pack chunks and the compact index remain protected by
  Datahike's unreferenced-write guard until one transaction publishes the pack
  and optional `:remote`/`:refs` together."
  ([store-or-conn producer] (import-pack-producer! store-or-conn producer nil))
  ([store-or-conn producer opts]
   (with-store
     store-or-conn
     (fn [store]
       (budget/check! (:import-budget opts))
       (let [opts (or opts {})
             conn (:conn store)
             object-format (:object-format opts :sha1)
             sid (:id (:store (:config @conn)))
             phase-fn (:phase-fn opts)]
         (guard/with-unreferenced-writes sid
           (let [receive-start (phase-start phase-fn :receive-pack {})
                 {:keys [write! finish!]} (pack-source/streaming-sink conn opts)
                 producer-result (producer write!)
                 descriptor (finish!)
                 receive-ms (phase-complete phase-fn :receive-pack receive-start
                                            {:pack-bytes (:size descriptor)
                                             :chunks (count (:chunks descriptor))})
                 source (pack-source/descriptor-source
                         conn (:chunks descriptor) (:size descriptor)
                         (:cache store))
                 scan-start (phase-start phase-fn :scan-pack
                                         {:pack-bytes (:size descriptor)})
                 indexed (pack-read/scan
                          source
                          (assoc opts
                                 :resolve-ref #(stored-base store % opts)
                                 :resolve-ref-context (fn [oid depth]
                                                        (stored-base store oid (assoc opts :import-depth depth)))))
                 scan-ms (phase-complete phase-fn :scan-pack scan-start
                                         {:pack-bytes (:size descriptor)
                                          :objects (:count indexed)})
                 publish-start (phase-start phase-fn :publish-pack
                                            {:objects (:count indexed)})
                 persisted (publish-pack-source! store descriptor indexed
                                                 object-format opts)
                 publish-ms (phase-complete phase-fn :publish-pack publish-start
                                            {:objects (:count indexed)
                                             :persisted persisted})]
             (cond-> (assoc (dissoc indexed :index-shards)
                            :persisted persisted
                            :timings-ms
                            (merge (:timings-ms indexed)
                                   {:receive-pack receive-ms
                                    :scan-pack scan-ms
                                    :publish-pack publish-ms}))
               (map? producer-result) (merge producer-result)))))))))

(defn- persist-pack! [store bytes indexed object-format]
  (let [conn (:conn store)
        sid (:id (:store (:config @conn)))]
    (guard/with-unreferenced-writes sid
      (let [{:keys [write! finish!]} (pack-source/streaming-sink conn nil)
            _ (write! bytes)
            descriptor (finish!)]
        (publish-pack-source! store descriptor indexed object-format nil)))))

(defn import-pack!
  "Decode a Git pack and persist every resolved object. Thin-pack bases are read
  from existing Geschichte Git-object storage when callers do not supply them."
  ([store-or-conn bytes] (import-pack! store-or-conn bytes nil))
  ([store-or-conn bytes opts]
   (import-pack-producer!
    store-or-conn
    (fn [write!] (write! bytes))
    opts)))

(defn import-pack-stream!
  "Import a raw pack from `input` without materializing it. The caller owns and
  closes the InputStream."
  ([store-or-conn input] (import-pack-stream! store-or-conn input nil))
  ([store-or-conn ^InputStream input opts]
   (import-pack-producer!
    store-or-conn
    (fn [write!]
      (let [buffer (byte-array (* 64 1024))]
        (loop []
          (let [n (.read input buffer)]
            (cond
              (neg? n) nil
              (zero? n) (recur)
              :else (do (write! buffer 0 n) (recur)))))))
    opts)))

(defn tracking-name
  "Map a source ref into its local remote-tracking namespace."
  [remote source]
  (cond
    (= source "HEAD") (str "refs/remotes/" remote "/HEAD")
    (.startsWith ^String source "refs/heads/")
    (str "refs/remotes/" remote "/" (subs source (count "refs/heads/")))
    :else source))

(defn record-refs!
  "Record parsed ls-refs results as Git refs without materializing commits."
  [conn remote refs]
  (let [tx (ref-tx-data remote refs)]
    (when (seq tx) (d/transact conn tx))
    (count tx)))

(defn refs
  "Return local Git ref metadata keyed by tracking-ref name."
  [conn]
  (into (sorted-map)
        (map (fn [[ref]] [(:geschichte.git.ref/name ref) ref]))
        (d/q '[:find (pull ?e [:geschichte.git.ref/name
                               :geschichte.git.ref/source
                               :geschichte.git.ref/oid
                               :geschichte.git.ref/peeled
                               :geschichte.git.ref/symref-target])
               :where [?e :geschichte.git.ref/name]] @conn)))
