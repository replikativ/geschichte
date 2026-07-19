(ns geschichte.content
  "Content-addressed logical files with representation-independent storage."
  (:refer-clojure :exclude [await])
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [datahike.blob :as blob]
            [datahike.gc-guard :as guard]
            [geschichte.async :as execution]
            [geschichte.bytes :as bytes]
            [geschichte.chunk :as chunk]
            [geschichte.diff :as diff]
            [geschichte.store.blob :as payload]
            [is.simm.partial-cps.async :refer [await]]
            #?(:clj [clojure.edn :as edn]
               :cljs [cljs.reader :as edn])
            #?@(:clj [[hasch.platform :as hasch-platform]]))
  #?(:cljs (:require-macros [geschichte.macros :refer [async+sync]]
                            [is.simm.partial-cps.async :refer [async]]))
  #?(:clj (:require [geschichte.macros :refer [async+sync]]
                    [is.simm.partial-cps.async :refer [async]]
                    [clojure.java.io :as io]))
  #?(:clj (:import [java.io File FileInputStream FilterInputStream OutputStream]
                   [java.nio ByteBuffer]
                   [java.nio.charset CharacterCodingException CodingErrorAction
                    StandardCharsets]
                   [java.util Arrays])))

(def default-options
  {:max-delta-depth 8
   :delta-ratio 0.8
   :chunk-threshold (* 4 1024 1024)
   :chunk-min-size (:chunk-min-size chunk/default-options)
   :chunk-size (:chunk-size chunk/default-options)
   :chunk-max-size (:chunk-max-size chunk/default-options)})

(def stop-consumption
  "Consumer return value that stops `consume-by-id!` after the current chunk."
  ::stop-consumption)

(defn- content-entity [db id]
  (d/q '[:find (pull ?e [:db/id :geschichte.content/id
                         :geschichte.content/kind :geschichte.content/payload
                         :geschichte.content/depth :geschichte.content/size
                         :geschichte.content/chunking-algorithm
                         :geschichte.content/chunking-version
                         :geschichte.content/chunk-min-size
                         :geschichte.content/chunk-size
                         :geschichte.content/chunk-max-size
                         {:geschichte.content/chunks
                          [:geschichte.chunk/index :geschichte.chunk/offset
                           :geschichte.chunk/payload
                           :geschichte.chunk/size]}
                         {:geschichte.content/base [:geschichte.content/id]}]) .
         :in $ ?id
         :where [?e :geschichte.content/id ?id]]
       db id))

(defn- ordered-chunks [entity]
  (sort-by :geschichte.chunk/index (:geschichte.content/chunks entity)))

(defn- checked-payload [conn id opts]
  (async+sync (:sync? opts)
              (async
               (let [value (await (payload/get-bytes conn id opts))
                     actual (blob/blob-id value)]
                 (when-not (= id actual)
                   (throw (ex-info "Chunk hash mismatch"
                                   {:chunk id :actual actual})))
                 value))))

(defn- strict-utf8 [value]
  #?(:clj
     (try
       (let [decoder (doto (.newDecoder StandardCharsets/UTF_8)
                       (.onMalformedInput CodingErrorAction/REPORT)
                       (.onUnmappableCharacter CodingErrorAction/REPORT))
             text (str (.decode decoder (ByteBuffer/wrap value)))]
         (when-not (str/includes? text "\u0000") text))
       (catch CharacterCodingException _ nil))
     :cljs
     (try
       (let [text (.decode (js/TextDecoder. "utf-8" #js {:fatal true}) value)]
         (when-not (str/includes? text "\u0000") text))
       (catch :default _ nil))))

(defn- line-delta [base target]
  (let [result (diff/diff-text base target)]
    {:final-newline? (:b-final-newline? result)
     :edits
     (mapv (fn [{:keys [op b-start b-count] :as edit}]
             (cond-> (select-keys edit [:op :a-start :a-count :b-start :b-count])
               (= :insert op)
               (assoc :lines (subvec (:b-lines result)
                                     b-start (+ b-start b-count)))))
           (:edits result))}))

(defn- apply-line-delta [base {:keys [edits final-newline?]}]
  (let [base-lines (:lines (diff/text-lines base))
        target-lines
        (persistent!
         (reduce (fn [out {:keys [op a-start a-count lines]}]
                   (case op
                     :equal (reduce conj! out
                                    (subvec base-lines a-start (+ a-start a-count)))
                     :delete out
                     :insert (reduce conj! out lines)))
                 (transient []) edits))]
    (str (str/join "\n" target-lines)
         (when final-newline? "\n"))))

(declare read-by-id)

(defn- decode [conn entity opts]
  (async+sync (:sync? opts)
              (async
               (let [stored
                     (await
                      (payload/get-bytes
                       conn (:geschichte.content/payload entity) opts))]
                 (case (:geschichte.content/kind entity)
                   :full stored
                   :chunks
                   (loop [remaining (seq (ordered-chunks entity))
                          values []]
                     (if-let [chunk (first remaining)]
                       (recur (next remaining)
                              (conj values
                                    (await
                                     (checked-payload
                                      conn (:geschichte.chunk/payload chunk) opts))))
                       (apply bytes/concat-bytes values)))
                   :line-delta
                   (let [base-id
                         (get-in entity
                                 [:geschichte.content/base
                                  :geschichte.content/id])
                         base (strict-utf8
                               (await (read-by-id conn base-id opts)))
                         delta (edn/read-string (bytes/decode-utf8 stored))]
                     (bytes/utf8 (apply-line-delta base delta)))
                   (throw (ex-info "Unknown content representation"
                                   {:content (:geschichte.content/id entity)
                                    :kind (:geschichte.content/kind entity)})))))))

(defn read-by-id
  "Reconstruct and validate logical content bytes.

  The two-argument JVM API remains synchronous. ClojureScript returns a
  partial-cps computation; pass execution options explicitly when composing."
  ([conn id] (read-by-id conn id nil))
  ([conn id provided-opts]
   (let [opts (execution/opts provided-opts)]
     (when id
       (async+sync (:sync? opts)
                   (async
                    (let [entity (or (content-entity @conn id)
                                     (throw (ex-info "Unknown content"
                                                     {:content id})))
                          value (await (decode conn entity opts))
                          actual (blob/blob-id value)]
                      (when-not (= id actual)
                        (throw (ex-info "Content hash mismatch"
                                        {:content id :actual actual})))
                      value)))))))

(defn info
  "Return representation metadata without loading the physical payload."
  [conn id]
  (some-> (content-entity @conn id)
          (dissoc :db/id)))

(defn consume-by-id!
  "Pass logical content to `consume!` one bounded byte buffer at a time.

  Chunked values invoke the consumer in manifest order; full values and deltas
  invoke it once. Returning `stop-consumption` prevents loading later chunks.
  In CLJS the default consumer is synchronous. Set
  `:async-consumer? true` when the consumer returns a partial-cps computation
  (for example while waiting for Node stream drain or a browser WritableStream
  promise). Returns a partial-cps computation on CLJS and a direct value on JVM."
  ([conn id consume!] (consume-by-id! conn id consume! nil))
  ([conn id consume! provided-opts]
   (let [opts (execution/opts provided-opts)]
     (async+sync (:sync? opts)
                 (async
                  (let [entity (or (content-entity @conn id)
                                   (throw (ex-info "Unknown content"
                                                   {:content id})))]
                    (if (= :chunks (:geschichte.content/kind entity))
                      (loop [remaining (seq (ordered-chunks entity))]
                        (when-let [chunk (first remaining)]
                          (let [value
                                (await
                                 (checked-payload
                                  conn (:geschichte.chunk/payload chunk) opts))]
                            (let [result (if (:async-consumer? opts)
                                           (await (consume! value))
                                           (consume! value))]
                              (when-not (= stop-consumption result)
                                (recur (next remaining)))))))
                      (let [value (await (read-by-id conn id opts))]
                        (if (:async-consumer? opts)
                          (await (consume! value))
                          (consume! value))))
                    true))))))

(defn- choose-representation [conn value base-id provided-opts]
  (let [opts (merge default-options (execution/opts provided-opts))]
    (async+sync (:sync? opts)
                (async
                 (let [{:keys [max-delta-depth delta-ratio]} opts
                       base-entity (when base-id (content-entity @conn base-id))
                       base-depth (:geschichte.content/depth base-entity 0)
                       base-text
                       (when (and base-entity (< base-depth max-delta-depth))
                         (strict-utf8
                          (await (read-by-id conn base-id opts))))
                       target-text (strict-utf8 value)
                       delta (when (and base-text target-text)
                               (line-delta base-text target-text))
                       delta-bytes (when delta (bytes/utf8 (pr-str delta)))]
                   (if (and delta-bytes
                            (< (bytes/length delta-bytes)
                               (* delta-ratio (max 1 (bytes/length value)))))
                     {:kind :line-delta :bytes delta-bytes :base base-id
                      :depth (inc base-depth)}
                     {:kind :full :bytes value :depth 0}))))))

(defn- raw-transact [conn tx-data opts]
  #?(:clj (if (:sync? opts)
            (d/transact conn tx-data)
            (d/transact! conn tx-data))
     :cljs (if (:sync? opts)
             (throw (ex-info "Synchronous Datahike transactions are unavailable in CLJS"
                             {}))
             (d/transact! conn tx-data))))

(defn- transact-data [conn tx-data opts]
  (async+sync (:sync? opts)
              (async
               (await
                (execution/io-result (raw-transact conn tx-data opts) opts)))))

(defn- store-id [conn]
  (:id (:store (:config @conn))))

(defn- chunk-metadata [compiled chunks]
  {:geschichte.content/kind :chunks
   :geschichte.content/chunks chunks
   :geschichte.content/chunking-algorithm (:chunking-algorithm compiled)
   :geschichte.content/chunking-version (long (:chunking-version compiled))
   :geschichte.content/chunk-min-size (long (:chunk-min-size compiled))
   :geschichte.content/chunk-size (long (:chunk-size compiled))
   :geschichte.content/chunk-max-size (long (:chunk-max-size compiled))})

(defn- store-byte-chunks! [conn value compiled opts]
  (async+sync (:sync? opts)
              (async
               (loop [index 0
                      remaining (seq (chunk/ranges value compiled))
                      chunks []]
                 (if-let [[start end] (first remaining)]
                   (let [value (bytes/slice value start end)
                         payload-id (await (payload/put! conn value opts))]
                     (recur (inc index) (next remaining)
                            (conj chunks
                                  {:geschichte.chunk/index (long index)
                                   :geschichte.chunk/offset (long start)
                                   :geschichte.chunk/payload payload-id
                                   :geschichte.chunk/size
                                   (long (- end start))})))
                   chunks)))))

(defn transact-content!
  "Ensure logical bytes exist, then transact metadata referencing their entity.

  `tx-fn` receives the logical content UUID. Payload publication and the
  Datahike transaction share the store-ref GC guard. Returns the UUID directly
  on the JVM and a partial-cps computation in ClojureScript."
  ([conn value base-id tx-fn]
   (transact-content! conn value base-id tx-fn nil))
  ([conn value base-id tx-fn provided-opts]
   (let [opts (execution/opts provided-opts)
         id (blob/blob-id value)]
     (if (content-entity @conn id)
       (async+sync (:sync? opts)
                   (async
                    (await (transact-data conn (vec (tx-fn id)) opts))
                    id))
       (let [sid (store-id conn)
             token (guard/writing! sid)]
         (execution/with-finalizer
           (fn []
             (async+sync (:sync? opts)
                         (async
                          (let [logical-size (bytes/length value)
                                {:keys [chunk-threshold] :as chunk-opts}
                                (merge default-options opts)
                                chunked? (>= logical-size chunk-threshold)
                                compiled (when chunked?
                                           (chunk/options chunk-opts))
                                chunks (when chunked?
                                         (await (store-byte-chunks!
                                                 conn value compiled opts)))
                                representation
                                (when-not chunked?
                                  (await (choose-representation
                                          conn value base-id opts)))
                                {kind :kind stored-value :bytes
                                 base :base depth :depth} representation
                                payload-id (when stored-value
                                             (await (payload/put!
                                                     conn stored-value opts)))
                                entity (if chunked?
                                         (merge
                                          {:geschichte.content/id id
                                           :geschichte.content/depth 0
                                           :geschichte.content/size (long logical-size)}
                                          (chunk-metadata compiled chunks))
                                         (cond->
                                          {:geschichte.content/id id
                                           :geschichte.content/kind kind
                                           :geschichte.content/payload payload-id
                                           :geschichte.content/depth (long depth)
                                           :geschichte.content/size (long logical-size)}
                                           base
                                           (assoc :geschichte.content/base
                                                  [:geschichte.content/id base])))]
                            (await
                             (transact-data
                              conn (into [entity] (tx-fn id)) opts))
                            id))))
           #(guard/done! sid token)
           opts))))))

#?(:clj
   (defn transact-content-batch!
     "Ensure multiple logical byte values exist and publish all metadata in one
     Datahike transaction. Each item is `{:value bytes :base-id uuid :tx-fn f}`;
     `tx-fn` receives its content UUID. Blob writes remain protected by one
     store-ref GC guard until the combined transaction commits.

     This JVM bulk boundary is intended for imports and checkouts. The portable
     single-content API remains the reference path for asynchronous CLJS hosts."
     ([conn items] (transact-content-batch! conn items nil))
     ([conn items provided-opts]
      (let [opts (execution/opts provided-opts)]
        (when-not (:sync? opts)
          (throw (ex-info "Asynchronous JVM batch ingestion is not implemented"
                          {})))
        (let [sid (store-id conn)
              token (guard/writing! sid)]
          (try
            (let [{:keys [entities publications ids]}
                  (reduce
                   (fn [{:keys [prepared] :as result}
                        {:keys [value base-id tx-fn]}]
                     (let [id (blob/blob-id value)
                           known? (or (contains? prepared id)
                                      (content-entity @conn id))
                           [entity prepared]
                           (if known?
                             [nil prepared]
                             (let [logical-size (bytes/length value)
                                   {:keys [chunk-threshold] :as chunk-opts}
                                   (merge default-options opts)
                                   chunked? (>= logical-size chunk-threshold)
                                   compiled (when chunked?
                                              (chunk/options chunk-opts))
                                   chunks (when chunked?
                                            (store-byte-chunks!
                                             conn value compiled opts))
                                   representation
                                   (when-not chunked?
                                     (choose-representation
                                      conn value base-id opts))
                                   {kind :kind stored-value :bytes
                                    base :base depth :depth} representation
                                   payload-id (when stored-value
                                                (payload/put!
                                                 conn stored-value opts))
                                   entity
                                   (if chunked?
                                     (merge
                                      {:geschichte.content/id id
                                       :geschichte.content/depth 0
                                       :geschichte.content/size
                                       (long logical-size)}
                                      (chunk-metadata compiled chunks))
                                     (cond->
                                      {:geschichte.content/id id
                                       :geschichte.content/kind kind
                                       :geschichte.content/payload payload-id
                                       :geschichte.content/depth (long depth)
                                       :geschichte.content/size
                                       (long logical-size)}
                                       base
                                       (assoc :geschichte.content/base
                                              [:geschichte.content/id base])))]
                               [entity (assoc prepared id true)]))]
                       (cond-> (-> result
                                   (assoc :prepared prepared)
                                   (update :publications into (tx-fn id))
                                   (update :ids conj id))
                         entity (update :entities conj entity))))
                   {:prepared {} :entities [] :publications [] :ids []}
                   items)
                  tx-data (into entities publications)]
              (when (seq tx-data) (d/transact conn tx-data))
              ids)
            (finally (guard/done! sid token))))))))

#?(:clj
   (defn- chunking-input-stream
     "Wrap `source`, publishing exactly the bytes returned by each read as chunks.
     Returns `[stream flush!]`; `flush!` publishes the final partial chunk."
     [conn source compiled opts]
     (let [max-size (:chunk-max-size compiled)
           state (volatile! {:buffer (byte-array (int max-size))
                             :used 0
                             :index 0
                             :offset 0
                             :detector chunk/initial-state
                             :chunks []})
           publish! (fn [buffer size]
                      (let [{:keys [index offset chunks]} @state
                            value (Arrays/copyOf ^bytes buffer (int size))
                            payload-id (payload/put! conn value opts)]
                        (vswap! state assoc
                                :buffer (byte-array (int max-size))
                                :used 0
                                :index (inc index)
                                :offset (+ offset size)
                                :chunks
                                (conj chunks
                                      {:geschichte.chunk/index (long index)
                                       :geschichte.chunk/offset (long offset)
                                       :geschichte.chunk/payload payload-id
                                       :geschichte.chunk/size (long size)}))))
           capture! (fn [^bytes input offset length]
                      (let [{next-detector :state cuts :cuts}
                            (chunk/scan (:detector @state) input offset length compiled)
                            end (+ offset length)]
                        (loop [cursor offset remaining (seq cuts)]
                          (let [segment-end (or (first remaining) end)
                                n (- segment-end cursor)
                                {:keys [buffer used]} @state]
                            (when (pos? n)
                              (System/arraycopy input (int cursor) buffer (int used) (int n))
                              (vswap! state update :used + n))
                            (if remaining
                              (do
                                (publish! (:buffer @state) (:used @state))
                                (recur segment-end (next remaining)))
                              (vswap! state assoc :detector next-detector))))))
           stream (proxy [FilterInputStream hasch.benc.PHashCoercion] [source]
                    (_coerce [md-create-fn _write-handlers]
                      (hasch-platform/input-stream->binary-coercion
                       this md-create-fn))
                    (read
                      ([]
                       (let [value (.read ^java.io.InputStream source)]
                         (when-not (neg? value)
                           (capture! (byte-array [(unchecked-byte value)]) 0 1))
                         value))
                      ([buffer]
                       (let [n (.read ^java.io.InputStream source buffer)]
                         (when (pos? n) (capture! buffer 0 n))
                         n))
                      ([buffer offset length]
                       (let [n (.read ^java.io.InputStream source buffer offset length)]
                         (when (pos? n) (capture! buffer offset n))
                         n))))
           flush! (fn []
                    (when (pos? (:used @state))
                      (publish! (:buffer @state) (:used @state)))
                    (:chunks @state))]
       [stream flush!])))

#?(:clj
   (defn transact-file!
     "Ingest a file with bounded heap use and transact metadata through `tx-fn`.

     Files below `:chunk-threshold` use the normal text/full representation
     selector. Larger files are hashed and content-defined-chunked in one pass.
     The caller's file is never modified."
     ([conn file base-id tx-fn]
      (transact-file! conn file base-id tx-fn nil))
     ([conn file base-id tx-fn provided-opts]
      (let [opts (merge default-options (execution/opts provided-opts))
            ^File file (io/file file)
            logical-size (.length file)]
        (when-not (:sync? opts)
          (throw (ex-info "Asynchronous JVM file ingestion is not implemented"
                          {:file file})))
        (if (< logical-size (:chunk-threshold opts))
          (with-open [in (FileInputStream. file)]
            (transact-content! conn (.readAllBytes in) base-id tx-fn opts))
          (let [sid (store-id conn)
                token (guard/writing! sid)]
            (try
              (let [[id compiled chunks]
                    (with-open [source (FileInputStream. file)]
                      (let [compiled (chunk/options opts)
                            [in flush!] (chunking-input-stream
                                         conn source compiled opts)
                            id (blob/blob-id in)]
                        [id compiled (flush!)]))]
                (if (content-entity @conn id)
                  (do (transact-data conn (vec (tx-fn id)) opts) id)
                  (let [entity (merge
                                {:geschichte.content/id id
                                 :geschichte.content/depth 0
                                 :geschichte.content/size (long logical-size)}
                                (chunk-metadata compiled chunks))]
                    (transact-data conn (into [entity] (tx-fn id)) opts)
                    id)))
              (finally
                (guard/done! sid token)))))))))

#?(:clj
   (defn copy-by-id!
     "Copy logical content to `out`, retaining only one bounded chunk in heap."
     [conn id ^OutputStream out]
     (consume-by-id! conn id
                     (fn [value]
                       (.write out ^bytes value 0 (alength ^bytes value))))
     out))
