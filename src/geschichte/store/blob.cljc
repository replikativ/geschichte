(ns geschichte.store.blob
  "Immutable payload operations in a Datahike database's Konserve store."
  (:refer-clojure :exclude [await])
  (:require [datahike.blob :as blob]
            [geschichte.async :as execution]
            [geschichte.bytes :as bytes]
            [geschichte.budget :as budget]
            [is.simm.partial-cps.async :refer [await]]
            [konserve.core :as k]
            #?(:cljs [cljs.core.async :as core-async]))
  #?(:cljs (:require-macros [geschichte.macros :refer [async+sync]]
                            [is.simm.partial-cps.async :refer [async]]))
  #?(:clj (:require [geschichte.macros :refer [async+sync]]
                    [is.simm.partial-cps.async :refer [async]]))
  #?(:clj (:import [java.io ByteArrayOutputStream InputStream]
                   [java.util.concurrent Callable CancellationException
                    ExecutionException ExecutorService Executors Future])))

(def default-write-parallelism 4)

(defn content-id
  "Return Datahike's content-addressed UUID for a byte buffer."
  [value]
  (blob/blob-id value))

(defn put!
  "Store bytes without yet publishing a datom reference.

  Returns the content ID directly on the JVM and a partial-cps computation in
  ClojureScript. The caller must keep Datahike's unreferenced-write guard open
  until the transaction publishing the returned ID has landed."
  ([conn value] (put! conn value nil))
  ([conn value provided-opts]
   (let [opts (execution/opts provided-opts)
         id (content-id value)]
     (async+sync (:sync? opts)
                 (async
                  (await (execution/io-result
                          (k/bassoc (:store @conn) id value opts)
                          opts))
                  id)))))

#?(:clj
   (defn- settled-future [^Future future]
     (loop [interrupted? false]
       (let [[status value]
             (try
               [:value (.get future)]
               (catch InterruptedException error [:interrupted error])
               (catch ExecutionException error [:error (.getCause error)])
               (catch CancellationException error [:error error]))]
         (if (= :interrupted status)
           ;; The enclosing GC guard must remain open until every backend write
           ;; has actually stopped. Restore interruption after joining them all.
           (recur true)
           (cond-> {:interrupted? interrupted?}
             (= :value status) (assoc :value value)
             (= :error status) (assoc :error value)))))))

#?(:clj
   (defn- put-many-sync! [conn values opts]
     (let [values (vec values)
           parallelism (min (count values)
                            (max 1 (long (or (:blob-write-parallelism opts)
                                             default-write-parallelism))))]
       (if (<= parallelism 1)
         (mapv #(put! conn % opts) values)
         (let [^ExecutorService executor
               (Executors/newFixedThreadPool (int parallelism))]
           (try
             (let [futures
                   (mapv (fn [value]
                           (.submit executor
                                    ^Callable
                                    (fn [] (put! conn value opts))))
                         values)]
               (let [settled (mapv settled-future futures)
                     interrupted? (some :interrupted? settled)
                     error (some :error settled)]
                 (when interrupted?
                   (.interrupt (Thread/currentThread)))
                 (cond
                   error (throw error)
                   interrupted? (throw (InterruptedException.
                                        "Interrupted while publishing blobs"))
                   :else (mapv :value settled))))
             (finally
               (.shutdown executor))))))))

(defn put-many!
  "Store immutable byte values, preserving input order.

  JVM writes use a bounded executor because independent Konserve keys can be
  written concurrently. CLJS retains its non-blocking partial-CPS contract and
  currently sequences the writes; callers receive the same ordered IDs. The GC
  guard remains the responsibility of the operation that later publishes all
  returned store-refs."
  ([conn values] (put-many! conn values nil))
  ([conn values provided-opts]
   (let [opts (execution/opts provided-opts)]
     #?(:clj (put-many-sync! conn values opts)
        :cljs
        (async+sync (:sync? opts)
                    (async
                     (loop [remaining (seq values) ids []]
                       (if-let [value (first remaining)]
                         (recur (next remaining)
                                (conj ids (await (put! conn value opts))))
                         ids))))))))

(defn- account-read! [operation size]
  (budget/bound! operation :object-bytes size)
  (budget/charge! operation :content-read-bytes size))

#?(:clj
   (defn- bounded-stream [^InputStream input operation]
     (let [scratch (byte-array 8192)
           output (ByteArrayOutputStream.)]
       (loop [size 0]
         (budget/check! operation)
         (let [n (.read input scratch)]
           (budget/check! operation)
           (if (neg? n)
             (.toByteArray output)
             (do (budget/bound! operation :object-bytes (+ size n))
                 (budget/charge! operation :content-read-bytes n)
                 (.write output scratch 0 n)
                 (recur (+ size n)))))))))

#?(:clj
   (defn- sync-binary [{:keys [input-stream blob] :as value} operation]
     (cond
       (bytes/byte-buffer? value) (do (account-read! operation (bytes/length value)) value)
       (bytes/byte-buffer? input-stream) (do (account-read! operation (bytes/length input-stream)) input-stream)
       input-stream (if operation (bounded-stream input-stream operation)
                        (.readAllBytes ^java.io.InputStream input-stream))
       (bytes/byte-buffer? blob) (do (account-read! operation (bytes/length blob)) blob)
       :else (throw (ex-info "Unsupported Konserve binary value"
                             {:value-type (type value)})))))

#?(:cljs
   (defn- value-channel [value]
     (let [out (core-async/promise-chan)]
       (core-async/put! out value)
       out)))

#?(:cljs
   (defn- stream-channel [stream]
     (let [out (core-async/promise-chan)
           chunks (atom [])]
       (.on ^js stream "data" #(swap! chunks conj %))
       (.once ^js stream "end"
              #(core-async/put!
                out
                (if (seq @chunks)
                  (apply bytes/concat-bytes @chunks)
                  (bytes/empty-bytes))))
       (.once ^js stream "error" #(core-async/put! out %))
       out)))

#?(:cljs
   (defn- async-binary [{:keys [input-stream blob] :as value} operation]
     (try
       (cond
         (bytes/byte-buffer? value) (do (account-read! operation (bytes/length value)) (value-channel value))
         (bytes/byte-buffer? input-stream) (do (account-read! operation (bytes/length input-stream)) (value-channel input-stream))
         (bytes/byte-buffer? blob) (do (account-read! operation (bytes/length blob)) (value-channel blob))
         input-stream (if operation
                        (budget/unsupported! operation :node-borrowed-stream)
                        (stream-channel input-stream))
         :else (value-channel
                (ex-info "Unsupported Konserve binary value"
                         {:value-type (type value)})))
       (catch :default error (value-channel error)))))

(defn get-bytes
  "Read a binary payload by content ID.

  Returns bytes (or nil) directly on the JVM and a partial-cps computation in
  ClojureScript. Node file stores are consumed while Konserve holds its lock."
  ([conn id] (get-bytes conn id nil))
  ([conn id provided-opts]
   (let [opts (execution/opts provided-opts)]
     (when id
       (async+sync (:sync? opts)
                   (async
                    (await
                     (execution/io-result
                      (k/bget (:store (or (:read-db opts) @conn)) id
                              #?(:clj #(sync-binary % (:merge-budget opts))
                                 :cljs #(async-binary % (:merge-budget opts)))
                              (dissoc opts :merge-budget :read-db :content-seen :content-depth))
                      opts))))))))
