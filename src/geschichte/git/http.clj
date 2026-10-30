(ns geschichte.git.http
  "Git smart-HTTP transport for fetch and push. Protocol and pack semantics stay
  in their own namespaces; `send-fn` is injectable for tests and alternate HTTP
  stacks."
  (:require [clojure.string :as str]
            [geschichte.git.budget :as budget]
            [geschichte.git.client :as client]
            [geschichte.git.pkt-line :as pkt]
            [geschichte.git.protocol-v2 :as protocol]
            [geschichte.git.receive-pack :as receive]
            [geschichte.git.store :as store])
  (:import [java.io ByteArrayInputStream FilterInputStream IOException InputStream]
           [java.net URI]
           [java.time Duration]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse
            HttpResponse$BodyHandlers HttpTimeoutException]
           [java.util.concurrent ExecutionException Executors ExecutorService ThreadFactory TimeUnit TimeoutException]
           [java.util.concurrent.atomic AtomicBoolean AtomicLong]))

(def ^:private max-control-response-bytes (* 16 1024 1024))
(def ^:private max-error-response-bytes (* 64 1024))

(defn- base-url [url]
  (str/replace url #"/+$" ""))

(defn advertisement-request
  [url service]
  {:method :get
   :url (str (base-url url) "/info/refs?service=" service)
   :headers
   (cond-> {"Accept" (str "application/x-" service "-advertisement")}
     (= service "git-upload-pack")
     (assoc "Git-Protocol" "version=2"))})

(defn rpc-request
  [url service body]
  {:method :post
   :url (str (base-url url) "/" service)
   :headers
   (cond-> {"Content-Type" (str "application/x-" service "-request")
            "Accept" (str "application/x-" service "-result")}
     (= service "git-upload-pack")
     (assoc "Git-Protocol" "version=2"))
   :body body})

(defonce ^:private http-client
  (delay (HttpClient/newHttpClient)))

(defn- join-watchdog! [^ExecutorService executor]
  (loop [interrupted? false]
    (let [[done? interrupted?]
          (try [(.awaitTermination executor 50 TimeUnit/MILLISECONDS) interrupted?]
               (catch InterruptedException _ [false true]))]
      (if done?
        (when interrupted? (.interrupt (Thread/currentThread)))
        (recur interrupted?)))))

(defn- timed-body
  "Own the default Java client's body and its monitor. Closing the Java HTTP
  response stream cancels its subscription and releases a blocked native read.
  This contract does not extend to arbitrary injected senders/streams."
  ^InputStream [^InputStream body operation idle-ms]
  (if-not (or operation idle-ms) body
          (let [closed (AtomicBoolean. false)
                reading (AtomicBoolean. false)
                failure (atom nil)
                last-progress (AtomicLong. (System/nanoTime))
                executor (Executors/newSingleThreadScheduledExecutor
                          (reify ThreadFactory
                            (newThread [_ runnable]
                              (doto (Thread. runnable "geschichte-http-body-watchdog")
                                (.setDaemon true)))))
                check-failure! (fn [] (when-let [error @failure] (throw error)))
                read! (fn [f]
                        (check-failure!)
                        (.set last-progress (System/nanoTime))
                        (.set reading true)
                        (try
                          (let [n (try (f)
                                       (catch IOException e
                                         (check-failure!)
                                         ;; Newer JDKs may fire their request
                                         ;; timer during body reads, before
                                         ;; the monitor's next checkpoint.
                                         (budget/check! operation)
                                         (throw e)))]
                            (check-failure!)
                            n)
                          (finally (.set reading false))))]
            (.scheduleWithFixedDelay
             executor
             ^Runnable
             (fn []
               (when-not (.get closed)
                 (try
                   (budget/check! operation)
                   (when (and idle-ms (.get reading) (>= (- (System/nanoTime) (.get last-progress))
                                                         (* (long idle-ms) 1000000)))
                     (throw (ex-info "Git HTTP response body made no progress"
                                     {:type ::body-idle-timeout :timeout-ms idle-ms})))
                   (catch Exception error
                     (compare-and-set! failure nil error)
                     ;; The native response body, rather than an arbitrary
                     ;; caller stream, is the resource this monitor may close.
                     (try
                       (when (.compareAndSet closed false true) (.close body))
                       (finally (.shutdown executor)))))))
             10 10 TimeUnit/MILLISECONDS)
            (proxy [FilterInputStream] [body]
              (read
                ([] (read! #(.read body)))
                ([buffer] (read! #(.read body ^bytes buffer 0 (alength ^bytes buffer))))
                ([buffer offset length] (read! #(.read body ^bytes buffer (int offset) (int length)))))
              (close []
                (.shutdown executor)
                (try (when (.compareAndSet closed false true) (.close body))
                     (finally (join-watchdog! executor))))))))

(defn- send-native! ^HttpResponse [request operation]
  (let [pending (.sendAsync ^HttpClient @http-client request (HttpResponse$BodyHandlers/ofInputStream))]
    (try
      (loop []
        (budget/check! operation)
        (let [response (try (.get pending 25 TimeUnit/MILLISECONDS)
                            (catch TimeoutException _ ::pending))]
          (if (= ::pending response) (recur)
              (do (budget/check! operation) response))))
      (catch Exception error
        ;; A response which wins the cancellation race still has an owned body
        ;; to close. Otherwise cancellation aborts the default client's request.
        (when-not (.cancel pending true)
          (try
            (when-let [^HttpResponse response (.getNow pending nil)] (.close ^InputStream (.body response)))
            (catch Exception _ nil)))
        (when (instance? InterruptedException error) (.interrupt (Thread/currentThread)))
        (let [cause (if (instance? ExecutionException error) (.getCause error) error)]
          (if (instance? HttpTimeoutException cause)
            (do (budget/check! operation)
                (throw (ex-info "Git HTTP request timed out" {:type ::request-timeout} cause)))
            (throw cause)))))))

(defn send-http
  "Default synchronous Java HTTP client. Returns {:status :headers :body}."
  [{:keys [method url headers body timeout-ms import-budget http-body-idle-timeout-ms]}]
  (when (and http-body-idle-timeout-ms
             (not (and (integer? http-body-idle-timeout-ms)
                       (pos? http-body-idle-timeout-ms)
                       (<= http-body-idle-timeout-ms (quot Long/MAX_VALUE 1000000)))))
    (throw (ex-info "Invalid Git HTTP body idle timeout"
                    {:type ::invalid-body-idle-timeout :timeout-ms http-body-idle-timeout-ms})))
  (let [builder (HttpRequest/newBuilder (URI/create url))
        _ (when timeout-ms (.timeout builder (Duration/ofMillis timeout-ms)))
        _ (doseq [[name value] headers] (.header builder name value))
        request (case method
                  :get (.GET builder)
                  :post (.POST builder
                               (HttpRequest$BodyPublishers/ofByteArray
                                ^bytes (or body (byte-array 0))))
                  (throw (ex-info "Unsupported HTTP method" {:method method})))
        response (send-native! (.build request) import-budget)]
    {:status (.statusCode response)
     :headers (into {}
                    (map (fn [[name values]]
                           [(str/lower-case name) (str/join "," values)]))
                    (.map (.headers response)))
     :body (timed-body (.body response) import-budget http-body-idle-timeout-ms)}))

(defn- body-input ^InputStream [body]
  (cond
    (instance? InputStream body) body
    (bytes? body) (ByteArrayInputStream. ^bytes body)
    :else (throw (ex-info "Unsupported HTTP response body"
                          {:body-type (type body)}))))

(defn- read-limited! [^InputStream input limit]
  (let [value (.readNBytes input (inc (int limit)))]
    (when (> (alength value) limit)
      (throw (ex-info "Git HTTP control response is too large"
                      {:limit limit})))
    value))

(defn- header [response name]
  (get (:headers response) (str/lower-case name)))

(defn- send! [send-fn request opts]
  (let [remaining (budget/remaining-ms (:import-budget opts))]
    (send-fn (cond-> request
               remaining (assoc :timeout-ms remaining)
               (:import-budget opts) (assoc :import-budget (:import-budget opts))
               (:http-body-idle-timeout-ms opts)
               (assoc :http-body-idle-timeout-ms (:http-body-idle-timeout-ms opts))))))

(defn- validate-response! [response expected-content-type input opts]
  (budget/check! (:import-budget opts))
  (when-not (= 200 (:status response))
    (let [body (String. ^bytes (read-limited! input max-error-response-bytes) "UTF-8")]
      (throw (ex-info "Git smart-HTTP request failed" {:status (:status response) :body body}))))
  (when-let [actual (header response "content-type")]
    (when-not (str/starts-with? actual expected-content-type)
      (throw (ex-info "Unexpected Git smart-HTTP content type"
                      {:expected expected-content-type :actual actual})))))

(defn- response-bytes! [response expected-content-type opts]
  (with-open [input (budget/input-stream (body-input (:body response)) (:import-budget opts))]
    (validate-response! response expected-content-type input opts)
    (read-limited! input max-control-response-bytes)))

(defn- unwrap-advertisement [body service]
  (let [frames (pkt/decode body)
        prelude (str "# service=" service "\n")]
    (if (= prelude (some-> (first frames) pkt/text))
      (do
        (when-not (= :flush (second frames))
          (throw (ex-info "Malformed Git smart-HTTP service prelude"
                          {:service service})))
        (pkt/encode (subvec (vec frames) 2)))
      body)))

(defn advertise!
  ([url service] (advertise! send-http url service nil))
  ([send-fn url service] (advertise! send-fn url service nil))
  ([send-fn url service opts]
   (let [response (send! send-fn (advertisement-request url service) opts)
         body (response-bytes! response (str "application/x-" service "-advertisement") opts)]
     (unwrap-advertisement body service))))

(defn rpc!
  ([url service body] (rpc! send-http url service body nil))
  ([send-fn url service body] (rpc! send-fn url service body nil))
  ([send-fn url service body opts]
   (response-bytes! (send! send-fn (rpc-request url service body) opts)
                    (str "application/x-" service "-result") opts)))

(defn- rpc-bytes! [send-fn url service body opts]
  (rpc! send-fn url service body opts))

(defn- rpc-with-input! [send-fn url service body consume opts]
  (let [response (send! send-fn (rpc-request url service body) opts)]
    (with-open [input (budget/input-stream (body-input (:body response)) (:import-budget opts))]
      (validate-response! response (str "application/x-" service "-result") input opts)
      (consume input))))

(defn fetch!
  "Discover refs, fetch all advertised branch tips, persist remote-tracking refs,
  and import the returned pack. Optional :haves supplies commit OIDs whose full
  reachable object graph the caller already holds. It is not inferred from
  advertised refs or a partially materialized history."
  ([conn remote url] (fetch! send-http conn remote url nil))
  ([conn remote url opts] (fetch! send-http conn remote url opts))
  ([send-fn conn remote url {:keys [prefixes haves]
                             :or {prefixes ["HEAD" "refs/heads/" "refs/tags/"]}
                             :as opts}]
   (let [advertisement (advertise! send-fn url "git-upload-pack" opts)
         capabilities (protocol/parse-advertisement advertisement)
         request-capabilities
         (cond-> []
           (contains? capabilities "object-format")
           (conj (str "object-format=" (capabilities "object-format"))))
         refs-response
         (rpc-bytes! send-fn url "git-upload-pack"
                     (protocol/ls-refs-request
                      {:capabilities request-capabilities :prefixes prefixes
                       :unborn? (some? (capabilities "ls-refs"))}) opts)
         refs (protocol/parse-ls-refs refs-response)
         wants (->> refs (keep :oid) distinct vec)]
     (if (empty? wants)
       (do (budget/check! (:import-budget opts))
           (store/record-refs! conn remote refs)
           {:refs refs :objects 0 :persisted 0 :unborn? true})
       (let [imported
             (rpc-with-input!
              send-fn url "git-upload-pack"
              (protocol/fetch-request
               {:capabilities request-capabilities :wants wants :haves haves})
              #(client/ingest-fetch-stream!
                conn % (assoc (or opts {}) :remote remote :refs refs)) opts)]
         (assoc imported :refs refs :unborn? false))))))

(defn ls-remote!
  "Discover refs without persisting refs or fetching objects."
  ([url opts] (ls-remote! send-http url opts))
  ([send-fn url {:keys [prefixes]
                 :or {prefixes ["HEAD" "refs/heads/" "refs/tags/"]} :as opts}]
   (let [advertisement (advertise! send-fn url "git-upload-pack" opts)
         capabilities (protocol/parse-advertisement advertisement)
         request-capabilities
         (cond-> []
           (contains? capabilities "object-format")
           (conj (str "object-format=" (capabilities "object-format"))))
         response (rpc-bytes! send-fn url "git-upload-pack"
                              (protocol/ls-refs-request
                               {:capabilities request-capabilities
                                :prefixes prefixes
                                :unborn? (some? (capabilities "ls-refs"))}) opts)]
     (protocol/parse-ls-refs response))))

(defn push!
  "Project and push one Geschichte commit to a smart-HTTP receive-pack endpoint."
  ([conn url commit-id opts] (push! send-http conn url commit-id opts))
  ([send-fn conn url commit-id {:keys [ref] :as opts}]
   (when (:import-budget opts)
     (throw (ex-info "Import budgets do not bound Git push projection"
                     {:type ::unsupported-push-budget})))
   (let [ref (or ref "refs/heads/main")
         advertisement (receive/parse-advertisement
                        (advertise! send-fn url "git-receive-pack" opts))
         supported (:capabilities advertisement)
         desired ["report-status" "atomic" "object-format=sha1"]
         capabilities (filterv supported desired)
         old (get-in advertisement [:refs ref] receive/zero-oid)
         prepared (client/prepare-push conn commit-id
                                       (assoc opts :old old :ref ref
                                              :capabilities capabilities))
         response (rpc-bytes! send-fn url "git-receive-pack" (:request prepared) opts)]
     (assoc prepared :report (receive/parse-report response)))))

(defn clone!
  "Fetch and lazily materialize the advertised HEAD."
  ([conn remote url] (clone! send-http conn remote url nil))
  ([conn remote url opts] (clone! send-http conn remote url opts))
  ([send-fn conn remote url opts]
   (store/with-store
     (or (:object-store opts) conn)
     (fn [object-store]
       (let [opts (assoc (or opts {}) :object-store object-store)]
         (let [fetched (fetch! send-fn conn remote url opts)]
           (if (:no-checkout? opts)
             (assoc fetched :checkout/skipped? true)
             (client/checkout-fetched! conn fetched opts))))))))

(defn pull!
  "Fetch then apply explicit fast-forward/merge pull policy."
  ([conn remote url opts] (pull! send-http conn remote url opts))
  ([send-fn conn remote url opts]
   (store/with-store
     (or (:object-store opts) conn)
     (fn [object-store]
       (let [opts (assoc (or opts {}) :object-store object-store)]
         (client/apply-pull! conn remote
                             (fetch! send-fn conn remote url opts) opts))))))
