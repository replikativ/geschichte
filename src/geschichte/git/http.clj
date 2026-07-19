(ns geschichte.git.http
  "Git smart-HTTP transport for fetch and push. Protocol and pack semantics stay
  in their own namespaces; `send-fn` is injectable for tests and alternate HTTP
  stacks."
  (:require [clojure.string :as str]
            [geschichte.git.client :as client]
            [geschichte.git.pkt-line :as pkt]
            [geschichte.git.protocol-v2 :as protocol]
            [geschichte.git.receive-pack :as receive]
            [geschichte.git.store :as store])
  (:import [java.io ByteArrayInputStream InputStream]
           [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers]))

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

(defn send-http
  "Default synchronous Java HTTP client. Returns {:status :headers :body}."
  [{:keys [method url headers body]}]
  (let [builder (HttpRequest/newBuilder (URI/create url))
        _ (doseq [[name value] headers] (.header builder name value))
        request (case method
                  :get (.GET builder)
                  :post (.POST builder
                               (HttpRequest$BodyPublishers/ofByteArray
                                ^bytes (or body (byte-array 0))))
                  (throw (ex-info "Unsupported HTTP method" {:method method})))
        response (.send ^HttpClient @http-client (.build request)
                        (HttpResponse$BodyHandlers/ofInputStream))]
    {:status (.statusCode response)
     :headers (into {}
                    (map (fn [[name values]]
                           [(str/lower-case name) (str/join "," values)]))
                    (.map (.headers response)))
     :body (.body response)}))

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

(defn- validate-response! [response expected-content-type]
  (when-not (= 200 (:status response))
    (let [body (with-open [input (body-input (:body response))]
                 (String. ^bytes (read-limited! input max-error-response-bytes)
                          "UTF-8"))]
      (throw (ex-info "Git smart-HTTP request failed"
                      {:status (:status response) :body body}))))
  (when-let [actual (header response "content-type")]
    (when-not (str/starts-with? actual expected-content-type)
      (throw (ex-info "Unexpected Git smart-HTTP content type"
                      {:expected expected-content-type :actual actual}))))
  (:body response))

(defn- response-bytes! [response expected-content-type]
  (let [body (validate-response! response expected-content-type)]
    (with-open [input (body-input body)]
      (read-limited! input max-control-response-bytes))))

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
  ([url service] (advertise! send-http url service))
  ([send-fn url service]
   (let [response (send-fn (advertisement-request url service))
         body (response-bytes!
               response (str "application/x-" service "-advertisement"))]
     (unwrap-advertisement body service))))

(defn rpc!
  ([url service body] (rpc! send-http url service body))
  ([send-fn url service body]
   (response-bytes!
    (send-fn (rpc-request url service body))
    (str "application/x-" service "-result"))))

(defn- rpc-bytes! [send-fn url service body]
  (response-bytes! (send-fn (rpc-request url service body))
                   (str "application/x-" service "-result")))

(defn- rpc-with-input! [send-fn url service body consume]
  (let [response (send-fn (rpc-request url service body))
        body (validate-response! response
                                 (str "application/x-" service "-result"))]
    (with-open [input (body-input body)]
      (consume input))))

(defn fetch!
  "Discover refs, fetch all advertised branch tips, persist remote-tracking refs,
  and import the returned pack."
  ([conn remote url] (fetch! send-http conn remote url nil))
  ([send-fn conn remote url {:keys [prefixes]
                             :or {prefixes ["HEAD" "refs/heads/" "refs/tags/"]}
                             :as opts}]
   (let [advertisement (advertise! send-fn url "git-upload-pack")
         capabilities (protocol/parse-advertisement advertisement)
         request-capabilities
         (cond-> []
           (contains? capabilities "object-format")
           (conj (str "object-format=" (capabilities "object-format"))))
         refs-response
         (rpc-bytes! send-fn url "git-upload-pack"
                     (protocol/ls-refs-request
                      {:capabilities request-capabilities :prefixes prefixes
                       :unborn? (some? (capabilities "ls-refs"))}))
         refs (protocol/parse-ls-refs refs-response)
         wants (->> refs (keep :oid) distinct vec)]
     (if (empty? wants)
       (do (store/record-refs! conn remote refs)
           {:refs refs :objects 0 :persisted 0 :unborn? true})
       (let [imported
             (rpc-with-input!
              send-fn url "git-upload-pack"
              (protocol/fetch-request
               {:capabilities request-capabilities :wants wants})
              #(client/ingest-fetch-stream!
                conn % (assoc (or opts {}) :remote remote :refs refs)))]
         (assoc imported :refs refs :unborn? false))))))

(defn ls-remote!
  "Discover refs without persisting refs or fetching objects."
  ([url opts] (ls-remote! send-http url opts))
  ([send-fn url {:keys [prefixes]
                 :or {prefixes ["HEAD" "refs/heads/" "refs/tags/"]}}]
   (let [advertisement (advertise! send-fn url "git-upload-pack")
         capabilities (protocol/parse-advertisement advertisement)
         request-capabilities
         (cond-> []
           (contains? capabilities "object-format")
           (conj (str "object-format=" (capabilities "object-format"))))
         response (rpc-bytes! send-fn url "git-upload-pack"
                              (protocol/ls-refs-request
                               {:capabilities request-capabilities
                                :prefixes prefixes
                                :unborn? (some? (capabilities "ls-refs"))}))]
     (protocol/parse-ls-refs response))))

(defn push!
  "Project and push one Geschichte commit to a smart-HTTP receive-pack endpoint."
  ([conn url commit-id opts] (push! send-http conn url commit-id opts))
  ([send-fn conn url commit-id {:keys [ref] :as opts}]
   (let [ref (or ref "refs/heads/main")
         advertisement (receive/parse-advertisement
                        (advertise! send-fn url "git-receive-pack"))
         supported (:capabilities advertisement)
         desired ["report-status" "atomic" "object-format=sha1"]
         capabilities (filterv supported desired)
         old (get-in advertisement [:refs ref] receive/zero-oid)
         prepared (client/prepare-push conn commit-id
                                       (assoc opts :old old :ref ref
                                              :capabilities capabilities))
         response (rpc-bytes! send-fn url "git-receive-pack" (:request prepared))]
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
