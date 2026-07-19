(ns geschichte.git.ssh
  "Stateful Git process transport used for SSH and local upload-pack validation."
  (:require [geschichte.git.client :as client]
            [geschichte.git.pkt-line :as pkt]
            [geschichte.git.protocol-v2 :as protocol]
            [geschichte.git.receive-pack :as receive]
            [geschichte.git.store :as store])
  (:import [java.io InputStream]))

(defn- shell-quote [value]
  (str "'" (.replace ^String (str value) "'" "'\\''") "'"))

(defn ssh-argv
  "Build an SSH argv without passing host/options through a local shell."
  [{:keys [host path service port identity-file options]
    :or {service "git-upload-pack"}}]
  (vec
   (concat ["ssh"]
           (when port ["-p" (str port)])
           (when identity-file ["-i" identity-file])
           options
           ["-o" "SendEnv=GIT_PROTOCOL" host
            (str service " " (shell-quote path))])))

(defn local-argv
  "Build an argv for native local service validation."
  [{:keys [path service] :or {service "git-upload-pack"}}]
  ["git" (case service
           "git-upload-pack" "upload-pack"
           "git-receive-pack" "receive-pack")
   path])

(declare session-consume-with!)

(defn session-with!
  "Run one stateful command. `request-fn` receives advertisement bytes and
  returns request bytes to send on the same process."
  [argv-fn service request-fn]
  (let [session
        (session-consume-with!
         argv-fn service request-fn
         (fn [^InputStream input] (.readAllBytes input)))]
    (assoc session :response (:result session))))

(defn session-consume-with!
  "Run a stateful Git service and consume its response before process teardown.

  `consume` receives the live stdout InputStream, allowing fetch packs to be
  demultiplexed with bounded memory. Stderr is drained concurrently so an SSH
  diagnostic cannot deadlock a large transfer."
  [argv-fn service request-fn consume]
  (let [builder (ProcessBuilder. ^java.util.List (argv-fn service))
        _ (when (= service "git-upload-pack")
            (.put (.environment builder) "GIT_PROTOCOL" "version=2"))
        process (.start builder)
        input ^InputStream (.getInputStream process)
        stderr (future (slurp (.getErrorStream process)))
        advertisement-frames (pkt/read-through-flush! input)
        advertisement (pkt/encode advertisement-frames)
        request (request-fn advertisement)]
    (with-open [output (.getOutputStream process)]
      (.write output ^bytes request))
    (let [result (consume input)
          exit (.waitFor process)]
      (when-not (zero? exit)
        (throw (ex-info "Git SSH/process service failed"
                        {:service service :exit exit :stderr @stderr})))
      {:advertisement advertisement
       :result result
       :stderr @stderr})))

(defn session!
  "Run one stateful command with already-constructed request bytes."
  [argv-fn service request]
  (session-with! argv-fn service (constantly request)))

(defn fetch!
  "Fetch all branch/tag tips over stateful upload-pack. `argv-fn` may launch SSH
  or a local Git service and is invoked once per protocol-v2 command."
  ([conn remote argv-fn] (fetch! conn remote argv-fn nil))
  ([conn remote argv-fn {:keys [prefixes]
                         :or {prefixes ["HEAD" "refs/heads/" "refs/tags/"]}
                         :as opts}]
   (let [probe (session! argv-fn "git-upload-pack"
                         (protocol/ls-refs-request {:prefixes prefixes}))
         capabilities (protocol/parse-advertisement (:advertisement probe))
         refs (protocol/parse-ls-refs (:response probe))
         wants (->> refs (keep :oid) distinct vec)
         request-capabilities
         (cond-> []
           (contains? capabilities "object-format")
           (conj (str "object-format=" (capabilities "object-format"))))]
     (if (empty? wants)
       (do (store/record-refs! conn remote refs)
           {:refs refs :objects 0 :persisted 0 :unborn? true})
       (let [request (protocol/fetch-request
                      {:capabilities request-capabilities :wants wants})
             fetch-session
             (session-consume-with!
              argv-fn "git-upload-pack" (constantly request)
              #(client/ingest-fetch-stream!
                conn % (assoc (or opts {}) :remote remote :refs refs)))]
         (assoc (:result fetch-session) :refs refs :unborn? false))))))

(defn ls-remote!
  "Discover refs over SSH/local upload-pack without persisting or fetching."
  [argv-fn {:keys [prefixes]
            :or {prefixes ["HEAD" "refs/heads/" "refs/tags/"]}}]
  (let [probe (session! argv-fn "git-upload-pack"
                        (protocol/ls-refs-request {:prefixes prefixes}))]
    (protocol/parse-ls-refs (:response probe))))

(defn push!
  "Push one Geschichte commit over stateful receive-pack."
  [conn commit-id argv-fn {:keys [ref] :as opts}]
  (let [prepared (atom nil)
        session
        (session-with!
         argv-fn "git-receive-pack"
         (fn [advertisement-bytes]
           (let [advertisement (receive/parse-advertisement advertisement-bytes)
                 ref (or ref "refs/heads/main")
                 supported (:capabilities advertisement)
                 capabilities (filterv supported
                                       ["report-status" "atomic"
                                        "object-format=sha1"])
                 request (client/prepare-push
                          conn commit-id
                          (assoc opts :ref ref
                                 :old (get-in advertisement [:refs ref]
                                              receive/zero-oid)
                                 :capabilities capabilities))]
             (reset! prepared request)
             (:request request))))]
    (assoc @prepared :report (receive/parse-report (:response session)))))

(defn clone!
  "Fetch and lazily materialize the advertised HEAD over stateful transport."
  [conn remote argv-fn opts]
  (store/with-store
    (or (:object-store opts) conn)
    (fn [object-store]
      (let [opts (assoc (or opts {}) :object-store object-store)]
        (let [fetched (fetch! conn remote argv-fn opts)]
          (if (:no-checkout? opts)
            (assoc fetched :checkout/skipped? true)
            (client/checkout-fetched! conn fetched opts)))))))

(defn pull!
  "Fetch then apply explicit fast-forward/merge pull policy."
  [conn remote argv-fn opts]
  (store/with-store
    (or (:object-store opts) conn)
    (fn [object-store]
      (let [opts (assoc (or opts {}) :object-store object-store)]
        (client/apply-pull! conn remote (fetch! conn remote argv-fn opts) opts)))))
