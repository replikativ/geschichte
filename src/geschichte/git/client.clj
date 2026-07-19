(ns geschichte.git.client
  "Transport-neutral Git client composition. HTTP, SSH, and local-process
  transports exchange request/response bytes at this boundary."
  (:require [geschichte.git.checkout :as checkout]
            [geschichte.git.object :as object]
            [geschichte.git.pack :as pack]
            [geschichte.git.project :as project]
            [geschichte.git.protocol-v2 :as protocol]
            [geschichte.git.receive-pack :as receive]
            [geschichte.git.store :as store]
            [geschichte.merge :as merge]
            [geschichte.repo :as repo]))

(defn ingest-fetch!
  "Extract and import the pack from a protocol-v2 fetch response."
  ([conn response] (ingest-fetch! conn response nil))
  ([conn response opts]
   (let [{:keys [pack progress]} (protocol/parse-fetch-response response)
         imported (store/import-pack! (or (:object-store opts) conn) pack opts)]
     (assoc imported :progress progress))))

(defn ingest-fetch-stream!
  "Incrementally demultiplex and import a protocol-v2 fetch response stream."
  ([conn input] (ingest-fetch-stream! conn input nil))
  ([conn input opts]
   (store/import-pack-producer!
    (or (:object-store opts) conn)
    (fn [write!]
      (protocol/consume-fetch-response! input write! opts))
    opts)))

(defn ingest-ls-refs!
  "Parse and store an ls-refs response under a remote name."
  [conn remote response]
  (let [refs (protocol/parse-ls-refs response)]
    (store/record-refs! conn remote refs)
    refs))

(defn prepare-push
  "Project a Geschichte commit and build a receive-pack request. The caller sends
  `:request` through its chosen transport and parses the response with
  `receive-pack/parse-report`."
  [conn commit-id {:keys [old ref capabilities project-options object-store]
                   :or {old receive/zero-oid
                        ref "refs/heads/main"
                        capabilities ["report-status" "object-format=sha1"]}}]
  (if commit-id
    (let [graph (project/project conn commit-id
                                 (cond-> (or project-options {})
                                   object-store (assoc :object-store object-store)))
          packed (pack/encode (:objects graph))]
      {:oid (:oid graph)
       :objects (count (:objects graph))
       :pack-bytes (alength ^bytes packed)
       :request
       (receive/request
        {:updates [{:old old :new (:oid graph) :ref ref}]
         :capabilities capabilities
         :pack packed})})
    {:oid receive/zero-oid
     :objects 0
     :pack-bytes 0
     :request (receive/request
               {:updates [{:old old :new receive/zero-oid :ref ref}]
                :capabilities capabilities})}))

(defn checkout-fetched!
  "Materialize the fetched HEAD (or `:oid`) as a local Geschichte branch."
  ([conn fetched] (checkout-fetched! conn fetched nil))
  ([conn fetched {:keys [oid ref force? branch object-format object-store
                         history-materialization-limit]}]
   (let [head (some #(when (= "HEAD" (:ref %)) %) (:refs fetched))
         branch-ref (when branch (str "refs/heads/" branch))
         branch-head (when branch-ref
                       (some #(when (= branch-ref (:ref %)) %) (:refs fetched)))
         _ (when (and branch (nil? branch-head))
             (throw (ex-info (str "Remote branch " branch " not found")
                             {:branch branch :refs (:refs fetched)})))
         oid (or oid (:oid branch-head) (:oid head))
         source-ref (or branch-ref
                        (get-in head [:attributes :symref-target]))
         ref (or ref source-ref "refs/heads/main")]
     (when-not oid
       (throw (ex-info "Fetched repository has no checkout target"
                       {:refs (:refs fetched)})))
     (assoc (checkout/materialize! conn oid {:ref ref :force? force?
                                             :object-format (or object-format
                                                                :sha1)
                                             :object-store object-store
                                             :history-materialization-limit
                                             (or history-materialization-limit
                                                 2048)})
            :fetched fetched))))

(defn- fetched-head [fetched]
  (or (some #(when (= "HEAD" (:ref %)) %) (:refs fetched))
      (some #(when (.startsWith ^String (:ref %) "refs/heads/") %)
            (:refs fetched))))

(defn- ensure-current-oid! [conn object-store commit]
  (or (:geschichte.commit/git-oid
       (repo/commit-by-id conn (:geschichte.commit/id commit)))
      (let [graph (project/project conn (:geschichte.commit/id commit)
                                   {:object-store object-store})]
        (store/persist-graph! (or object-store conn) graph)
        (:oid graph))))

(defn- git-ancestor?
  [conn object-store ancestor descendant]
  (loop [pending [descendant] seen #{}]
    (if-let [oid (peek pending)]
      (cond
        (= oid ancestor) true
        (seen oid) (recur (pop pending) seen)
        :else
        (let [metadata (store/read-object (or object-store conn) oid)]
          (when-not (= :commit (:geschichte.git.object/type metadata))
            (throw (ex-info "Git ancestry requires a stored commit graph"
                            {:oid oid :type (:geschichte.git.object/type metadata)})))
          (let [parents (:parents
                         (object/parse-commit (:payload metadata)))]
            (recur (into (pop pending) parents) (conj seen oid)))))
      false)))

(defn apply-pull!
  "Apply an already-fetched result with explicit `:ff-only` or `:merge` policy.
  Ancestry is decided from exact Git objects before worktree mutation."
  ([conn remote fetched] (apply-pull! conn remote fetched nil))
  ([conn remote fetched {:keys [policy ref object-store]
                         :or {policy :ff-only}}]
   (when-not (:clean? (repo/status conn))
     (throw (ex-info "Pull requires a clean worktree and index"
                     {:status (repo/status conn)})))
   (let [remote-head (fetched-head fetched)
         remote-oid (:oid remote-head)
         current-ref (repo/current-ref conn)
         current (repo/head-commit conn)
         current-id (:geschichte.commit/id current)]
     (when-not remote-oid
       (throw (ex-info "Fetched remote has no branch tip" {:remote remote})))
     (if-not current
       (assoc (checkout-fetched! conn fetched {:oid remote-oid
                                               :ref (or ref current-ref)})
              :pull/status :initialized)
       (let [current-oid (ensure-current-oid! conn object-store current)]
         (cond
           (= current-oid remote-oid)
           {:pull/status :up-to-date :oid current-oid}

           (git-ancestor? conn object-store current-oid remote-oid)
           (assoc (checkout/materialize! conn remote-oid
                                         {:ref (or ref current-ref)})
                  :pull/status :fast-forward
                  :from current-oid :to remote-oid)

           (git-ancestor? conn object-store remote-oid current-oid)
           {:pull/status :local-ahead :oid current-oid :remote remote-oid}

           (= policy :ff-only)
           (throw (ex-info "Pull is not a fast-forward"
                           {:current current-oid :remote remote-oid
                            :policy policy}))

           (= policy :merge)
           (let [temporary-ref (str "refs/pull/" remote)
                 remote-commit
                 (checkout/materialize! conn remote-oid {:ref temporary-ref})
                 remote-id (:commit remote-commit)]
             (repo/checkout! conn current-ref)
             (assoc (merge/prepare! conn current-id remote-id)
                    :pull/status :merge-prepared
                    :from current-oid :to remote-oid))

           :else
           (throw (ex-info "Unsupported pull policy" {:policy policy}))))))))
