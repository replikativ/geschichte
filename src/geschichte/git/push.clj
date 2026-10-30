(ns geschichte.git.push
  "Opt-in bounded incremental Git export. Verified imported receiver ancestry
   is an explicit boundary; unrelated imported histories refuse rather than
   silently regenerating their Git identities. Preparation grants no network
   authority and never mutates the source."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [geschichte.budget :as operation]
            [geschichte.content :as content]
            [geschichte.git.budget :as budget]
            [geschichte.git.http :as http]
            [geschichte.git.object :as object]
            [geschichte.git.pack :as pack]
            [geschichte.git.pkt-line :as pkt]
            [geschichte.git.receive-pack :as receive]
            [geschichte.merge.policy :as merge-policy]
            [geschichte.text-budget :as text]
            [geschichte.tree :as tree]))

(def projection :geschichte.git.push/canonical-v1)

(defn- refuse! [reason]
  (throw (ex-info "Bounded Git push refused" {:type ::refused :reason reason})))

(defn oid? [value] (and (string? value) (boolean (re-matches #"[0-9a-f]{40}" value))))

(defn ref? [value]
  (and (string? value) (<= 12 (count value) 512) (str/starts-with? value "refs/heads/")
       (not (re-find #"[\s\p{Cntrl}~^:?*\[\\]" value))
       (not (some #(or (str/blank? %) (str/starts-with? % ".") (str/ends-with? % ".lock")) (str/split value #"/" -1)))
       (not (str/includes? value "..")) (not (str/includes? value "@{")) (not (str/ends-with? value "."))))

(def recommended-limits
  (merge (:limits (merge-policy/recommended-policy))
         {:objects 50000 :pack-bytes (* 16 1024 1024) :response-bytes (* 2 1024 1024)
          :tree-depth 32 :object-bytes (* 4 1024 1024)}))

(defn budget-profile []
  {:id ::incremental-push :version 1 :required-limits (vec (sort (keys recommended-limits)))
   :deadline-required? true :projection projection})

(defn recommended-policy
  ([] (recommended-policy 120000))
  ([timeout-ms] {:limits recommended-limits :timeout-ms timeout-ms}))

(defn- require! [op]
  (operation/require-limits! op (:required-limits (budget-profile))))

(defn- commit! [db id op]
  (budget/charge! op :commit-visits 1)
  (when-not (uuid? id) (refuse! :missing-history))
  (let [attrs [:db/id :geschichte.commit/id :geschichte.commit/git-oid :geschichte.commit/snapshot
               :geschichte.commit/message :geschichte.commit/author :geschichte.commit/time]
        row (d/pull db attrs [:geschichte.commit/id id])]
    (when-not (= id (:geschichte.commit/id row)) (refuse! :missing-history))
    (let [edges (vec (take 17 (d/datoms db :eavt (:db/id row) :geschichte.commit/parents)))]
      (when (> (count edges) 16) (refuse! :parent-limit))
      (budget/charge! op :graph-edges (count edges))
      (assoc row :parents
             (mapv (fn [edge]
                     (let [parent (:geschichte.commit/id (d/pull db [:geschichte.commit/id] (:v edge)))]
                       (when-not (uuid? parent) (refuse! :missing-history)) parent)) edges)))))

(defn- receiver-known [db upstream old op]
  (if (= receive/zero-oid old)
    {}
    (do
      (when-not (= old (:geschichte.commit/git-oid (commit! db upstream op))) (refuse! :upstream-mismatch))
      (loop [pending [upstream] known {}]
        (if-let [id (peek pending)]
          (if (contains? known id)
            (recur (pop pending) known)
            (let [row (commit! db id op) oid (:geschichte.commit/git-oid row)]
              (when-not (oid? oid) (refuse! :unverified-upstream-history))
              ;; Imported frontiers have a verified OID but no snapshot. They
              ;; are receiver-known boundaries, never fabricated root commits.
              ;; Ordinary imported ancestors may retain parents/metadata but no
              ;; snapshot. Only the actual id/OID frontier has no parent edges.
              (recur (into (pop pending) (:parents row))
                     (assoc known id oid))))
          known)))))

(defn- snapshot-tree [db row op]
  (let [snapshot (d/commit-as-db db (:geschichte.commit/snapshot row))]
    (when-not (and snapshot (= (dissoc (:config db) :branch) (dissoc (:config snapshot) :branch)))
      (refuse! :missing-snapshot))
    (let [entries
          (reduce
           (fn [out datom]
             (budget/charge! op :tree-entries 1)
             (let [path (:v datom)]
               (when-not (and (string? path) (<= 1 (count path) 4096)) (refuse! :invalid-tree))
               (let [path-bytes (text/utf8-size op path)]
                 (budget/bound! op :path-bytes path-bytes)
                 (budget/charge! op :tree-path-bytes path-bytes))
               (budget/bound! op :tree-depth (count (str/split path #"/")))
               (let [entry (d/pull snapshot [:geschichte.stage/state :geschichte.stage/mode :geschichte.stage/size
                                             {:geschichte.stage/content [:geschichte.content/id]}] (:e datom))]
                 (case (:geschichte.stage/state entry)
                   :deleted out
                   :present
                   (let [id (get-in entry [:geschichte.stage/content :geschichte.content/id])
                         size (:geschichte.stage/size entry) mode (:geschichte.stage/mode entry)]
                     (when-not (and (uuid? id) (integer? size) (<= 0 size Long/MAX_VALUE)
                                    (#{33188 33261} mode))
                       (refuse! :invalid-tree))
                     (budget/bound! op :object-bytes size)
                     (budget/charge! op :tree-bytes size)
                     (assoc out path {:content id :size size :mode mode}))
                   (refuse! :invalid-tree)))))
           (sorted-map) (d/datoms snapshot :avet :geschichte.stage/path))]
      (tree/assert-valid! entries [] op)
      {:db snapshot :entries entries})))

(defn- add-object! [objects type payload op]
  (budget/bound! op :object-bytes (alength ^bytes payload))
  (let [oid (object/object-id type payload)]
    (when-not (contains? @objects oid)
      (budget/charge! op :objects 1)
      (budget/charge! op :merge-output-bytes (alength ^bytes payload))
      (vswap! objects assoc oid {:type type :payload payload}))
    oid))

(defn- project-tree! [db row objects op]
  (let [{snapshot :db entries :entries} (snapshot-tree db row op)
        wrapper (reify clojure.lang.IDeref (deref [_] snapshot))
        trie (reduce-kv
              (fn [out path {:keys [content size mode]}]
                (budget/check! op)
                (let [bytes (content/read-by-id wrapper content {:merge-budget op :read-db snapshot :sync? true})]
                  (when-not (= size (alength ^bytes bytes)) (refuse! :invalid-content-size))
                  (assoc-in out (str/split path #"/") {:oid (add-object! objects :blob bytes op) :mode mode})))
              {} entries)]
    (letfn [(emit [node depth]
              (budget/bound! op :tree-depth depth)
              (let [children (mapv (fn [[name child]]
                                     (budget/check! op)
                                     (if (:oid child)
                                       {:name name :mode (Integer/toOctalString (int (:mode child))) :oid (:oid child)}
                                       {:name name :mode "40000" :oid (emit child (inc depth))})) node)]
                (budget/bound! op :object-bytes
                               (reduce (fn [size {:keys [name mode]}]
                                         (+ size (count mode) 22 (text/utf8-size op name))) 0 children))
                (add-object! objects :tree (object/tree-payload children) op)))]
      (emit trie 0))))

(defn- commit-payload [row parents tree op]
  (let [author (or (:geschichte.commit/author row) "unknown") message (:geschichte.commit/message row)
        time (:geschichte.commit/time row)]
    (when-not (and (string? author) (<= (count author) 4096) (not (re-find #"[\p{Cntrl}<>]" author))
                   (string? message) (<= (count message) 65536) (inst? time))
      (refuse! :invalid-commit))
    (let [name (if (str/blank? author) "unknown" author)
          local (-> name str/lower-case (str/replace #"[^a-z0-9]+" "."))
          person (str name " <" local "@geschichte.invalid> " (quot (.getTime ^java.util.Date time) 1000) " +0000")]
      ;; Parent order is deliberately canonical: the native graph is a set,
      ;; and incidental Datahike entity ordering must not change Git identity.
      (budget/bound! op :object-bytes
                     (+ 46 (* 48 (count parents)) 7 1 10 1 1
                        (* 2 (text/utf8-size op person)) (text/utf8-size op message)))
      (object/commit-payload {:tree tree :parents (vec (sort parents)) :author person :committer person :message message}))))

(defn prepare
  "Bounded pure incremental pack over ONE immutable source snapshot. upstream
   is the logical commit matching a verified expected remote OID. Source Git
   metadata must come from verified fetches or this projection's acknowledged
   mappings. Outside receiver-known ancestry, a stored Git OID must match its
   full canonical reconstruction exactly; missing snapshots/mismatches refuse."
  [conn head {:keys [upstream-commit expected-remote-oid ref operation]}]
  (require! operation)
  (when-not (and (uuid? head) (oid? expected-remote-oid) (ref? ref)) (refuse! :invalid-options))
  (let [db @conn known (receiver-known db upstream-commit expected-remote-oid operation) objects (volatile! {})
        done
        (loop [pending [[head false nil]] active #{} done {}]
          (budget/check! operation)
          (if-let [[id exit? row] (peek pending)]
            (cond
              (contains? done id) (recur (pop pending) active done)
              (contains? known id)
              (recur (pop pending) active (assoc done id {:oid (known id) :old? (= expected-remote-oid (known id)) :boundary? true}))
              exit?
              (let [parents (mapv #(get-in done [% :oid]) (:parents row))
                    _ (when-not (every? oid? parents) (refuse! :missing-history))
                    payload (commit-payload row parents (project-tree! db row objects operation) operation)
                    oid (add-object! objects :commit payload operation)
                    stored (:geschichte.commit/git-oid row)]
                (when (and stored (not= stored oid)) (refuse! :projection-mismatch))
                (recur (pop pending) (disj active id)
                       (assoc done id {:oid oid :old? (boolean (some #(get-in done [% :old?]) (:parents row)))})))
              (contains? active id) (refuse! :cyclic-history)
              :else
              (let [row (commit! db id operation)]
                (when-not (:geschichte.commit/snapshot row) (refuse! :missing-history))
                (recur (into (conj (pop pending) [id true row]) (map #(vector % false nil) (:parents row)))
                       (conj active id) done)))
            done))
        root (done head)]
    (when-not (or (= receive/zero-oid expected-remote-oid) (:old? root)) (refuse! :not-fast-forward))
    {:version 1 :projection projection :ref ref :old expected-remote-oid :new (:oid root)
     :head head :source-checkpoint (d/commit-id db)
     :commits (into {} (keep (fn [[id value]] (when-not (:boundary? value) [id (:oid value)]))) done)
     :objects (count @objects) :pack (pack/encode-bounded @objects operation)}))

(defn inspect-remote!
  "Require the exact reviewed ref OID before source projection. Missing refs
   have the explicit all-zero OID; nil is never interpreted as absence."
  [send-fn url ref expected op]
  (require! op)
  (when-not (and (ref? ref) (oid? expected)) (refuse! :invalid-options))
  (let [advertised (receive/parse-advertisement (http/advertise! send-fn url "git-receive-pack" {:import-budget op}))
        caps (:capabilities advertised) actual (get-in advertised [:refs ref] receive/zero-oid)]
    (when-not (and (contains? caps "report-status")
                   (not-any? #(and (str/starts-with? % "object-format=") (not= % "object-format=sha1")) caps)
                   (every? oid? (vals (:refs advertised)))) (refuse! :unsupported-remote))
    (when-not (= expected actual) (refuse! :remote-changed))
    {:ref ref :old actual :capabilities (cond-> ["report-status"] (contains? caps "object-format=sha1") (conj "object-format=sha1"))}))

(defn prepare-http! [send-fn conn url head opts]
  (when-not (uuid? head) (refuse! :invalid-options))
  (let [remote (inspect-remote! send-fn url (:ref opts) (:expected-remote-oid opts) (:operation opts))]
    (assoc (prepare conn head opts) :capabilities (:capabilities remote))))

(defn send-http!
  "Dispatch ONE caller-admitted update with the SAME expected OID in wire CAS.
   A thrown/malformed/missing result is unknown; this API never retries."
  [send-fn url prepared op]
  (require! op)
  (when-not (and (= 1 (:version prepared)) (= projection (:projection prepared))
                 (ref? (:ref prepared)) (every? oid? [(:old prepared) (:new prepared)])
                 (bytes? (:pack prepared))
                 (vector? (:capabilities prepared)) (<= 1 (count (:capabilities prepared)) 2)
                 (contains? (set (:capabilities prepared)) "report-status")
                 (every? #{"report-status" "object-format=sha1"} (:capabilities prepared)))
    (refuse! :invalid-preparation))
  (when (> (alength ^bytes (:pack prepared)) (get-in op [:limits :pack-bytes])) (refuse! :pack-limit))
  (budget/check! op)
  (let [request (receive/request {:updates [{:ref (:ref prepared) :old (:old prepared) :new (:new prepared)}]
                                  :capabilities (:capabilities prepared) :pack (:pack prepared)})
        response (http/rpc! send-fn url "git-receive-pack" request {:import-budget op})
        frames (pkt/decode response)
        report (receive/parse-report response)]
    (when-not (and (= 3 (count frames)) (= :flush (last frames))
                   (every? bytes? (take 2 frames))
                   (= "unpack ok\n" (pkt/text (first frames)))
                   (let [line (pkt/text (second frames))]
                     (or (= (str "ok " (:ref prepared) "\n") line)
                         (str/starts-with? line (str "ng " (:ref prepared) " ")))))
      (refuse! :outcome-unconfirmed))
    (when-not (and (= "ok" (:unpack report)) (= #{(:ref prepared)} (set (keys (:refs report))))
                   (empty? (:other report)))
      (refuse! :outcome-unconfirmed))
    (case (get-in report [:refs (:ref prepared) :status])
      :ok {:status :pushed :ref (:ref prepared) :old (:old prepared) :new (:new prepared)}
      :rejected {:status :rejected :ref (:ref prepared) :old (:old prepared) :new (:new prepared)}
      (refuse! :outcome-unconfirmed))))

(defn mapping-tx
  "Pure owned-writer fragment. Record generated OIDs before external dispatch,
   under the exact inspected native checkpoint. Does not move any file/ref or
   prove a remote push. Caller independently checks source authorization."
  [db prepared]
  (when-not (and (= projection (:projection prepared)) (= (:source-checkpoint prepared) (d/commit-id db))
                 (map? (:commits prepared)) (<= (count (:commits prepared)) 50000))
    (refuse! :source-changed))
  (let [tx (mapv (fn [[id oid]]
                   (let [row (d/entity db [:geschichte.commit/id id]) stored (:geschichte.commit/git-oid row)]
                     (when-not (and (uuid? id) (:db/id row) (oid? oid) (or (nil? stored) (= stored oid))) (refuse! :mapping-conflict))
                     {:db/id (:db/id row) :geschichte.commit/git-oid oid}))
                 (:commits prepared))]
    tx))
