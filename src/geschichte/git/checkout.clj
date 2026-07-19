(ns geschichte.git.checkout
  "Lazy materialization of imported Git commits into Geschichte snapshots."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [geschichte.git.object :as object]
            [geschichte.git.store :as store]
            [geschichte.repo :as repo])
  (:import [java.util Date UUID]))

(defn- stored-object [object-store oid expected-type]
  (let [metadata (or (store/read-object object-store oid)
                     (throw (ex-info "Missing imported Git object" {:oid oid})))
        type (:geschichte.git.object/type metadata)]
    (when-not (= expected-type type)
      (throw (ex-info "Unexpected imported Git object type"
                      {:oid oid :expected expected-type :actual type})))
    (:payload metadata)))

(defn- mode-long [mode]
  (Long/parseLong mode 8))

(defn- tree-files
  ([object-store tree-oid object-format]
   (tree-files object-store "" tree-oid object-format))
  ([object-store prefix tree-oid object-format]
   (mapcat
    (fn [{:keys [mode name oid]}]
      (let [administrative-name (str/lower-case name)
            _ (when (contains? #{"." ".." ".git" ".geschichte"}
                               administrative-name)
                (throw (ex-info "Git tree contains a reserved administrative path"
                                {:prefix prefix :name name :oid oid})))
            path (if (str/blank? prefix) name (str prefix "/" name))]
        (case mode
          "40000" (tree-files object-store path oid object-format)
          "040000" (tree-files object-store path oid object-format)
          "160000" (throw (ex-info "Gitlinks require submodule policy"
                                   {:path path :oid oid}))
          [[path {:bytes (stored-object object-store oid :blob)
                  :mode (mode-long mode)}]])))
    (object/parse-tree object-format
                       (stored-object object-store tree-oid :tree)))))

(defn- logical-ids [conn oids]
  (let [existing
        (into {}
              (d/q '[:find ?oid ?id
                     :where
                     [?e :geschichte.commit/git-oid ?oid]
                     [?e :geschichte.commit/id ?id]] @conn))]
    (into {}
          (map (fn [oid]
                 [oid (or (get existing oid)
                          (UUID/nameUUIDFromBytes
                           (.getBytes (str "geschichte:git:sha1:" oid)
                                      "UTF-8")))]))
          oids)))

(defn- identity-and-time [person]
  (if-let [[_ identity seconds]
           (and person (re-matches #"(?s)(.*) ([0-9]+) [+-][0-9]{4}" person))]
    {:identity identity :time (Date. (* 1000 (Long/parseLong seconds)))}
    {:identity (or person "unknown") :time (Date. 0)}))

(defn- commit-graph [object-store root-oid limit]
  (loop [pending [root-oid] seen #{} commits {} frontier #{}]
    (if-let [oid (peek pending)]
      (cond
        (seen oid)
        (recur (pop pending) seen commits frontier)

        (>= (count commits) limit)
        (recur (pop pending) (conj seen oid) commits (conj frontier oid))

        :else
        (let [parsed (object/parse-commit
                      (stored-object object-store oid :commit))]
          (recur (into (pop pending) (:parents parsed))
                 (conj seen oid)
                 (assoc commits oid parsed)
                 frontier)))
      {:commits commits :frontier frontier})))

(defn materialize!
  "Materialize one imported Git commit and switch a Geschichte logical ref to it.
  Ancestors become lightweight commit metadata and remain unmaterialized until
  selected."
  ([conn oid] (materialize! conn oid nil))
  ([conn oid {:keys [ref force? object-format object-store
                     history-materialization-limit]
              :or {ref "refs/heads/imported" object-format :sha1
                   history-materialization-limit 2048}}]
   (store/with-store
     (or object-store conn)
     (fn [object-store]
       (let [{:keys [commits frontier]}
             (commit-graph object-store oid history-materialization-limit)
             id-by-oid (logical-ids conn (into (set (keys commits)) frontier))
             selected (get commits oid)
             file-count (volatile! 0)
             files (map (fn [entry] (vswap! file-count inc) entry)
                        (tree-files object-store (:tree selected) object-format))
             snapshot (repo/materialize-bytes! conn files {:force? force?})
             commit-tx
             (into
              (mapv (fn [commit-oid]
                      {:geschichte.commit/id (get id-by-oid commit-oid)
                       :geschichte.commit/git-oid commit-oid})
                    frontier)
              (map
               (fn [[commit-oid {:keys [author message parents]}]]
                 (let [{:keys [identity time]} (identity-and-time author)]
                   (cond-> {:geschichte.commit/id (get id-by-oid commit-oid)
                            :geschichte.commit/git-oid commit-oid
                       ;; Git conventionally stores one framing newline at the
                       ;; end of a commit message. The exact payload remains in
                       ;; the Git object store; logical history keeps the message
                       ;; value used by Geschichte's own commits and formatters.
                            :geschichte.commit/message (str/replace message #"\n$" "")
                            :geschichte.commit/author identity
                            :geschichte.commit/time time}
                     (seq parents)
                     (assoc :geschichte.commit/parents
                            (mapv (fn [parent]
                                    {:geschichte.commit/id (get id-by-oid parent)})
                                  parents))
                     (= commit-oid oid)
                     (assoc :geschichte.commit/snapshot snapshot))))
               commits))
             id (get id-by-oid oid)
             repo-id (d/q '[:find ?id . :where [_ :geschichte.repo/id ?id]] @conn)]
     ;; Nested unique-identity maps upsert parent entities inside this same
     ;; transaction. Unlike lookup refs, they do not require an intermediate DB
     ;; state where identities exist but the graph/ref publication is incomplete.
         (d/transact conn
                     (into commit-tx
                           [{:geschichte.ref/name ref
                             :geschichte.ref/target {:geschichte.commit/id id}}
                            {:geschichte.repo/id repo-id
                             :geschichte.repo/head ref}]))
         {:commit id :git-oid oid :ref ref :files @file-count
          :imported-commits (count commits)
          :history-truncated? (boolean (seq frontier))
          :history-frontier-count (count frontier)})))))
