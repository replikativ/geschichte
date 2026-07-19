(ns geschichte.git.project
  "Pure projection of Geschichte snapshots into canonical Git object graphs."
  (:require [clojure.string :as str]
            [geschichte.git.object :as object]
            [geschichte.git.store :as store]
            [geschichte.repo :as repo]))

(defn- git-mode [mode]
  (Integer/toOctalString (int mode)))

(defn- tree-trie [tree]
  (reduce-kv (fn [trie path metadata]
               (assoc-in trie (str/split path #"/") metadata))
             {} tree))

(defn- add-object! [objects type payload]
  (let [oid (object/object-id type payload)]
    (swap! objects assoc oid {:type type :payload payload})
    oid))

(defn- project-trie! [objects node]
  (let [entries
        (mapv (fn [[name child]]
                (if (:content child)
                  {:mode (git-mode (:mode child))
                   :name name
                   :oid (add-object! objects :blob (:bytes child))}
                  {:mode "40000" :name name
                   :oid (project-trie! objects child)}))
              node)]
    (add-object! objects :tree (object/tree-payload entries))))

(defn- default-identity [author]
  (let [name (if (str/blank? author) "unknown" author)
        local (-> name str/lower-case (str/replace #"[^a-z0-9]+" "."))]
    (str name " <" local "@geschichte.invalid>")))

(defn- git-person [identity date]
  (str identity " " (quot (.getTime ^java.util.Date date) 1000) " +0000"))

(defn- stored-object [object-store oid]
  (store/object object-store oid))

(defn- include-exact-object!
  "Copy an imported Git object and everything it references into `objects`.
  This keeps an exact imported ancestry usable when pushing to a remote that
  does not already have it."
  [object-store objects oid]
  (when-not (contains? @objects oid)
    (let [object-format (case (count oid)
                          40 :sha1
                          64 :sha256
                          (throw (ex-info "Unsupported exact Git object ID"
                                          {:oid oid})))
          metadata (or (store/read-object object-store oid)
                       (throw (ex-info "Missing exact Git object"
                                       {:oid oid})))
          type (:geschichte.git.object/type metadata)
          payload (:payload metadata)]
      (when-not (= oid (object/object-id object-format type payload))
        (throw (ex-info "Stored Git object does not match its OID"
                        {:oid oid :type type})))
      ;; Mark before descent so malformed cyclic input cannot recurse forever.
      (swap! objects assoc oid {:type type :payload payload})
      (case type
        :commit
        (let [{:keys [tree parents]} (object/parse-commit payload)]
          (include-exact-object! object-store objects tree)
          (doseq [parent parents]
            (include-exact-object! object-store objects parent)))

        :tree
        (doseq [{:keys [oid]} (object/parse-tree object-format payload)]
          (include-exact-object! object-store objects oid))

        :tag
        (when-let [[_ target] (re-find #"(?m)^object ([0-9a-f]+)$"
                                       (String. ^bytes payload "UTF-8"))]
          (include-exact-object! object-store objects target))

        :blob nil
        (throw (ex-info "Unsupported exact Git object type"
                        {:oid oid :type type}))))))

(defn project
  "Project a Geschichte commit and its ancestors to a complete Git object map.

  Returns {:oid root-commit-oid, :objects {oid {:type :payload}},
  :commits {geschichte-uuid git-oid}}. `:identity-fn` maps Geschichte's author
  string to a Git `Name <email>` identity."
  ([conn commit-id] (project conn commit-id nil))
  ([conn commit-id {:keys [identity-fn object-store]
                    :or {identity-fn default-identity}}]
   (store/with-store
     (or object-store conn)
     (fn [object-store]
       (let [objects (atom {})
             commits (atom {})]
         (letfn [(project-commit! [id]
                   (if-let [oid (get @commits id)]
                     oid
                     (let [commit (or (repo/commit-by-id conn id)
                                      (throw (ex-info "Unknown Geschichte commit"
                                                      {:commit id})))
                           exact-oid (:geschichte.commit/git-oid commit)]
                       (if (and exact-oid (stored-object object-store exact-oid))
                         (do
                       ;; Retain logical mappings for the imported ancestry as
                       ;; well as its exact serialized Git graph.
                           (doseq [parent (:geschichte.commit/parents commit)]
                             (project-commit! (:geschichte.commit/id parent)))
                           (include-exact-object! object-store objects exact-oid)
                           (swap! commits assoc id exact-oid)
                           exact-oid)
                         (let [parents (mapv #(project-commit!
                                               (:geschichte.commit/id %))
                                             (:geschichte.commit/parents commit))
                               tree (repo/tree-at conn id)
                               trie (tree-trie
                                     (reduce-kv
                                      (fn [result path metadata]
                                        (assoc result path
                                               (assoc metadata :bytes
                                                      (repo/read-at conn id path))))
                                      {} tree))
                               tree-oid (project-trie! objects trie)
                               person (git-person
                                       (identity-fn (:geschichte.commit/author commit))
                                       (:geschichte.commit/time commit))
                               payload (object/commit-payload
                                        {:tree tree-oid :parents parents
                                         :author person :committer person
                                         :message (:geschichte.commit/message commit)})
                               oid (add-object! objects :commit payload)]
                           (swap! commits assoc id oid)
                           oid)))))]
           (let [oid (project-commit! commit-id)]
             {:oid oid :objects @objects :commits @commits})))))))
