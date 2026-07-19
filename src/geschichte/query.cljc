(ns geschichte.query
  "Portable, read-only Datalog views over a Geschichte database value.

  These functions never perform I/O and are synchronous on JVM, Node, and the
  browser. Callers obtain a current or historical Datahike db value through the
  platform's connection/versioning API, then interpret it here."
  (:require [datahike.api :as d]))

(defn repository [db]
  (d/q '[:find (pull ?repo [*]) .
         :where [?repo :geschichte.repo/id]] db))

(defn initialized? [db]
  (boolean (repository db)))

(defn configuration
  "Return repository-local configuration as a sorted string map."
  [db]
  (into (sorted-map)
        (d/q '[:find ?key ?value
               :where
               [?entry :geschichte.config/key ?key]
               [?entry :geschichte.config/value ?value]]
             db)))

(defn current-ref [db]
  (:geschichte.repo/head (repository db)))

(defn refs
  "Return `{logical-ref commit-id-or-nil}`."
  [db]
  (into (sorted-map)
        (map (fn [[ref]]
               [(:geschichte.ref/name ref)
                (get-in ref [:geschichte.ref/target :geschichte.commit/id])]))
        (d/q '[:find (pull ?ref [:geschichte.ref/name
                                 {:geschichte.ref/target
                                  [:geschichte.commit/id]}])
               :where [?ref :geschichte.ref/name]] db)))

(defn commit [db id]
  (d/q '[:find (pull ?commit [:geschichte.commit/id
                              :geschichte.commit/snapshot
                              :geschichte.commit/message
                              :geschichte.commit/author
                              :geschichte.commit/time
                              :geschichte.commit/git-oid
                              {:geschichte.commit/parents
                               [:geschichte.commit/id]}]) .
         :in $ ?id
         :where [?commit :geschichte.commit/id ?id]] db id))

(defn commits
  "All logical commit metadata currently catalogued in this workspace."
  [db]
  (d/q '[:find [(pull ?commit [:geschichte.commit/id
                               :geschichte.commit/snapshot
                               :geschichte.commit/message
                               :geschichte.commit/author
                               :geschichte.commit/time
                               :geschichte.commit/git-oid
                               {:geschichte.commit/parents
                                [:geschichte.commit/id]}]) ...]
         :where [?commit :geschichte.commit/id]] db))

(defn worktree
  "Current worktree metadata without loading content payloads."
  [db]
  (into (sorted-map)
        (map (fn [[path content size mode]]
               [path {:content content :size size :mode mode}]))
        (d/q '[:find ?path ?content-id ?size ?mode
               :where
               [?entry :geschichte.work/path ?path]
               [?entry :geschichte.work/content ?content]
               [?content :geschichte.content/id ?content-id]
               [?entry :geschichte.work/size ?size]
               [?entry :geschichte.work/mode ?mode]] db)))

(defn stage
  "Current staging-index metadata, retaining explicit deletion states."
  [db]
  (into (sorted-map)
        (map (fn [[entry]]
               [(:geschichte.stage/path entry)
                {:state (:geschichte.stage/state entry)
                 :content (get-in entry [:geschichte.stage/content
                                         :geschichte.content/id])
                 :size (:geschichte.stage/size entry)
                 :mode (:geschichte.stage/mode entry)}]))
        (d/q '[:find (pull ?entry [:geschichte.stage/path
                                   :geschichte.stage/state
                                   :geschichte.stage/size
                                   :geschichte.stage/mode
                                   {:geschichte.stage/content
                                    [:geschichte.content/id]}])
               :where [?entry :geschichte.stage/path]] db)))

(defn content [db id]
  (d/q '[:find (pull ?content [:geschichte.content/id
                               :geschichte.content/kind
                               :geschichte.content/payload
                               :geschichte.content/depth
                               :geschichte.content/size
                               {:geschichte.content/base
                                [:geschichte.content/id]}]) .
         :in $ ?id
         :where [?content :geschichte.content/id ?id]] db id))

(defn git-refs [db]
  (into (sorted-map)
        (map (fn [entity]
               [(:geschichte.git.ref/name entity)
                (cond-> {:oid (:geschichte.git.ref/oid entity)
                         :source (:geschichte.git.ref/source entity)}
                  (:geschichte.git.ref/peeled entity)
                  (assoc :peeled (:geschichte.git.ref/peeled entity))
                  (:geschichte.git.ref/symref-target entity)
                  (assoc :symref-target
                         (:geschichte.git.ref/symref-target entity)))]))
        (d/q '[:find [(pull ?entity
                            [:geschichte.git.ref/name
                             :geschichte.git.ref/oid
                             :geschichte.git.ref/source
                             :geschichte.git.ref/peeled
                             :geschichte.git.ref/symref-target]) ...]
               :where [?entity :geschichte.git.ref/name]] db)))

(defn git-packs
  "Exact Git pack manifests. Index and chunk payloads remain store-refs; a
  query clause may resolve them with `konserve.core/bget` when payload-aware
  interpretation is useful, without placing their bytes in Datahike indices."
  [db]
  (d/q '[:find [(pull ?pack
                      [:geschichte.git.pack/id
                       :geschichte.git.pack/size
                       :geschichte.git.pack/checksum
                       :geschichte.git.pack/object-format
                       {:geschichte.git.pack/index-shards
                        [:geschichte.git.index/prefix
                         :geschichte.git.index/count
                         :geschichte.git.index/payload]}
                       {:geschichte.git.pack/chunks
                        [:geschichte.chunk/index
                         :geschichte.chunk/offset
                         :geschichte.chunk/size
                         :geschichte.chunk/payload]}]) ...]
         :where [?pack :geschichte.git.pack/id]] db))
