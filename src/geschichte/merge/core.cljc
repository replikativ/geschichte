(ns geschichte.merge.core
  "Pure, platform-neutral commit-graph and three-tree merge semantics."
  (:require [clojure.set :as set]))

(def ^:private absent ::absent)

(defn ancestor-distances
  "Return `{commit-id shortest-parent-distance}`, including root at distance 0.
  `parents-of` receives an ID and returns its direct parent IDs."
  [parents-of root]
  (loop [queue (conj #?(:clj clojure.lang.PersistentQueue/EMPTY
                        :cljs cljs.core/PersistentQueue.EMPTY)
                     [root 0])
         distances {}]
    (if (empty? queue)
      distances
      (let [[id distance] (peek queue)
            queue (pop queue)]
        (if (<= (get distances id #?(:clj Long/MAX_VALUE
                                     :cljs js/Number.MAX_SAFE_INTEGER))
                distance)
          (recur queue distances)
          (recur (into queue (map (fn [parent] [parent (inc distance)]))
                       (parents-of id))
                 (assoc distances id distance)))))))

(defn merge-base-from-distances
  "Choose a deterministic nearest common ancestor from two `{id distance}` maps.
  Minimizes the combined distance `ours + theirs`; ties are broken by `(str id)`,
  so the choice is stable across peers whenever ids are stable (as commit ids
  are). Returns nil when the two share no common ancestor. This is the tie-break
  *policy*; the distance maps may come from a pure `ancestor-distances` walk or
  from the database (e.g. datahike's `bfs-distances`)."
  [ours-distance theirs-distance]
  (let [common (set/intersection (set (keys ours-distance))
                                 (set (keys theirs-distance)))]
    (first
     (sort-by (fn [id]
                [(+ (ours-distance id) (theirs-distance id)) (str id)])
              common))))

(defn merge-base
  "Choose a deterministic nearest common ancestor from a parent function."
  [parents-of ours theirs]
  (merge-base-from-distances (ancestor-distances parents-of ours)
                             (ancestor-distances parents-of theirs)))

(defn- resolve-path
  "Resolve one path from its three tree entries.

  The first three cases are decidable from the entries alone. The fourth — both
  sides changed it — is where git does one more thing: it merges the two blobs
  line by line and only conflicts if THAT fails. `resolve-content` is that step,
  supplied by a caller that can read content (see `geschichte.repo/content-merger`);
  it takes the three entries and returns a replacement entry, or nil when it
  cannot merge them.

  Without it the fourth case is a conflict, which is what this did before and
  what it still does when no resolver is given. That is correct and coarse: two
  people editing different parts of one file collide on every merge."
  [path base ours theirs resolve-content]
  (cond
    (= ours theirs) {:path path :value ours}
    (= ours base) {:path path :value theirs}
    (= theirs base) {:path path :value ours}
    :else
    (if-let [merged (when (and resolve-content
                               ;; a side that does not EXIST has no content to
                               ;; merge — add/add and add/delete are structural
                               ;; disagreements, not textual ones
                               (not-any? #(= absent %) [base ours theirs]))
                      (resolve-content path base ours theirs))]
      {:path path :value merged :merged? true}
      {:path path :conflict {:base base :ours ours :theirs theirs}})))

(defn plan-trees
  "Plan a merge from already-resolved commit IDs and immutable path maps.

  `opts` may carry `:resolve-content`, a fn `(path base ours theirs)` over tree
  ENTRIES returning a merged entry or nil — the content-level merge git performs
  before declaring a path conflicted. Omit it and every path both sides touched
  conflicts, which is the historical behaviour.

  This namespace stays pure: the resolver is passed in because merging content
  means READING it, and reading needs a connection. `geschichte.repo/content-merger`
  builds one."
  ([base-id ours-id theirs-id base-tree ours-tree theirs-tree]
   (plan-trees base-id ours-id theirs-id base-tree ours-tree theirs-tree nil))
  ([base-id ours-id theirs-id base-tree ours-tree theirs-tree
    {:keys [resolve-content]}]
  (let [paths (sort (set/union (set (keys base-tree))
                               (set (keys ours-tree))
                               (set (keys theirs-tree))))
        resolutions
        (mapv (fn [path]
                (resolve-path path
                              (get base-tree path absent)
                              (get ours-tree path absent)
                              (get theirs-tree path absent)
                              resolve-content))
              paths)
        conflicts (into (sorted-map)
                        (keep (fn [{:keys [path conflict]}]
                                (when conflict [path conflict])))
                        resolutions)
        tree (into (sorted-map)
                   (keep (fn [{:keys [path value conflict]}]
                           (when (and (nil? conflict) (not= value absent))
                             [path value])))
                   resolutions)]
    {:kind (cond (= ours-id theirs-id) :up-to-date
                 (= base-id ours-id) :fast-forward
                 :else :merge)
     :base base-id :ours ours-id :theirs theirs-id
     :tree tree :conflicts conflicts :clean? (empty? conflicts)
     ;; the paths a content merge settled — a caller writing a merge commit
     ;; message, or a reviewer, wants to know which files were reconciled
     ;; rather than taken wholesale from one side
     :merged (into (sorted-set) (keep (fn [{:keys [path merged?]}]
                                        (when merged? path))
                                      resolutions))})))
