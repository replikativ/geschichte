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

(defn- resolve-path [path base ours theirs]
  (cond
    (= ours theirs) {:path path :value ours}
    (= ours base) {:path path :value theirs}
    (= theirs base) {:path path :value ours}
    :else {:path path :conflict {:base base :ours ours :theirs theirs}}))

(defn plan-trees
  "Plan a merge from already-resolved commit IDs and immutable path maps."
  [base-id ours-id theirs-id base-tree ours-tree theirs-tree]
  (let [paths (sort (set/union (set (keys base-tree))
                               (set (keys ours-tree))
                               (set (keys theirs-tree))))
        resolutions
        (mapv (fn [path]
                (resolve-path path
                              (get base-tree path absent)
                              (get ours-tree path absent)
                              (get theirs-tree path absent)))
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
     :tree tree :conflicts conflicts :clean? (empty? conflicts)}))
