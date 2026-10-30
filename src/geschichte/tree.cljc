(ns geschichte.tree
  "Pure path and file-tree invariants shared by repository and merge boundaries."
  (:require [clojure.string :as str]
            [geschichte.budget :as budget]))

(defn parent-paths
  ([path] (parent-paths path nil))
  ([path operation]
   (budget/charge! operation :text-work (count path))
   (let [parts (str/split path #"/")]
     (mapv (fn [n]
             (let [size (+ (dec n) (reduce + (map count (take n parts))))]
               (budget/charge! operation :text-work size)
               (str/join "/" (take n parts))))
           (range 1 (count parts))))))

(defn assert-path! [path]
  (when-not (and (string? path) (not (str/blank? path))
                 (not (str/includes? path "\\"))
                 (not (str/includes? path "\u0000"))
                 (every? #(and (not (str/blank? %)) (not (#{"." ".."} %)))
                         (str/split path #"/" -1)))
    (throw (ex-info "Invalid canonical repository path" {:type ::invalid-path :path path})))
  path)

(defn prefix-conflicts
  "Return all [ancestor descendant] file pairs in deterministic order."
  ([paths] (prefix-conflicts paths nil))
  ([paths operation]
   (budget/charge! operation :text-work (count paths))
   (let [paths (set paths)
         ordered (sort (fn [a b]
                         (budget/charge! operation :text-work (max (count a) (count b)))
                         (compare a b)) paths)]
     (vec (for [path ordered parent (parent-paths path operation) :when (contains? paths parent)]
            [parent path])))))

(defn assert-valid!
  "Validate a file-path map and optional explicit directory paths, without IO."
  ([tree] (assert-valid! tree []))
  ([tree directories] (assert-valid! tree directories nil))
  ([tree directories operation]
   (let [paths (set (keys tree))]
     (doseq [path paths] (budget/charge! operation :text-work (count path)) (assert-path! path))
     (when-let [[ancestor descendant] (first (prefix-conflicts paths operation))]
       (throw (ex-info "A repository file cannot contain another file"
                       {:type ::path-conflict :path ancestor :descendant descendant})))
     (doseq [dir directories]
       (assert-path! dir)
       (when-let [file (some paths (conj (parent-paths dir operation) dir))]
         (throw (ex-info "A repository file conflicts with an explicit directory"
                         {:type ::path-conflict :path file :directory dir})))))
   tree))
