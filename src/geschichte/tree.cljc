(ns geschichte.tree
  "Pure path and file-tree invariants shared by repository and merge boundaries."
  (:require [clojure.string :as str]))

(defn parent-paths [path]
  (let [parts (str/split path #"/")]
    (mapv #(str/join "/" (take % parts)) (range 1 (count parts)))))

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
  [paths]
  (let [paths (set paths)]
    (vec (for [path (sort paths) parent (parent-paths path) :when (contains? paths parent)]
           [parent path]))))

(defn assert-valid!
  "Validate a file-path map and optional explicit directory paths, without IO."
  ([tree] (assert-valid! tree []))
  ([tree directories]
   (let [paths (set (keys tree))]
     (doseq [path paths] (assert-path! path))
     (when-let [[ancestor descendant] (first (prefix-conflicts paths))]
       (throw (ex-info "A repository file cannot contain another file"
                       {:type ::path-conflict :path ancestor :descendant descendant})))
     (doseq [dir directories]
       (assert-path! dir)
       (when-let [file (some paths (conj (parent-paths dir) dir))]
         (throw (ex-info "A repository file conflicts with an explicit directory"
                         {:type ::path-conflict :path file :directory dir})))))
   tree))
