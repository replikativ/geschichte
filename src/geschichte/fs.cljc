(ns geschichte.fs
  "Filesystem-shaped access to a Geschichte working tree."
  (:refer-clojure :exclude [await read])
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [geschichte.async :as execution]
            [geschichte.repo :as repo]
            [is.simm.partial-cps.async :refer [await]])
  #?(:cljs (:require-macros [geschichte.macros :refer [platform-async]]))
  #?(:clj (:require [geschichte.macros :refer [platform-async]])))

(defn normalize [path]
  (let [parts (remove #(or (str/blank? %) (= "." %))
                      (str/split (str/replace (str (or path "")) #"\\" "/")
                                 #"/+"))]
    (when (some #{".."} parts)
      (throw (ex-info "Path escapes Geschichte root" {:path path})))
    (str/join "/" parts)))

(defn- parent [path]
  (let [i (.lastIndexOf ^String path "/")]
    (if (neg? i) "" (subs path 0 i))))

(defn- explicit-dirs [db]
  (into #{}
        (map first)
        (d/q '[:find ?path
               :where [_ :geschichte.work-dir/path ?path]] db)))

(defn- path-prefixes [path]
  (let [parts (str/split path #"/")]
    (map #(str/join "/" (take % parts)) (range 1 (count parts)))))

(defn directories [conn]
  (into (sorted-set "")
        (concat (explicit-dirs @conn)
                (mapcat path-prefixes (repo/files conn)))))

(defn stat [conn path]
  (let [path (normalize path)]
    (if-let [{:keys [size mode content]} (get (repo/worktree conn) path)]
      {:path path :type :file :size size :mode mode :content content}
      (when (contains? (directories conn) path)
        {:path path :type :dir :size 0}))))

(defn list-dir [conn path]
  (let [path (normalize path)]
    (when-not (= :dir (:type (stat conn path)))
      (throw (ex-info "Not a Geschichte directory" {:path path})))
    (let [prefix (if (str/blank? path) "" (str path "/"))
          tree (repo/worktree conn)
          dirs (directories conn)
          paths (concat (keys tree) (disj dirs ""))]
      (->> paths
           (keep (fn [candidate]
                   (when (str/starts-with? candidate prefix)
                     (let [relative (subs candidate (count prefix))
                           name (first (str/split relative #"/"))]
                       (when-not (str/blank? name)
                         [name (str prefix name)])))))
           (into {})
           (map (fn [[name child]]
                  (assoc (stat conn child) :name name)))
           (sort-by (juxt #(if (= :dir (:type %)) 0 1) :name))
           vec))))

(defn read [conn path]
  (let [path (normalize path)
        entry (stat conn path)]
    (when (= :dir (:type entry))
      (throw (ex-info "Cannot read a Geschichte directory" {:path path})))
    (repo/read conn path)))

(defn- transact-data [conn tx-data]
  (execution/io-result #?(:clj (d/transact conn tx-data)
                          :cljs (d/transact! conn tx-data))
                       execution/default-opts))

(defn mkdir! [conn path]
  (platform-async
   (let [path (normalize path)]
     (when (str/blank? path)
       (throw (ex-info "Geschichte root already exists" {})))
     (when (stat conn path)
       (throw (ex-info "Geschichte path already exists" {:path path})))
     (when-not (= :dir (:type (stat conn (parent path))))
       (throw (ex-info "Parent directory does not exist" {:path path})))
     (await (transact-data conn [{:geschichte.work-dir/path path}]))
     path)))

(defn- ensure-parents! [conn path]
  (platform-async
   (doseq [dir (path-prefixes path)]
     (when-not (stat conn dir)
       (await (transact-data conn [{:geschichte.work-dir/path dir}]))))))

(defn write!
  ([conn path value] (write! conn path value nil))
  ([conn path value opts]
   (platform-async
    (let [path (normalize path)]
      (when (str/blank? path)
        (throw (ex-info "Cannot write the Geschichte root" {})))
      (when (= :dir (:type (stat conn path)))
        (throw (ex-info "Cannot replace directory with file" {:path path})))
      (await (ensure-parents! conn path))
      (await (repo/write! conn path value (or opts {})))))))

(defn delete! [conn path]
  (platform-async
   (let [path (normalize path)
         entry (stat conn path)]
     (case (:type entry)
       :file (boolean (await (repo/remove! conn path)))
       :dir (do
              (when (str/blank? path)
                (throw (ex-info "Cannot delete Geschichte root" {})))
              (when (seq (list-dir conn path))
                (throw (ex-info "Directory is not empty" {:path path})))
              (when-let [eid (d/q '[:find ?e . :in $ ?path
                                    :where [?e :geschichte.work-dir/path ?path]]
                                  @conn path)]
                (await (transact-data conn [[:db/retractEntity eid]])))
              true)
       false))))

(defn- move-path [from to path]
  (str to (subs path (count from))))

(defn rename! [conn from to]
  (platform-async
   (let [from (normalize from)
         to (normalize to)
         entry (stat conn from)]
     (when (or (str/blank? from) (str/blank? to))
       (throw (ex-info "Cannot rename Geschichte root" {:from from :to to})))
     (when-not entry
       (throw (ex-info "Source path does not exist" {:from from})))
     (when (stat conn to)
       (throw (ex-info "Destination path already exists" {:to to})))
     (when-not (= :dir (:type (stat conn (parent to))))
       (throw (ex-info "Destination parent does not exist" {:to to})))
     (when (and (= :dir (:type entry))
                (str/starts-with? to (str from "/")))
       (throw (ex-info "Cannot move a directory into itself"
                       {:from from :to to})))
     (let [file-paths (if (= :file (:type entry))
                        [from]
                        (filter #(str/starts-with? % (str from "/"))
                                (repo/files conn)))
           dir-paths (if (= :dir (:type entry))
                       (filter #(or (= % from)
                                    (str/starts-with? % (str from "/")))
                               (explicit-dirs @conn))
                       [])
           file-tx (for [path file-paths
                         :let [eid (d/q '[:find ?e . :in $ ?path
                                          :where [?e :geschichte.work/path ?path]]
                                        @conn path)]]
                     [:db/add eid :geschichte.work/path
                      (move-path from to path)])
           dir-tx (for [path dir-paths
                        :let [eid (d/q '[:find ?e . :in $ ?path
                                         :where [?e :geschichte.work-dir/path ?path]]
                                       @conn path)]]
                    [:db/add eid :geschichte.work-dir/path
                     (move-path from to path)])]
       (await (transact-data conn (vec (concat file-tx dir-tx))))
       to))))
