(ns geschichte.git.revision
  "Resolve Git-shaped revision expressions onto Geschichte logical commits."
  (:refer-clojure :exclude [require resolve])
  (:require [clojure.string :as str]
            [geschichte.merge :as merge]
            [geschichte.query :as query]
            [geschichte.repo :as repo]))

(defn- ref-commit [conn name]
  (let [refs (repo/refs conn)
        logical-name (cond
                       (= name "HEAD") (repo/current-ref conn)
                       (str/starts-with? name "refs/") name
                       (contains? refs (str "refs/heads/" name))
                       (str "refs/heads/" name)
                       (contains? refs (str "refs/tags/" name))
                       (str "refs/tags/" name)
                       (contains? refs name) name)
        logical-id (get refs logical-name)]
    (if logical-id
      (repo/commit-by-id conn logical-id)
      (let [git-refs (query/git-refs @conn)
            ref-name (cond
                       (contains? git-refs (str "refs/remotes/" name))
                       (str "refs/remotes/" name)
                       (contains? git-refs (str "refs/tags/" name))
                       (str "refs/tags/" name)
                       (contains? git-refs name) name)
            git-oid (get-in git-refs [ref-name :oid])]
        (when git-oid
          (some #(when (= git-oid (:geschichte.commit/git-oid %)) %)
                (query/commits @conn)))))))

(defn- prefix-commit [conn prefix]
  (let [matches (filterv #(str/starts-with?
                           (str (:geschichte.commit/id %)) prefix)
                         (query/commits @conn))]
    (when (> (count matches) 1)
      (throw (ex-info (str "short object ID " prefix " is ambiguous")
                      {:revision prefix
                       :matches (mapv :geschichte.commit/id matches)})))
    (first matches)))

(defn- parent [conn commit index expression]
  (let [parents (:geschichte.commit/parents commit)
        parent-id (get-in parents [(dec index) :geschichte.commit/id])]
    (or (when parent-id (repo/commit-by-id conn parent-id))
        (throw (ex-info (str "revision " expression " has no parent " index)
                        {:revision expression :parent index
                         :commit (:geschichte.commit/id commit)})))))

(defn- apply-suffix [conn commit suffix expression]
  (loop [commit commit, suffix suffix]
    (if (str/blank? suffix)
      commit
      (let [[_ operator digits remaining]
            (re-matches #"^([~^])([0-9]*)(.*)$" suffix)]
        (when-not operator
          (throw (ex-info (str "invalid revision expression: " expression)
                          {:revision expression :suffix suffix})))
        (let [n (if (str/blank? digits) 1 (parse-long digits))]
          (case operator
            "^" (recur (if (zero? n) commit (parent conn commit n expression))
                       remaining)
            "~" (recur (loop [ancestor commit, remaining n]
                         (if (zero? remaining)
                           ancestor
                           (recur (parent conn ancestor 1 expression)
                                  (dec remaining))))
                       remaining)))))))

(defn resolve
  "Resolve a Git-shaped expression, returning a logical commit or nil.

  Supports HEAD, full logical refs, local branch/tag shorthand, unique commit
  UUID prefixes, and repeated first/nth-parent suffixes (`~N` and `^N`)."
  [conn expression]
  (when-not (str/blank? expression)
    (let [[_ base suffix] (re-matches #"^([^~^]+)(.*)$" expression)
          commit (or (ref-commit conn base)
                     (prefix-commit conn base))]
      (when commit
        (apply-suffix conn commit suffix expression)))))

(defn require
  "Resolve `expression` or throw a Git-shaped ambiguous-argument error."
  [conn expression]
  (or (resolve conn expression)
      (throw (ex-info
              (str "ambiguous argument '" expression
                   "': unknown revision or path not in the working tree")
              {:revision expression}))))

(defn reachable-ids
  "Return the logical commit IDs reachable from `commit`, including itself."
  [conn commit]
  (loop [pending [commit], seen #{}]
    (if-let [commit (peek pending)]
      (let [pending (pop pending)
            id (:geschichte.commit/id commit)]
        (if (contains? seen id)
          (recur pending seen)
          (recur (into pending
                       (keep (fn [parent]
                               (repo/commit-by-id
                                conn (:geschichte.commit/id parent))))
                       (:geschichte.commit/parents commit))
                 (conj seen id))))
      seen)))

(defn selection
  "Translate a Git revision or range into graph traversal starts/exclusions.

  `A..B` selects commits reachable from B but not A. `A...B` selects commits
  reachable from either tip but not their merge base. Missing endpoints mean
  HEAD, as in Git."
  [conn expression]
  (let [[_ left triple-right double-right]
        (when expression (re-matches #"^(.*?)\.\.(?:\.(.*)|(.*))$" expression))]
    (if-not left
      {:starts [(require conn (or expression "HEAD"))] :exclude #{}}
      (let [left (require conn (if (str/blank? left) "HEAD" left))
            right-expression (or triple-right double-right)
            right (require conn (if (str/blank? right-expression)
                                  "HEAD" right-expression))]
        (if (some? triple-right)
          (let [base-id (merge/merge-base conn
                                          (:geschichte.commit/id left)
                                          (:geschichte.commit/id right))
                base (when base-id (repo/commit-by-id conn base-id))]
            {:starts [left right]
             :exclude (if base (reachable-ids conn base) #{})
             :range :symmetric
             :left left :right right :base base})
          {:starts [right]
           :exclude (reachable-ids conn left)
           :range :difference
           :left left :right right})))))

(defn diff-pair
  "Resolve one Git range expression to the two endpoints used by `git diff`."
  [conn expression]
  (let [{:keys [range left right base]} (selection conn expression)]
    (case range
      :difference [left right]
      :symmetric [(or base
                      (throw (ex-info "no merge base"
                                      {:left left :right right})))
                  right]
      (throw (ex-info (str "not a revision range: " expression)
                      {:revision expression})))))
