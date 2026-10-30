(ns geschichte.merge-core-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [geschichte.merge.core :as merge]
            [clojure.string :as str]))

(deftest portable-graph-traversal
  (let [parents {:root [] :left [:root] :right [:root] :tip [:left :right]}
        parents-of #(get parents % [])]
    (is (= {:tip 0 :left 1 :right 1 :root 2}
           (merge/ancestor-distances parents-of :tip)))
    (is (= :root (merge/merge-base parents-of :left :right)))
    (is (= :left (merge/merge-base parents-of :left :tip)))))

(deftest criss-cross-merge-base-is-deterministic
  ;; :ours and :theirs each reach BOTH :a1 and :a2 at equal combined distance
  ;; (2 + 2). The tie must resolve deterministically by id — never by traversal
  ;; order or an entity id — so every peer picks the same base. `:a1` sorts
  ;; before `:a2`.
  (let [parents {:a1 [] :a2 []
                 :m1 [:a1 :a2] :m2 [:a2 :a1]
                 :ours [:m1] :theirs [:m2]}
        parents-of #(get parents % [])
        ours-dist (merge/ancestor-distances parents-of :ours)
        theirs-dist (merge/ancestor-distances parents-of :theirs)]
    (is (= 4 (+ (ours-dist :a1) (theirs-dist :a1))
           (+ (ours-dist :a2) (theirs-dist :a2)))
        "both candidates are at equal combined distance")
    (is (= :a1 (merge/merge-base parents-of :ours :theirs)))
    (is (= :a1 (merge/merge-base-from-distances ours-dist theirs-dist))
        "the tie-break policy is a pure function of the two distance maps")))

(deftest portable-three-tree-plan
  (let [base {"same" :a "ours" :old "theirs" :old "conflict" :base}
        ours {"same" :a "ours" :new "theirs" :old "conflict" :ours}
        theirs {"same" :a "ours" :old "theirs" :new "conflict" :theirs}
        plan (merge/plan-trees :base :ours :theirs base ours theirs)]
    (is (= {"same" :a "ours" :new "theirs" :new} (:tree plan)))
    (is (= {:base :base :ours :ours :theirs :theirs}
           (get-in plan [:conflicts "conflict"])))
    (is (false? (:clean? plan)))))

(deftest file-directory-conflicts-are-never-clean
  (doseq [[ours theirs] [[{"a" :file} {"a/b" :child}]
                         [{"a/b" :child} {"a" :file}]
                         [{"a" :file "a-b" :sibling} {"a/b/c" :child}]]]
    (let [plan (merge/plan-trees :base :ours :theirs {} ours theirs)]
      (is (false? (:clean? plan)))
      (is (= :file-directory (get-in plan [:conflicts "a" :kind])))
      (is (not (contains? (:tree plan) "a")))
      (is (not-any? #(str/starts-with? % "a/") (keys (:tree plan))))))
  (let [plan (merge/plan-trees :base :ours :theirs {} {"a/b" :b} {"a/c" :c})]
    (is (:clean? plan))
    (is (= {"a/b" :b "a/c" :c} (:tree plan)))))

(deftest bounded-valid-tree-model
  ;; Independent small reference model: any two different paths must not have
  ;; a file/directory relationship. Check every valid subset pair, including
  ;; lexically interleaved names that defeat adjacent-only prefix checks.
  (let [paths ["a" "a-b" "a/b" "a/b/c" "x/y"]
        valid? (fn [paths]
                 (not-any? true? (for [a paths b paths :when (not= a b)]
                                   (str/starts-with? b (str a "/")))))
        subsets (reduce (fn [sets path] (into sets (map #(conj % path) sets))) [#{}] paths)
        trees (map #(zipmap % %) (filter valid? subsets))]
    (doseq [ours trees theirs trees]
      (let [union (merge ours theirs)
            plan (merge/plan-trees :base :ours :theirs {} ours theirs)]
        (is (= (valid? (keys union)) (:clean? plan)))
        (is (valid? (keys (:tree plan))))
        (when (:clean? plan) (is (= union (:tree plan))))))))

(deftest shortcut-parents-characterize-the-distance-policy
  ;; B descends from A, so Git's best-common-ancestor antichain is {B}.
  ;; Redundant shortcut edges make the documented distance policy select A.
  ;; Keep this visible until a separate policy/version decision is made.
  (let [parents {:a [] :b [:a] :o1 [:b] :t1 [:b]
                 :ours [:o1 :a] :theirs [:t1 :a]}
        ours (merge/ancestor-distances parents :ours)
        theirs (merge/ancestor-distances parents :theirs)]
    (is (= 2 (+ (ours :a) (theirs :a))))
    (is (= 4 (+ (ours :b) (theirs :b))))
    (is (= :a (merge/merge-base parents :ours :theirs)))
    (is (= :a (merge/merge-base #(reverse (parents %)) :ours :theirs)))
    (is (contains? (merge/ancestor-distances parents :b) :a))))
