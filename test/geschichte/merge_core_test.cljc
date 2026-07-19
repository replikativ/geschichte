(ns geschichte.merge-core-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [geschichte.merge.core :as merge]))

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
