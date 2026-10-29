(ns geschichte.merge.bounded-diff-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [geschichte.budget :as budget]
            [geschichte.diff :as diff]
            [geschichte.merge.text :as text]
            [geschichte.merge.core :as core]
            [geschichte.merge.policy :as policy]))
(defn- operation [limits]
  (budget/create (update (policy/recommended-policy) :limits merge limits)))
(defn- refusal [f]
  (try (f) nil (catch #?(:clj Exception :cljs :default) error (ex-data error))))

(deftest bounded-diff-retains-native-results
  (doseq [[a b] [[[] []] [["a"] ["a"]] [["a" "b"] ["b" "a"]]
                 [["α" "β" "終"] ["α" "γ" "終"]]]]
    (is (= (diff/diff-lines a b)
           (diff/diff-lines a b {:merge-budget (operation {})})))))

(deftest diff-frontier-and-trace-refuse-before-allocation
  (doseq [[limits metric] [[{:text-frontier-cells 1} :text-frontier-cells]
                           [{:text-trace-cells 0} :text-trace-cells]]]
    (let [b (operation limits)]
      (is (= metric (:metric (refusal #(diff/diff-lines ["a"] ["b"] {:merge-budget b})))))))
  (let [b (operation {})
        failure (refusal #(diff/diff-lines ["a"] ["b"] {:merge-budget b :max-trace-cells 0}))]
    (is (= ::budget/exceeded (:type failure)))
    (is (= :text-trace-cells (:metric failure)))
    (is (= failure (refusal #(budget/check! b)))))
  (is (= [:delete :insert]
         (mapv :op (:edits (diff/diff-lines ["a"] ["b"] {:max-trace-cells 0}))))))

(deftest long-equal-lines-and-prefixes-count-work
  (let [b (operation {:text-work 5})]
    (is (= :text-work (:metric (refusal #(diff/diff-lines ["long equal string"]
                                                          ["long equal string"] {:merge-budget b})))))))

(deftest shared-budget-covers-both-base-diffs-and-result
  (let [base "0\n1\n2\n3\n4\n5\n6\n7\n8\n"
        ours "0\n1\nOURS\n8\n"
        theirs "0\n1\n2\nT3\n4\n5\nT6\n7\n8\n"
        b (operation {})]
    (is (= (text/merge-text base ours theirs)
           (text/merge-text base ours theirs {:merge-budget b})))
    (is (pos? (get-in (budget/snapshot b) [:used :text-trace-cells])))
    (is (pos? (get-in (budget/snapshot b) [:used :merge-output-bytes]))))
  (let [b (operation {:merge-output-bytes 0})]
    (is (= :merge-output-bytes
           (:metric (refusal #(text/merge-text "a" "a" "a" {:merge-budget b})))))))

(deftest bounded-pure-core-returns-values-without-native-budget-state
  (let [b (operation {}) tree {"f" {:content "content" :size 1 :mode 33188}}
        expected (core/plan-trees :base :ours :theirs tree tree tree)
        actual (core/plan-trees :base :ours :theirs tree tree tree {:merge-budget b})]
    (is (= expected actual))
    (is (= ::budget/exceeded (:type (refusal #(budget/exceed! b :text-work 0 1)))))
    (is (= #{"f" "new"} (set (keys (assoc (:tree actual) "new" {:size 0})))))
    (is (= {"new" {}} (assoc (:conflicts actual) "new" {})))
    (is (= #{"new"} (conj (:merged actual) "new")))))
