(ns geschichte.diff-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [geschichte.diff :as diff]))

(defn- reconstructed-b
  [{:keys [a-lines b-lines edits]}]
  (vec
   (mapcat (fn [{:keys [op a-start a-count b-start b-count]}]
             (case op
               :equal (subvec a-lines a-start (+ a-start a-count))
               :delete []
               :insert (subvec b-lines b-start (+ b-start b-count))))
           edits)))

(defn- sequences-of [alphabet n]
  (if (zero? n)
    [[]]
    (for [prefix (sequences-of alphabet (dec n))
          x alphabet]
      (conj (vec prefix) x))))

(defn- lcs-length [a b]
  ;; Tiny quadratic oracle used only for exhaustive short-sequence tests.
  (let [m (count b)]
    (last
     (reduce (fn [previous x]
               (reduce (fn [row j]
                         (conj row
                               (if (= x (nth b (dec j)))
                                 (inc (nth previous (dec j)))
                                 (max (nth previous j) (peek row)))))
                       [0]
                       (range 1 (inc m))))
             (vec (repeat (inc m) 0))
             a))))

(defn- edit-cost [result]
  (reduce + (map (fn [{:keys [op a-count b-count]}]
                   (case op
                     :delete a-count
                     :insert b-count
                     0))
                 (:edits result))))

(deftest text-lines-preserves-final-newline
  (is (= {:lines [] :final-newline? false} (diff/text-lines "")))
  (is (= {:lines ["a"] :final-newline? false} (diff/text-lines "a")))
  (is (= {:lines ["a"] :final-newline? true} (diff/text-lines "a\n")))
  (is (= {:lines ["a" ""] :final-newline? true} (diff/text-lines "a\n\n")))
  (is (= {:lines ["a\r"] :final-newline? true} (diff/text-lines "a\r\n"))))

(deftest structured-edits-have-source-coordinates
  (let [result (diff/diff-lines ["a" "b" "c"] ["a" "B" "c" "d"])]
    (is (= [{:op :equal :a-start 0 :a-count 1 :b-start 0 :b-count 1}
            {:op :delete :a-start 1 :a-count 1 :b-start 1 :b-count 0}
            {:op :insert :a-start 2 :a-count 0 :b-start 1 :b-count 1}
            {:op :equal :a-start 2 :a-count 1 :b-start 2 :b-count 1}
            {:op :insert :a-start 3 :a-count 0 :b-start 3 :b-count 1}]
           (:edits result)))
    (is (= ["a" "B" "c" "d"] (reconstructed-b result)))))

(deftest common-edit-shapes-reconstruct-target
  (doseq [[a b] [[[] []]
                 [[] ["a"]]
                 [["a"] []]
                 [["a"] ["a"]]
                 [["a"] ["b"]]
                 [["a" "b" "c"] ["a" "x" "b" "c"]]
                 [["a" "b" "c"] ["b"]]
                 [["same" "same" "x"] ["same" "x" "same"]]
                 [(mapv str (range 100))
                  (assoc (mapv str (range 100)) 50 "changed")]]]
    (testing (str "a=" (pr-str a) " b=" (pr-str b))
      (let [result (diff/diff-lines a b)]
        (is (= b (reconstructed-b result)))))))

(deftest normalization-does-not-destroy-rendered-lines
  (let [result (diff/diff-lines ["Alpha" "context"] ["alpha" "changed"]
                                {:normalize #(.toLowerCase %)})]
    (is (= :equal (-> result :edits first :op)))
    (is (= [[:keep "Alpha"] [:del "context"] [:add "changed"]]
           (diff/operations result)))))

(deftest unified-rendering-and-final-newline
  (let [result (diff/diff-text "a\nb\nc\n" "a\nB\nc\n")]
    (is (= (str "--- old\n+++ new\n"
                "@@ -1,3 +1,3 @@\n"
                " a\n-b\n+B\n c\n")
           (diff/unified result {:a-name "old" :b-name "new"}))))
  (let [result (diff/diff-text "a" "a\n")
        patch (diff/unified result)]
    (is (= [[:del "a"] [:add "a"]] (diff/operations result)))
    (is (re-find #"-a\n\\ No newline at end of file\n\+a\n" patch))))

(deftest trace-budget-has-valid-bounded-fallback
  (let [a (mapv #(str "old-" %) (range 100))
        b (mapv #(str "new-" %) (range 100))
        result (diff/diff-lines a b {:max-trace-cells 1})]
    (is (= [:delete :insert] (mapv :op (:edits result))))
    (is (= b (reconstructed-b result)))))

(deftest myers-is-minimal-on-exhaustive-short-sequences
  (let [sequences (vec (mapcat #(sequences-of ["a" "b"] %) (range 5)))]
    (doseq [a sequences
            b sequences]
      (let [result (diff/diff-lines a b)
            expected (- (+ (count a) (count b))
                        (* 2 (lcs-length a b)))]
        (is (= b (reconstructed-b result)) [a b (:edits result)])
        (is (= expected (edit-cost result)) [a b (:edits result)])))))
