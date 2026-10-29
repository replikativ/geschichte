(ns geschichte.merge.overlap-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [geschichte.merge.text :as text]))

(deftest spanning-edit-collects-all-other-side-hunks
  (let [base "0\n1\n2\n3\n4\n5\n6\n7\n8\n"
        ours "0\n1\nOURS\n8\n"
        theirs "0\n1\n2\nT3\n4\n5\nT6\n7\n8\n"
        a (text/merge-text base ours theirs)
        b (text/merge-text base theirs ours)]
    (is (false? (:clean? a)))
    (is (= ours (:text a)))
    (is (= theirs (:text b)))
    (is (= [{:base ["2" "3" "4" "5" "6" "7"]
             :ours ["OURS"] :theirs ["2" "T3" "4" "5" "T6" "7"]}]
           (:conflicts a)))
    (is (= (mapv #(-> % (assoc :ours (:theirs %) :theirs (:ours %))) (:conflicts a))
           (:conflicts b)))))

(deftest alternating-overlaps-form-one-complete-cluster
  (let [base (mapv str (range 12))
        ours ["0" "O1" "4" "5" "O6" "10" "11"]
        theirs ["0" "1" "T2" "8" "T9" "11"]
        result (text/merge-lines base ours theirs)]
    (is (false? (:clean? result)))
    (is (= ours (:lines result)))
    (is (= 1 (count (:conflicts result))))
    (is (= (subvec base 1 11) (get-in result [:conflicts 0 :base])))
    (is (= ["O1" "4" "5" "O6" "10"] (get-in result [:conflicts 0 :ours])))
    (is (= ["1" "T2" "8" "T9"] (get-in result [:conflicts 0 :theirs])))))

(deftest insertion-boundaries-preserve-all-side-content
  (doseq [[ours theirs] [[["0" "O" "3"] ["0" "I" "1" "2" "J" "3"]]
                         [["0" "I" "1" "2" "J" "3"] ["0" "O" "3"]]]]
    (let [result (text/merge-lines ["0" "1" "2" "3"] ours theirs)]
      (is (false? (:clean? result)))
      (is (= ours (:lines result)))
      (is (= ours (into ["0"] (concat (get-in result [:conflicts 0 :ours]) ["3"])))))))

(deftest single-range-conflicts-are-symmetric-and-never-revisit-base
  (let [base (mapv str (range 7))]
    (doseq [a (range 1 5) b (range (inc a) 6)
            c (range 1 5) d (range (inc c) 6)]
      (let [ours (into (conj (subvec base 0 a) "OURS") (subvec base b))
            theirs (into (conj (subvec base 0 c) "THEIRS") (subvec base d))
            x (text/merge-lines base ours theirs)
            y (text/merge-lines base theirs ours)]
        (is (= (:clean? x) (:clean? y)))
        (if (:clean? x)
          (is (= (:lines x) (:lines y)))
          (do (is (= ours (:lines x)))
              (is (= theirs (:lines y)))
              (is (= (mapv #(-> % (assoc :ours (:theirs %) :theirs (:ours %))) (:conflicts x))
                     (:conflicts y)))))))))
