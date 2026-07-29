(ns geschichte.merge.text-test
  "diff3, exhaustively — because this is where a merge tool loses work.

   `plan-trees` compares tree ENTRIES, so without a content merge two branches
   touching one file always collide even when they edited different lines. This
   is the function that decides which of those collisions are real.

   Every case here is one a wrong implementation gets wrong QUIETLY: a dropped
   line, a duplicated hunk, two edits interleaved into nonsense. None of them
   throw. So the assertions are on exact output, never on a flag."
  (:require [clojure.test :refer [deftest is testing]]
            [geschichte.merge.text :as tm]))

(defn- lines [& ls] (vec ls))

(deftest disjoint-edits-merge-cleanly
  ;; THE case this exists for: one fork edits the top, another the bottom.
  ;; geschichte calls it a conflict; it is not one.
  (let [base (lines "one" "two" "three" "four" "five")
        ours (lines "ONE" "two" "three" "four" "five")
        theirs (lines "one" "two" "three" "four" "FIVE")
        r (tm/merge-lines base ours theirs)]
    (is (:clean? r))
    (is (= (lines "ONE" "two" "three" "four" "FIVE") (:lines r))
        "both edits present, everything untouched preserved, nothing doubled")))

(deftest an-untouched-file-comes-back-identical
  (let [base (lines "a" "b" "c")
        r (tm/merge-lines base base base)]
    (is (:clean? r))
    (is (= base (:lines r)))))

(deftest one-sided-edits-are-taken-whole
  (testing "only ours changed"
    (let [base (lines "a" "b" "c")
          r (tm/merge-lines base (lines "a" "B" "c") base)]
      (is (:clean? r))
      (is (= (lines "a" "B" "c") (:lines r)))))
  (testing "only theirs changed"
    (let [base (lines "a" "b" "c")
          r (tm/merge-lines base base (lines "a" "b" "C"))]
      (is (:clean? r))
      (is (= (lines "a" "b" "C") (:lines r))))))

(deftest the-same-edit-made-twice-is-not-a-conflict
  ;; Two agents told to do one thing, both doing it. Reporting that as a clash
  ;; would send a reviewer to arbitrate between two identical answers.
  (let [base (lines "a" "old" "c")
        both (lines "a" "new" "c")
        r (tm/merge-lines base both both)]
    (is (:clean? r))
    (is (= both (:lines r)))))

(deftest competing-edits-to-one-line-conflict
  (let [base (lines "a" "old" "c")
        r (tm/merge-lines base (lines "a" "OURS" "c") (lines "a" "THEIRS" "c"))]
    (is (not (:clean? r)))
    (is (= 1 (count (:conflicts r))))
    (let [c (first (:conflicts r))]
      (is (= (lines "old") (:base c)))
      (is (= (lines "OURS") (:ours c)))
      (is (= (lines "THEIRS") (:theirs c)))
      "all three sides reported — a conflict a human cannot see is one they
       can only refuse")))

(deftest insertions-at-different-points-both-survive
  (let [base (lines "a" "b" "c")
        ours (lines "a" "OURS" "b" "c")
        theirs (lines "a" "b" "THEIRS" "c")
        r (tm/merge-lines base ours theirs)]
    (is (:clean? r))
    (is (= (lines "a" "OURS" "b" "THEIRS" "c") (:lines r)))))

(deftest insertions-at-the-same-point-conflict
  ;; Adjacency counts as overlap. Two inserts at one seam have no defensible
  ;; order, and picking one silently is how a merge invents code nobody wrote.
  (let [base (lines "a" "b")
        r (tm/merge-lines base (lines "a" "OURS" "b") (lines "a" "THEIRS" "b"))]
    (is (not (:clean? r)))
    (is (= 1 (count (:conflicts r))))))

(deftest a-deletion-on-one-side-is-applied
  (let [base (lines "a" "b" "c")
        r (tm/merge-lines base (lines "a" "c") base)]
    (is (:clean? r))
    (is (= (lines "a" "c") (:lines r)))))

(deftest delete-versus-edit-of-the-same-line-conflicts
  (let [base (lines "a" "b" "c")
        r (tm/merge-lines base (lines "a" "c") (lines "a" "B" "c"))]
    (is (not (:clean? r)))
    (is (= (lines "b") (:base (first (:conflicts r)))))))

(deftest edits-at-the-file-edges
  (testing "both ends, disjoint"
    (let [base (lines "a" "b" "c")
          r (tm/merge-lines base (lines "A" "b" "c") (lines "a" "b" "C"))]
      (is (:clean? r))
      (is (= (lines "A" "b" "C") (:lines r)))))
  (testing "append on both sides is a conflict at one seam"
    (let [base (lines "a")
          r (tm/merge-lines base (lines "a" "ours") (lines "a" "theirs"))]
      (is (not (:clean? r))))))

(deftest an-empty-base-with-content-on-both-sides-conflicts
  (let [r (tm/merge-lines [] (lines "ours") (lines "theirs"))]
    (is (not (:clean? r)))))

(deftest multiple-independent-hunks-all-land
  (let [base (lines "1" "2" "3" "4" "5" "6" "7" "8" "9")
        ours (lines "1" "TWO" "3" "4" "5" "6" "7" "8" "9")
        theirs (lines "1" "2" "3" "4" "FIVE" "6" "7" "EIGHT" "9")
        r (tm/merge-lines base ours theirs)]
    (is (:clean? r))
    (is (= (lines "1" "TWO" "3" "4" "FIVE" "6" "7" "EIGHT" "9") (:lines r))
        "three separate edits across two sides, none lost, none duplicated")))

(deftest merge-text-round-trips-exactly
  (testing "a clean merge reproduces the file"
    (let [r (tm/merge-text "one\ntwo\nthree\n" "ONE\ntwo\nthree\n" "one\ntwo\nTHREE\n")]
      (is (:clean? r))
      (is (= "ONE\ntwo\nTHREE\n" (:text r))
          "including the trailing newline — a merge that eats it rewrites every
           line of the next diff")))
  (testing "a file with no trailing newline keeps not having one"
    (let [r (tm/merge-text "a\nb" "A\nb" "a\nb")]
      (is (:clean? r))
      (is (= "A\nb" (:text r))))))

(deftest merge-text-reports-conflicts-and-still-returns-text
  (let [r (tm/merge-text "x\n" "ours\n" "theirs\n")]
    (is (not (:clean? r)))
    (is (string? (:text r)) "callers that want to show something still can")
    (is (seq (:conflicts r)))))
