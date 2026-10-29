(ns geschichte.merge.whitespace-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [geschichte.merge.text :as text]))

(deftest unchanged-whitespace-is-authored-content
  (doseq [value ["" " " "\t" "\n" "\n\n" " \n" "\t\r\n" "\r\n\r\n"]]
    (let [result (text/merge-text value value value)]
      (is (:clean? result))
      (is (= value (:text result))))))

(deftest one-sided-and-identical-whitespace-edits-survive
  (doseq [[base ours theirs expected]
          [["\n" " \n" "\n" " \n"]
           ["\t" "\t" "\t\t" "\t\t"]
           ["\r\n" "\t\r\n" "\t\r\n" "\t\r\n"]
           ["\n" "" "\n" ""]]]
    (let [result (text/merge-text base ours theirs)]
      (is (:clean? result))
      (is (= expected (:text result))))))

(deftest disjoint-whitespace-only-edits-preserve-both-sides
  ;; Native git merge-file also produces this exact clean result. Distinct
  ;; space/tab/CR lines provide anchors without any non-whitespace text.
  (let [result (text/merge-text " \n\t\n\r\n"
                                "  \n\t\n\r\n"
                                " \n\t\n\r\r\n")]
    (is (:clean? result))
    (is (= "  \n\t\n\r\r\n" (:text result)))))

(deftest whitespace-only-inputs-are-not-silently-erased-before-diff3
  (let [result (text/merge-text "\n" " \n" "\n\n")]
    ;; Keep the existing diff3 alignment policy; the wrapper must preserve the
    ;; authored space and added newline instead of passing three empty vectors.
    (is (:clean? result))
    (is (= " \n\n" (:text result)))))
