(ns geschichte.tree-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [geschichte.tree :as tree]))

(deftest canonical-paths-and-prefixes
  (doseq [path ["" "." "a/.." "a//b" "/a" "a/" "a\\b" (str "a" (char 0) "b")]]
    (is (= ::tree/invalid-path
           (try (tree/assert-path! path) nil
                (catch #?(:clj Exception :cljs :default) error (:type (ex-data error)))))))
  (is (= "wiki/研究.md" (tree/assert-path! "wiki/研究.md")))
  (is (= [["a" "a/b"] ["a" "a/b/c"] ["a/b" "a/b/c"]]
         (tree/prefix-conflicts ["a" "a-b" "a/b" "a/b/c"]))))

(deftest files-and-explicit-directories-are-consistent
  (is (= {"a" :a "a0" :b "other/child" :c}
         (tree/assert-valid! {"a" :a "a0" :b "other/child" :c} ["other" "empty"])))
  (doseq [[files dirs] [[{"a" :a "a/b" :b} []]
                        [{"a/b" :b "a-b" :c "a" :a} []]
                        [{"a" :a} ["a"]]
                        [{"a" :a} ["a/empty"]]]]
    (is (= ::tree/path-conflict
           (try (tree/assert-valid! files dirs) nil
                (catch #?(:clj Exception :cljs :default) error (:type (ex-data error))))))))
