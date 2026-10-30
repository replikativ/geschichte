(ns geschichte.merge.bounded-delta-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [geschichte.budget :as budget]
            [geschichte.merge.delta :as delta]
            [geschichte.merge.policy :as policy]
            [geschichte.text-budget :as text-budget]))

(defn- operation ([] (operation {}))
  ([limits] (budget/create (update (policy/recommended-policy) :limits merge limits))))
(defn- refusal [f]
  (try (f) nil (catch #?(:clj Exception :cljs :default) error (ex-data error))))
(defn- equal-edit [start n]
  {:op :equal :a-start start :a-count n :b-start start :b-count n})
(defn- encoded [edits] (pr-str {:final-newline? true :edits edits}))

(deftest bounded-generated-delta-preserves-lines
  (is (= "α\r\n \t\r\n終\r\n"
         (delta/decode (operation) "α\r\n \t\r\n終\r\n" (encoded [(equal-edit 0 3)])))
      (is (= "[\\\"#_]\n"
             (delta/decode (operation) "" (encoded [{:op :insert :a-start 0 :a-count 0
                                                     :b-start 0 :b-count 1 :lines ["[\\\"#_]"]}])))))

  (deftest malformed-generated-data-is-refused
    (doseq [stored ["#_ {} {}" "#foo {}" "{:edits ()}" "{} {}"
                    "{:x [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]}"
                    (str "{:x " (apply str (repeat 129 "9")) "}")]]
      (is (some? (refusal #(delta/decode (operation) "" stored)))))
    (doseq [edits [[(assoc (equal-edit 0 1) :a-start -1)]
                   [(equal-edit 0 2)]
                   [(equal-edit 0 1) (equal-edit 0 1)]
                   [{:op :insert :a-start 0 :a-count 0 :b-start 0 :b-count 1 :lines ["a\nb"]}]
                   [(assoc (equal-edit 0 1) :unknown true)]]]
      (is (= ::delta/invalid-delta (:type (refusal #(delta/decode (operation) "x\n" (encoded edits))))))))

  (deftest expansion-checks-before-output
    (let [b (operation {:object-bytes 3})]
      (is (= ::budget/exceeded
             (:type (refusal #(delta/decode b "a long base line\n" (encoded [(equal-edit 0 1)]))))))
      (is (nil? (get-in (budget/snapshot b) [:used :content-decoded-bytes]))))
    (let [b (operation {:content-decoded-bytes 3})]
      (is (= ::budget/exceeded
             (:type (refusal #(delta/decode b "abcd\n" (encoded [(equal-edit 0 1)]))))))))

  (deftest text-accounting-checks-long-single-lines
    (let [b (operation {:text-work 2})]
      (is (= ::budget/exceeded (:type (refusal #(text-budget/utf8-size b "abcdef")))))))
  (is (= 11 (text-budget/utf8-size (operation) "aα漢🦋\n"))))

(deftest generated-delta-parity-table
  (doseq [[base newline? edits expected]
          [["" false [] ""]
           ["" true [] "\n"]
           ["a\nb" false [(equal-edit 0 2)] "a\nb"]
           ["a\r\nb\r\n" true
            [(equal-edit 0 1)
             {:op :delete :a-start 1 :a-count 1 :b-start 1 :b-count 0}
             {:op :insert :a-start 2 :a-count 0 :b-start 1 :b-count 1 :lines ["漢\r"]}]
            "a\r\n漢\r\n"]]]
    (is (= expected (delta/decode (operation) base
                                  (pr-str {:final-newline? newline? :edits edits}))))))

(deftest lexical-bounds-and-mid-scan-cancellation
  (doseq [[limits metric] [[{:delta-forms 1} :delta-forms]
                           [{:delta-token-chars 2} :delta-token-chars]
                           [{:delta-edn-depth 1} :delta-edn-depth]]]
    (is (= metric (:metric (refusal #(delta/decode (operation limits) ""
                                                   (encoded [{:op :insert :a-start 0 :a-count 0
                                                              :b-start 0 :b-count 1 :lines ["x"]}])))))))
  (let [checks (atom 0)
        b (budget/create (assoc (policy/recommended-policy)
                                :cancelled? #(> (swap! checks inc) 40)))]
    (is (= ::budget/cancelled
           (:type (refusal #(delta/preflight! b (str "{:x \"" (apply str (repeat 1000 "α")) "\"}"))))))
    (is (= 41 @checks))))
