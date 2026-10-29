(ns geschichte.strict-utf8-test
  (:require [geschichte.bytes :as bytes]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])))

(deftest strict-utf8-preserves-authored-text
  (doseq [text ["" "α漢字🦋" " \t\r\n終\r\n" "\uFEFFtitle\n" "�"]]
    (let [encoded (bytes/utf8 text)]
      (is (= text (bytes/decode-utf8-strict encoded)))
      (is (bytes/same-bytes? encoded (bytes/utf8 (bytes/decode-utf8-strict encoded)))))))

(deftest strict-utf8-refuses-malformed-sequences
  (doseq [values [[255] [254] [253] [192 175] [226 130] [237 160 128] [244 144 128 128]]]
    (is (try (bytes/decode-utf8-strict (bytes/from-values values)) false
             (catch #?(:clj Exception :cljs :default) _ true))))
  (is (= "�" (bytes/decode-utf8 (bytes/from-values [255])))
      "the existing permissive decoder remains compatible"))
