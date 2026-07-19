(ns geschichte.git-pkt-line-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [geschichte.bytes :as bytes]
            [geschichte.git.object :as object]
            [geschichte.git.pkt-line :as pkt]))

(deftest pkt-line-roundtrip
  (let [frames [(object/utf8 "hello\n") :delimiter (bytes/empty-bytes)
                :flush :response-end]
        decoded (pkt/decode (pkt/encode frames))]
    (is (= ["hello\n" :delimiter "" :flush :response-end]
           (mapv #(if (bytes/byte-buffer? %) (pkt/text %) %) decoded)))
    (testing "malformed and truncated frames are rejected"
      (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core.ExceptionInfo)
                   (pkt/decode (object/utf8 "0003"))))
      (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core.ExceptionInfo)
                   (pkt/decode (object/utf8 "0008abc")))))))

(deftest byte-concatenation-retains-unsigned-values
  (let [joined (bytes/concat-bytes (bytes/from-values [0 127])
                                   (bytes/from-values [128 255]))]
    (is (bytes/same-bytes? (bytes/from-values [0 127 128 255]) joined))))
