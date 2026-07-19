(ns geschichte.git-pack-test
  (:require [clojure.test :refer [deftest is]]
            [geschichte.git.object :as object]
            [geschichte.git.pack :as pack])
  (:import [java.nio ByteBuffer ByteOrder]
           [java.security MessageDigest]
           [java.util Arrays]))

(deftest undeltified-pack-has-canonical-envelope
  (let [blob (object/utf8 "hello\n")
        oid (object/object-id :blob blob)
        bytes (pack/encode {oid {:type :blob :payload blob}})
        n (alength ^bytes bytes)
        prefix (Arrays/copyOfRange bytes 0 (- n 20))
        trailer (Arrays/copyOfRange bytes (- n 20) n)
        header (doto (ByteBuffer/wrap bytes 4 8)
                 (.order ByteOrder/BIG_ENDIAN))]
    (is (= "PACK" (String. ^bytes bytes 0 4 "US-ASCII")))
    (is (= 2 (.getInt header)))
    (is (= 1 (.getInt header)))
    (is (= (seq trailer)
           (seq (.digest (MessageDigest/getInstance "SHA-1") prefix))))))
