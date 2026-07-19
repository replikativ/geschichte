(ns geschichte.git-wire-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [geschichte.bytes :as bytes]
            [geschichte.git.object :as object]
            [geschichte.git.pkt-line :as pkt]
            [geschichte.git.protocol-v2 :as protocol]
            [geschichte.git.receive-pack :as receive]))

(deftest protocol-v2-wire-roundtrip
  (let [advertisement
        (pkt/encode ["version 2\n" "agent=git/2.51.0\n"
                     "ls-refs=unborn\n" "fetch=shallow wait-for-done\n" :flush])]
    (is (= {"agent" "git/2.51.0"
            "fetch" "shallow wait-for-done"
            "ls-refs" "unborn"}
           (protocol/parse-advertisement advertisement))))
  (is (= ["command=ls-refs\n" :delimiter "peel\n" "symrefs\n"
          "ref-prefix refs/heads/\n" :flush]
         (mapv #(if (bytes/byte-buffer? %) (pkt/text %) %)
               (pkt/decode
                (protocol/ls-refs-request {:prefixes ["refs/heads/"]}))))))

(deftest sideband-pack-reassembly
  (let [pack (object/utf8 "PACK bytes")
        response (pkt/encode
                  ["packfile\n"
                   (object/concat-bytes (bytes/from-values [2])
                                        (object/utf8 "counting\n"))
                   (object/concat-bytes (bytes/from-values [1])
                                        (bytes/slice pack 0 4))
                   (object/concat-bytes (bytes/from-values [1])
                                        (bytes/slice pack 4 (bytes/length pack)))
                   :flush])
        parsed (protocol/parse-fetch-response response)]
    (is (bytes/same-bytes? pack (:pack parsed)))
    (is (= "counting\n" (:progress parsed)))))

(deftest receive-pack-wire-roundtrip
  (let [oid (apply str (repeat 40 "a"))
        advertised
        (receive/parse-advertisement
         (pkt/encode [(str oid " refs/heads/main\u0000report-status atomic\n")
                      :flush]))]
    (is (= oid (get-in advertised [:refs "refs/heads/main"])))
    (is (= #{"report-status" "atomic"} (:capabilities advertised))))
  (is (= {:unpack "ok"
          :refs {"refs/heads/main" {:status :ok}}
          :other []}
         (receive/parse-report
          (pkt/encode ["unpack ok\n" "ok refs/heads/main\n" :flush])))))
