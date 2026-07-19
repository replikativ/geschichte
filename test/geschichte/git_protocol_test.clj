(ns geschichte.git-protocol-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [geschichte.git.client :as client]
            [geschichte.git.object :as object]
            [geschichte.git.pack :as pack]
            [geschichte.git.pkt-line :as pkt]
            [geschichte.git.protocol-v2 :as protocol]
            [geschichte.git.store :as store]
            [geschichte.repo :as repo])
  (:import [java.io ByteArrayInputStream]))

(deftest pkt-line-roundtrip
  (let [frames [(object/utf8 "hello\n") :delimiter (byte-array 0)
                :flush :response-end]
        decoded (pkt/decode (pkt/encode frames))]
    (is (= ["hello\n" :delimiter "" :flush :response-end]
           (mapv #(if (bytes? %) (pkt/text %) %) decoded)))
    (testing "malformed and truncated frames are rejected"
      (is (thrown? clojure.lang.ExceptionInfo
                   (pkt/decode (object/utf8 "0003"))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (pkt/decode (object/utf8 "0008abc")))))))

(deftest protocol-v2-advertisement-and-requests
  (let [advertisement
        (pkt/encode ["version 2\n" "agent=git/2.51.0\n"
                     "ls-refs=unborn\n" "fetch=shallow wait-for-done\n" :flush])]
    (is (= {"agent" "git/2.51.0"
            "fetch" "shallow wait-for-done"
            "ls-refs" "unborn"}
           (protocol/parse-advertisement advertisement))))
  (is (= ["command=ls-refs\n" :delimiter "peel\n" "symrefs\n"
          "ref-prefix refs/heads/\n" :flush]
         (mapv #(if (bytes? %) (pkt/text %) %)
               (pkt/decode
                (protocol/ls-refs-request
                 {:prefixes ["refs/heads/"]})))))
  (is (= [{:oid (apply str (repeat 40 "a"))
           :ref "HEAD" :unborn? false
           :attributes {:symref-target "refs/heads/main"}}
          {:oid nil :ref "refs/heads/empty" :unborn? true :attributes {}}]
         (protocol/parse-ls-refs
          (pkt/encode [(str (apply str (repeat 40 "a"))
                            " HEAD symref-target:refs/heads/main\n")
                       "unborn refs/heads/empty\n" :flush]))))
  (is (= ["command=fetch\n" "object-format=sha1\n" :delimiter
          (str "want " (apply str (repeat 40 "a")) "\n")
          "done\n" :flush]
         (mapv #(if (bytes? %) (pkt/text %) %)
               (pkt/decode
                (protocol/fetch-request
                 {:capabilities ["object-format=sha1"]
                  :wants [(apply str (repeat 40 "a"))]})))))
  (let [pack (object/utf8 "PACK bytes")
        response (pkt/encode
                  ["acknowledgments\n" "NAK\n" :delimiter "packfile\n"
                   (object/concat-bytes (byte-array [2])
                                        (object/utf8 "counting\n"))
                   (object/concat-bytes (byte-array [1])
                                        (java.util.Arrays/copyOfRange pack 0 4))
                   (object/concat-bytes (byte-array [1])
                                        (java.util.Arrays/copyOfRange
                                         pack 4 (alength pack)))
                   :flush])
        parsed (protocol/parse-fetch-response response)]
    (is (= (seq pack) (seq (:pack parsed))))
    (is (= "counting\n" (:progress parsed)))))

(deftest fetch-response-import-composes
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? true :commit-graph? true}
        payload (object/utf8 "fetched\n")
        oid (object/object-id :blob payload)
        packed (pack/encode {oid {:type :blob :payload payload}})
        response (pkt/encode
                  ["packfile\n"
                   (object/concat-bytes (byte-array [1]) packed)
                   :flush])]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (repo/init! conn)
        (let [result (client/ingest-fetch! conn response)]
          (is (= 1 (:persisted result)))
          (is (= (seq payload) (seq (store/read-payload conn oid))))
          (let [remote-response
                (pkt/encode [(str oid
                                  " HEAD symref-target:refs/heads/main\n")
                             (str oid " refs/heads/main\n") :flush])]
            (is (= 2 (count (client/ingest-ls-refs!
                             conn "origin" remote-response))))
            (is (= #{"refs/remotes/origin/HEAD"
                     "refs/remotes/origin/main"}
                   (set (keys (store/refs conn)))))))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(deftest streaming-fetch-response-import-composes
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? true :commit-graph? true}
        payload (object/utf8 "streamed\n")
        oid (object/object-id :blob payload)
        packed (pack/encode {oid {:type :blob :payload payload}})
        split (quot (alength ^bytes packed) 2)
        response (pkt/encode
                  ["packfile\n"
                   (object/concat-bytes
                    (byte-array [2]) (object/utf8 "receiving\n"))
                   (object/concat-bytes
                    (byte-array [1])
                    (java.util.Arrays/copyOfRange packed 0 split))
                   (object/concat-bytes
                    (byte-array [1])
                    (java.util.Arrays/copyOfRange packed split
                                                  (alength ^bytes packed)))
                   :flush])]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (repo/init! conn)
        (let [result (client/ingest-fetch-stream!
                      conn (ByteArrayInputStream. response))]
          (is (= 1 (:persisted result)))
          (is (= "receiving\n" (:progress result)))
          (is (= (seq payload) (seq (store/read-payload conn oid)))))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))
