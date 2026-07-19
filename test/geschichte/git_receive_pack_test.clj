(ns geschichte.git-receive-pack-test
  (:require [clojure.test :refer [deftest is]]
            [geschichte.git.object :as object]
            [geschichte.git.pkt-line :as pkt]
            [geschichte.git.receive-pack :as receive]))

(deftest receive-pack-advertisement-request-and-report
  (let [oid (apply str (repeat 40 "a"))
        advertised
        (pkt/encode [(str oid " refs/heads/main\u0000report-status-v2 atomic\n")
                     :flush])]
    (is (= {:refs {"refs/heads/main" oid}
            :capabilities #{"report-status-v2" "atomic"}}
           (receive/parse-advertisement advertised)))
    (let [request
          (receive/request
           {:updates [{:old oid :new (apply str (repeat 40 "b"))
                       :ref "refs/heads/main"}]
            :capabilities ["report-status-v2" "atomic"]
            :pack (object/utf8 "PACK...")})]
      (is (.endsWith (String. ^bytes request "ISO-8859-1") "PACK..."))))
  (is (= {:unpack "ok"
          :refs {"refs/heads/main" {:status :ok}}
          :other []}
         (receive/parse-report
          (pkt/encode ["unpack ok\n" "ok refs/heads/main\n" :flush])))))
