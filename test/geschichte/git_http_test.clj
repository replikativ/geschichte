(ns geschichte.git-http-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.git.http :as http]
            [geschichte.git.object :as object]
            [geschichte.git.pack :as pack]
            [geschichte.git.pkt-line :as pkt]
            [geschichte.repo :as repo]))

(deftest request-shapes-and-service-prelude
  (is (= {:method :get
          :url "https://example.test/repo.git/info/refs?service=git-upload-pack"
          :headers {"Accept" "application/x-git-upload-pack-advertisement"
                    "Git-Protocol" "version=2"}}
         (http/advertisement-request "https://example.test/repo.git/"
                                     "git-upload-pack")))
  (let [advertisement (pkt/encode ["# service=git-upload-pack\n" :flush
                                   "version 2\n" "ls-refs\n" :flush])
        send-fn (fn [_] {:status 200
                         :headers {"content-type"
                                   "application/x-git-upload-pack-advertisement"}
                         :body advertisement})]
    (is (= ["version 2\n" "ls-refs\n" :flush]
           (mapv #(if (bytes? %) (pkt/text %) %)
                 (pkt/decode
                  (http/advertise! send-fn "https://example.test/repo.git"
                                   "git-upload-pack")))))))

(deftest transport-neutral-smart-http-fetch
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? true :commit-graph? true}
        payload (object/utf8 "remote blob\n")
        oid (object/object-id :blob payload)
        packed (pack/encode {oid {:type :blob :payload payload}})
        calls (atom [])
        send-fn
        (fn [{:keys [method url body] :as request}]
          (swap! calls conj request)
          (cond
            (= method :get)
            {:status 200
             :headers {"content-type"
                       "application/x-git-upload-pack-advertisement"}
             :body (pkt/encode ["version 2\n" "ls-refs=unborn\n"
                                "fetch\n" "object-format=sha1\n" :flush])}

            (and (= method :post)
                 (= "command=ls-refs\n"
                    (pkt/text (first (pkt/decode body)))))
            {:status 200
             :headers {"content-type" "application/x-git-upload-pack-result"}
             :body (pkt/encode [(str oid " refs/heads/main\n") :flush])}

            :else
            {:status 200
             :headers {"content-type" "application/x-git-upload-pack-result"}
             :body (pkt/encode
                    ["packfile\n"
                     (object/concat-bytes (byte-array [1]) packed)
                     :flush])}))]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (repo/init! conn)
        (let [result (http/fetch! send-fn conn "origin"
                                  "https://example.test/repo.git" nil)]
          (is (= 1 (:persisted result)))
          (is (= 3 (count @calls)))
          (is (= oid (get-in result [:refs 0 :oid]))))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(deftest smart-http-ls-remote-does-not-need-a-repository
  (let [calls (atom [])
        oid (apply str (repeat 40 "a"))
        send-fn (fn [{:keys [method]}]
                  (swap! calls conj method)
                  (if (= method :get)
                    {:status 200
                     :headers {"content-type"
                               "application/x-git-upload-pack-advertisement"}
                     :body (pkt/encode ["version 2\n" "ls-refs\n" :flush])}
                    {:status 200
                     :headers {"content-type" "application/x-git-upload-pack-result"}
                     :body (pkt/encode [(str oid " refs/heads/main\n") :flush])}))]
    (is (= [{:oid oid :ref "refs/heads/main" :unborn? false :attributes {}}]
           (http/ls-remote! send-fn "https://example.test/repo.git"
                            {:prefixes ["refs/heads/"]})))
    (is (= [:get :post] @calls))))
