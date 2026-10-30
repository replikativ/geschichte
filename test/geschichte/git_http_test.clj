(ns geschichte.git-http-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.git.http :as http]
            [geschichte.git.transport :as transport]
            [geschichte.git.client :as client]
            [geschichte.git.project :as project]
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
        ;; The default transport invokes HTTP's options-bearing convenience
        ;; arity. Previously that arity was missing and real fetches failed.
        (reset! calls [])
        (let [result (with-redefs [http/send-http send-fn]
                       (transport/fetch! {:conn conn :remote "origin" :url "https://example.test/repo.git"
                                          :options {:haves [oid] :tags :none}}))]
          (is (= oid (get-in result [:refs 0 :oid])))
          (is (= 3 (count @calls)))
          (is (some #(= (str "have " oid "\n") (when (bytes? %) (pkt/text %)))
                    (pkt/decode (:body (last @calls))))))
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

(deftest incremental-fetch-transfers-only-the-new-objects
  (let [configs (repeatedly 2 #(hash-map :store {:backend :memory :id (random-uuid)}
                                         :schema-flexibility :write :keep-history? true
                                         :commit-graph? true))]
    (doseq [cfg configs] (d/create-database cfg))
    (let [[source target :as conns] (mapv d/connect configs)]
      (try
        (doseq [conn conns] (repo/init! conn))
        (repo/write! source "README.md" (object/utf8 "one\n"))
        (repo/stage-all! source)
        (let [base (repo/commit! source {:message "base"})
              base-graph (project/project source (:geschichte.commit/id base))
              base-oid (:oid base-graph)
              refs (fn [oid] [{:ref "HEAD" :oid oid
                               :attributes {:symref-target "refs/heads/main"}}])]
          (client/ingest-fetch! target
                                (pkt/encode ["packfile\n"
                                             (object/concat-bytes (byte-array [1]) (pack/encode (:objects base-graph)))
                                             :flush]))
          (client/checkout-fetched! target {:refs (refs base-oid)})
          (repo/write! source "notes.md" (object/utf8 "two\n"))
          (repo/stage-all! source)
          (let [next-commit (repo/commit! source {:message "next"})
                graph (project/project source (:geschichte.commit/id next-commit))
                oid (:oid graph)
                new-objects (apply dissoc (:objects graph) (keys (:objects base-graph)))
                calls (atom [])
                send-fn (fn [{:keys [method body] :as request}]
                          (swap! calls conj request)
                          (if (= :get method)
                            {:status 200 :body (pkt/encode ["version 2\n" "ls-refs\n" "fetch\n" :flush])}
                            (if (= "command=ls-refs\n" (pkt/text (first (pkt/decode body))))
                              {:status 200 :body (pkt/encode [(str oid " HEAD symref-target:refs/heads/main\n") :flush])}
                              {:status 200 :body (pkt/encode ["packfile\n"
                                                              (object/concat-bytes (byte-array [1]) (pack/encode new-objects))
                                                              :flush])})))
                result (http/fetch! send-fn target "origin" "https://example.test/repo.git"
                                    {:haves [base-oid]})
                request-lines (mapv #(if (bytes? %) (pkt/text %) %) (pkt/decode (:body (last @calls))))]
            (is (some #{(str "have " base-oid "\n")} request-lines))
            (is (= (count new-objects) (:persisted result)))
            (is (< (count new-objects) (count (:objects graph))))
            (is (= :fast-forward (:pull/status (client/apply-pull! target "origin" result))))
            (is (= "one\n" (String. ^bytes (repo/read target "README.md") "UTF-8")))
            (is (= "two\n" (String. ^bytes (repo/read target "notes.md") "UTF-8")))))
        (finally
          (doseq [conn conns] (d/release conn))
          (doseq [cfg configs] (d/delete-database cfg)))))))
