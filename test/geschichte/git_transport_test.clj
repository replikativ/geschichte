(ns geschichte.git-transport-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [geschichte.git.client :as client]
            [geschichte.git.http :as http]
            [geschichte.git.pack :as pack]
            [geschichte.git.project :as project]
            [geschichte.git.ssh :as ssh]
            [geschichte.git.store :as store]
            [geschichte.git.transport :as transport]
            [geschichte.repo :as repo]))

(defn- with-repository [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? true :commit-graph? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (repo/init! conn)
        (f conn)
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(deftest successful-clone-retains-remote-and-correct-upstream
  (with-repository
    (fn [source]
      (repo/write! source "README.md" (.getBytes "remote\n" "UTF-8"))
      (repo/stage-all! source)
      (let [commit (repo/commit! source {:message "base"})
            graph (project/project source (:geschichte.commit/id commit))
            packed (pack/encode (:objects graph))
            refs [{:ref "HEAD" :oid (:oid graph)
                   :attributes {:symref-target "refs/heads/main"}}
                  {:ref "refs/heads/main" :oid (:oid graph) :attributes {}}]
            clone! (fn [conn & args]
                     (let [options (last args)]
                       (store/import-pack! conn packed {:remote "upstream" :refs refs})
                       (if (:no-checkout? options)
                         {:refs refs :checkout/skipped? true}
                         (client/checkout-fetched! conn {:refs refs} options))))]
        (doseq [[url transport-var] [["https://example.test/repo.git" #'http/clone!]
                                     ["git@example.test:repo.git" #'ssh/clone!]]
                [options local] [[{} "main"]
                                 [{:branch "main" :ref "refs/heads/local"} "local"]
                                 [{:ref "refs/tags/pinned"} nil]
                                 [{:no-checkout? true} nil]]]
          (with-repository
            (fn [conn]
              (with-redefs-fn {transport-var clone!}
                #(transport/clone! {:conn conn :remote "upstream" :url url :options options}))
              (let [config (repo/configuration conn)]
                (is (= url (config "remote.upstream.url")))
                (is (= "+refs/heads/*:refs/remotes/upstream/*" (config "remote.upstream.fetch")))
                (if local
                  (do (is (= "upstream" (config (str "branch." local ".remote"))))
                      (is (= "refs/heads/main" (config (str "branch." local ".merge")))))
                  (is (not-any? #(.startsWith ^String % "branch.") (keys config)))))
              (testing "a failed transport never replaces the saved connection"
                (let [before (repo/configuration conn)]
                  (with-redefs-fn {transport-var (fn [& _] (throw (ex-info "offline" {})))}
                    #(is (thrown-with-msg? clojure.lang.ExceptionInfo #"offline"
                                           (transport/clone! {:conn conn :remote "upstream" :url url}))))
                  (is (= before (repo/configuration conn))))))))))))
