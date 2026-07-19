(ns geschichte.query-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.query :as query]
            [geschichte.repo :as repo]))

(deftest portable-query-views-on-jvm
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (repo/init! conn)
        (is (= {} (query/git-refs @conn)))
        (repo/set-config! conn "remote.origin.url" "https://example.test/repo.git")
        (is (= {"remote.origin.url" "https://example.test/repo.git"}
               (query/configuration @conn)))
        (repo/write! conn "README.md" (.getBytes "hello\n" "UTF-8"))
        (repo/stage-all! conn)
        (let [commit (repo/commit! conn {:message "initial" :author "Ada"})]
          (is (query/initialized? @conn))
          (is (= "refs/heads/main" (query/current-ref @conn)))
          (is (= (:geschichte.commit/id commit)
                 (get (query/refs @conn) "refs/heads/main")))
          (is (= "initial"
                 (:geschichte.commit/message
                  (query/commit @conn (:geschichte.commit/id commit)))))
          (is (= #{"README.md"} (set (keys (query/worktree @conn))))))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))
