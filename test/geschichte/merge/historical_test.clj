(ns geschichte.merge.historical-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.budget :as budget]
            [geschichte.bytes :as bytes]
            [geschichte.content :as content]
            [geschichte.merge :as merge]
            [geschichte.repo :as repo]
            [geschichte.workspace :as workspace]))

(defn- commit! [conn text]
  (repo/write! conn "notes.md" (bytes/utf8 text))
  (repo/stage-all! conn)
  (:geschichte.commit/id (repo/commit! conn {:message "Historical merge test" :author "Test"})))

(defn- operation [] (budget/create (merge/recommended-budget-policy)))

(deftest repeated-merge-after-publication-and-advance-uses-the-historical-base
  (doseq [bounded? [false true]]
    (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
               :keep-history? true :commit-graph? true}]
      (d/create-database cfg)
      (let [canonical (d/connect cfg)]
        (try
          (repo/init! canonical)
          (let [base (commit! canonical "local\n\nupstream\n")]
            (workspace/fork! canonical "worker")
            (workspace/fork! canonical "observer")
            (let [worker (d/connect (assoc cfg :branch :geschichte.workspace/worker))
                  observer (d/connect (assoc cfg :branch :geschichte.workspace/observer))]
              (try
                (repo/create-ref! worker "refs/heads/local" (repo/commit-by-id worker base))
                (let [incoming (commit! worker "local\n\nupstream one\n")
                      old-content (get-in (repo/tree-at worker incoming) ["notes.md" :content])]
                  (repo/create-ref! worker "refs/heads/incoming" (repo/commit-by-id worker incoming))
                  (repo/checkout! worker "refs/heads/local")
                  (let [local (commit! worker "local edit\n\nupstream\n")
                        first-plan (merge/plan worker local incoming)]
                    (is (:clean? first-plan))
                    (repo/prepare-merge! worker first-plan)
                    (let [merged (:geschichte.commit/id (repo/commit! worker {:message "First merge"}))]
                      (workspace/publish! canonical worker {:ref "main" :commit merged})
                      (workspace/advance! canonical observer {:expected-checkpoint (d/commit-id @observer)})
                      (is (nil? (d/q '[:find ?e . :in $ ?id :where [?e :geschichte.content/id ?id]] @observer old-content)))
                      (repo/checkout! worker "refs/heads/incoming")
                      (let [next-incoming (commit! worker "local\n\nupstream two\n")]
                        (workspace/publish! observer worker {:ref "incoming" :commit next-incoming :create? true})
                        (let [plan (merge/plan observer merged next-incoming
                                               (when bounded? {:merge-budget (operation)}))]
                          (is (:clean? plan))
                          (repo/prepare-merge! observer plan)
                          (repo/commit! observer {:message "Second merge"})
                          (is (= "local edit\n\nupstream two\n"
                                 (bytes/decode-utf8-strict (repo/read observer "notes.md")))))))))
                (finally (d/release observer) (d/release worker)))))
          (finally (d/release canonical) (d/delete-database cfg)))))))

(deftest selected-historical-representations-are-restored-before-materialization
  (doseq [kind [:line-delta :chunks] bounded? [false true]]
    (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
               :keep-history? true :commit-graph? true}]
      (d/create-database cfg)
      (let [conn (d/connect cfg)]
        (try
          (repo/init! conn)
          (let [base (commit! conn "base\n")
                _ (repo/create-ref! conn "refs/heads/side" (repo/commit-by-id conn base))
                ours (commit! conn "our edit\n")
                original (apply str (map #(str "unchanged line " % " padding\n") (range 300)))
                changed (.replace original "line 150" "changed line 150")]
            (repo/checkout! conn "refs/heads/side")
            (let [base-content (:content (repo/write! conn "archive.txt" (bytes/utf8 original)))
                  selected (:content (repo/write! conn "archive.txt" (bytes/utf8 changed)
                                                  (when (= kind :chunks) {:chunk-threshold 32 :chunk-min-size 64 :chunk-size 128 :chunk-max-size 256})))
                  _ (is (= kind (:geschichte.content/kind (content/info conn selected))))
                  _ (repo/stage-all! conn)
                  theirs (:geschichte.commit/id (repo/commit! conn {:message "Historical archive"}))]
              (repo/checkout! conn "refs/heads/main")
              ;; The current workspace need not catalog historical payload
              ;; representations. Their original immutable snapshot still does.
              (d/transact conn [[:db/retractEntity [:geschichte.content/id selected]]
                                [:db/retractEntity [:geschichte.content/id base-content]]])
              (let [before {:head (repo/head-commit conn) :work (repo/worktree conn) :status (repo/status conn)}
                    _ (when bounded?
                        (is (= :merge-output-bytes
                               (try (merge/plan conn ours theirs
                                                {:merge-budget (budget/create (assoc-in (merge/recommended-budget-policy) [:limits :merge-output-bytes] 0))})
                                    nil (catch clojure.lang.ExceptionInfo error (:metric (ex-data error)))))))
                    _ (is (nil? (d/q '[:find ?e . :in $ ?id :where [?e :geschichte.content/id ?id]] @conn selected)))
                    plan (merge/plan conn ours theirs (when bounded? {:merge-budget (operation)}))]
                (is (:clean? plan))
                (is (= before {:head (repo/head-commit conn) :work (repo/worktree conn) :status (repo/status conn)}))
                (is (= selected (get-in plan [:tree "archive.txt" :content])))
                (repo/prepare-merge! conn plan)
                (repo/commit! conn {:message "Materialized historical result"})
                (is (= changed (bytes/decode-utf8-strict (repo/read conn "archive.txt")))))))
          (finally (d/release conn) (d/delete-database cfg)))))))
