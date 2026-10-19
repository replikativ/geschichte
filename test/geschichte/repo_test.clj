(ns geschichte.repo-test
  (:refer-clojure :exclude [read remove])
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [geschichte.repo :as repo]
            [geschichte.workspace :as workspace]))

(defn- ->bytes [s] (.getBytes ^String s "UTF-8"))
(defn- text [bs] (when bs (String. ^bytes bs "UTF-8")))

(defn- fixture []
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write
             :keep-history? true
             :commit-graph? true}]
    (d/create-database cfg)
    {:cfg cfg :conn (d/connect cfg)}))

(defn- cleanup [{:keys [cfg conn]}]
  (d/release conn)
  (d/delete-database cfg))

(deftest init-write-stage-commit-status
  (let [{:keys [conn] :as f} (fixture)]
    (try
      (repo/init! conn {:name "demo"})
      (is (= "refs/heads/main" (repo/current-ref conn)))
      (is (= {:branch "refs/heads/main" :head nil :staged [] :unstaged []
              :untracked [] :clean? true}
             (repo/status conn)))

      (repo/write! conn "src/a.txt" (->bytes "one\n"))
      (is (= ["src/a.txt"] (:untracked (repo/status conn))))
      (is (= [{:path "src/a.txt" :worktree :untracked}]
             (repo/status-entries conn)))
      (repo/stage! conn ["src/a.txt"])
      (is (= ["src/a.txt"] (:staged (repo/status conn))))
      (is (= :added (:kind (first (repo/changes conn :head :index)))))
      (is (= "one\n"
             (text (repo/read-entry
                    conn (:after (first (repo/changes conn :head :index)))))))
      (let [c1 (repo/commit! conn {:message "initial" :author "Ada"})]
        (is (= (:geschichte.commit/id c1) (:head (repo/status conn))))
        (is (:clean? (repo/status conn)))
        (is (= "one\n" (text (repo/read-at conn
                                           (:geschichte.commit/id c1)
                                           "src/a.txt"))))
        (is (= ["initial"] (mapv :geschichte.commit/message (repo/log conn)))))

      (repo/write! conn "src/a.txt" (->bytes "two\n"))
      (is (= ["src/a.txt"] (:unstaged (repo/status conn))))
      (is (= [{:path "src/a.txt" :worktree :modified}]
             (repo/status-entries conn)))
      (repo/stage-all! conn)
      (is (= ["src/a.txt"] (:staged (repo/status conn))))
      (repo/commit! conn {:message "second" :author "Ada"})
      (is (= ["second" "initial"]
             (mapv :geschichte.commit/message (repo/log conn))))
      (finally (cleanup f)))))

(deftest prepared-commit-publishes-with-an-application-receipt
  (let [{:keys [conn] :as f} (fixture)]
    (try
      (repo/init! conn)
      (d/transact conn [{:db/ident :test/publication :db/valueType :db.type/uuid
                         :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
                        {:db/ident :test/commit :db/valueType :db.type/uuid
                         :db/cardinality :db.cardinality/one}])
      (repo/write! conn "wiki/brief.md" (->bytes "# Brief"))
      (repo/stage! conn ["wiki/brief.md"])
      (repo/write! conn "private-draft.txt" (->bytes "Unstaged work"))
      (let [before (d/commit-id @conn)
            plan (repo/prepare-commit conn {:message "Publish brief" :author "Ada"})
            operation (random-uuid)
            commit-id (get-in plan [:commit :geschichte.commit/id])
            publish (fn [db plan operation]
                      ;; Dedup first: a lost acknowledgement can be recovered
                      ;; even when the DB has moved since publication.
                      (if (d/entity db [:test/publication operation])
                        []
                        (conj (repo/prepared-commit-tx db plan)
                              {:test/publication operation :test/commit commit-id})))]
        (is (= before (d/commit-id @conn)) "Preparation is read-only")
        (is (false? (:noop? plan)))
        (d/transact conn [[:db.fn/call publish plan operation]])
        (is (= commit-id (:geschichte.commit/id (repo/head-commit conn))))
        (is (= commit-id (:test/commit (d/entity @conn [:test/publication operation]))))
        (is (= "# Brief" (text (repo/read-at conn commit-id "wiki/brief.md"))))
        (is (= "Unstaged work" (text (repo/read conn "private-draft.txt"))))
        (is (nil? (repo/read-at conn commit-id "private-draft.txt")))
        (repo/set-config! conn "later" "change")
        (d/transact conn [[:db.fn/call publish plan operation]])
        (is (= commit-id (:geschichte.commit/id (repo/head-commit conn)))))
      (finally (cleanup f)))))

(deftest prepared-commit-refuses-any-intervening-transaction
  (let [{:keys [conn] :as f} (fixture)]
    (try
      (repo/init! conn)
      (repo/write! conn "a.md" (->bytes "one"))
      (repo/stage-all! conn)
      (let [plan (repo/prepare-commit conn {:message "One"})]
        (repo/set-config! conn "unrelated" "still invalidates")
        (is (thrown? Exception
                     (d/transact conn [[:db.fn/call repo/prepared-commit-tx plan]])))
        (is (nil? (repo/head-commit conn))))
      (finally (cleanup f)))))

(deftest prepared-publication-is-bound-to-its-physical-workspace
  (let [{:keys [conn cfg] :as f} (fixture)]
    (try
      (repo/init! conn)
      (repo/write! conn "a.md" (->bytes "one"))
      (repo/stage-all! conn)
      (let [plan (repo/prepare-commit conn {:message "One"})]
        (workspace/fork! conn "other")
        (let [other (d/connect (assoc cfg :branch :geschichte.workspace/other))]
          (try
            (is (= (d/commit-id @conn) (d/commit-id @other)))
            (is (thrown? Exception (repo/prepared-commit-tx @other plan)))
            (is (seq (repo/prepared-commit-tx @conn plan)))
            (finally (d/release other)))))
      (finally (cleanup f)))))

(deftest prepared-publication-with-existing-head-and-merge-refusal
  (let [{:keys [conn] :as f} (fixture)]
    (try
      (repo/init! conn)
      (repo/write! conn "a.md" (->bytes "one"))
      (repo/stage-all! conn)
      (let [base (repo/commit! conn {:message "Base"})
            noop (repo/prepare-commit conn {:message "Unchanged"})]
        (is (:noop? noop))
        (is (= [] (repo/prepared-commit-tx @conn noop)))
        (repo/write! conn "a.md" (->bytes "two"))
        (repo/stage-all! conn)
        (let [plan (repo/prepare-commit conn {:message "Second"})]
          (is (= [[:geschichte.commit/id (:geschichte.commit/id base)]]
                 (get-in plan [:commit :geschichte.commit/parents])))
          (d/transact conn [[:db.fn/call repo/prepared-commit-tx plan]])
          (is (= "two" (text (repo/read-at conn (get-in plan [:commit :geschichte.commit/id]) "a.md")))))
        (d/transact conn [[:db/add [:geschichte.repo/id (:geschichte.repo/id
                                                         (d/q '[:find (pull ?r [*]) . :where [?r :geschichte.repo/id]] @conn))]
                           :geschichte.repo/merge-parent [:geschichte.commit/id (:geschichte.commit/id base)]]])
        (is (thrown? Exception (repo/prepare-commit conn {:message "Not a merge"}))))
      (finally (cleanup f)))))

(deftest prepared-noop-validates-without-creating-a-logical-commit
  (let [{:keys [conn] :as f} (fixture)]
    (try
      (repo/init! conn)
      (let [plan (repo/prepare-commit conn {:message "No changes"})]
        (is (:noop? plan))
        (is (nil? (:commit plan)))
        (is (= [] (repo/prepared-commit-tx @conn plan)))
        (repo/set-config! conn "unrelated" "change")
        (is (thrown? Exception (repo/prepared-commit-tx @conn plan))))
      (is (nil? (repo/head-commit conn)))
      (is (thrown? Exception (repo/prepare-commit conn {:message "Amend" :amend? true})))
      (finally (cleanup f)))))

(deftest branches-checkout-and-deletion
  (let [{:keys [conn] :as f} (fixture)]
    (try
      (repo/init! conn)
      (repo/write! conn "a" (->bytes "base"))
      (repo/stage-all! conn)
      (repo/commit! conn {:message "base"})
      (repo/branch! conn "feature")

      (repo/checkout! conn "feature")
      (repo/write! conn "a" (->bytes "feature"))
      (repo/write! conn "b" (->bytes "new"))
      (repo/stage-all! conn)
      (repo/commit! conn {:message "feature"})
      (is (= "feature" (text (repo/read conn "a"))))

      (repo/checkout! conn "main")
      (is (= "base" (text (repo/read conn "a"))))
      (is (nil? (repo/read conn "b")))

      (repo/remove! conn "a")
      (repo/stage-all! conn)
      (is (= ["a"] (:staged (repo/status conn))))
      (repo/commit! conn {:message "delete a"})
      (is (:clean? (repo/status conn)))
      (is (nil? (repo/read conn "a")))
      (finally (cleanup f)))))

(deftest checkout-preserves-non-conflicting-untracked-files
  (let [{:keys [conn] :as f} (fixture)]
    (try
      (repo/init! conn)
      (repo/write! conn "tracked.txt" (->bytes "main"))
      (repo/stage-all! conn)
      (repo/commit! conn {:message "main"})
      (repo/branch! conn "feature")
      (repo/write! conn "build/output.log" (->bytes "untracked"))
      (repo/checkout! conn "feature")
      (is (= "untracked" (text (repo/read conn "build/output.log"))))
      (is (= "refs/heads/feature" (repo/current-ref conn)))
      (finally (cleanup f)))))

(deftest restaging-deleted-paths-is-idempotent-before-and-after-commit
  (let [{:keys [conn] :as f} (fixture)]
    (try
      (repo/init! conn)
      (repo/write! conn "removed.txt" (->bytes "original"))
      (repo/stage-all! conn)
      (repo/commit! conn {:message "base"})
      (repo/remove! conn "removed.txt")
      (repo/stage! conn ["removed.txt"])
      (is (= ["removed.txt"] (:staged (repo/status conn))))
      (repo/stage! conn ["removed.txt"])
      (repo/stage-all! conn)
      (is (= ["removed.txt"] (:staged (repo/status conn))))
      (is (= :deleted (:index (first (repo/status-entries conn)))))
      (repo/commit! conn {:message "delete"})
      (is (:clean? (repo/status conn)))
      ;; The index retains the deletion marker after commit. Neither named
      ;; staging nor the next `git add .` may retract [:content/id nil].
      (repo/stage! conn ["removed.txt" "never-existed.txt"])
      (repo/stage-all! conn)
      (is (:clean? (repo/status conn)))
      (is (nil? (repo/read conn "removed.txt")))
      (repo/write! conn "new.txt" (->bytes "new"))
      (repo/stage-all! conn)
      (is (= ["new.txt"] (:staged (repo/status conn))))
      (repo/commit! conn {:message "next"})
      (is (:clean? (repo/status conn)))
      (is (= #{"new.txt"} (set (keys (repo/tree conn :head)))))
      (repo/write! conn "removed.txt" (->bytes "restored"))
      (repo/stage-all! conn)
      (repo/commit! conn {:message "restore"})
      (is (= "restored" (text (repo/read conn "removed.txt"))))
      (is (:clean? (repo/status conn)))
      (finally (cleanup f)))))

(deftest restore-reset-and-delete-branch
  (let [{:keys [conn] :as f} (fixture)]
    (try
      (repo/init! conn)
      (repo/write! conn "tracked.txt" (->bytes "one"))
      (repo/stage-all! conn)
      (let [first-commit (repo/commit! conn {:message "one"})]
        (repo/write! conn "tracked.txt" (->bytes "work"))
        (repo/restore-paths! conn ["tracked.txt"])
        (is (= "one" (text (repo/read conn "tracked.txt"))))

        (repo/write! conn "tracked.txt" (->bytes "two"))
        (repo/stage-all! conn)
        (repo/restore-paths! conn ["tracked.txt"] {:staged? true
                                                   :worktree? false})
        (is (= [] (:staged (repo/status conn))))
        (is (= ["tracked.txt"] (:unstaged (repo/status conn))))
        (repo/stage-all! conn)
        (let [second-commit (repo/commit! conn {:message "two"})]
          (repo/branch! conn "discard")
          (repo/write! conn "untracked.log" (->bytes "keep"))
          (repo/reset! conn first-commit {:mode :soft})
          (is (= (:geschichte.commit/id first-commit)
                 (:head (repo/status conn))))
          (is (= ["tracked.txt"] (:staged (repo/status conn))))
          (repo/reset! conn second-commit {:mode :hard})
          (is (= "two" (text (repo/read conn "tracked.txt"))))
          (is (= "keep" (text (repo/read conn "untracked.log"))))
          (is (= [] (:staged (repo/status conn))))
          (is (= [] (:unstaged (repo/status conn))))
          (is (= ["untracked.log"] (:untracked (repo/status conn))))
          (repo/delete-branch! conn "discard")
          (is (not (contains? (repo/refs conn) "refs/heads/discard")))))
      (finally (cleanup f)))))

(deftest datahike-workspace-branch-is-separate
  (let [{:keys [conn cfg] :as f} (fixture)]
    (try
      (repo/init! conn)
      (repo/fork-workspace! conn :agent-one)
      (is (contains? (d/branches conn) :agent-one))
      (is (= #{"refs/heads/main"} (set (keys (repo/refs conn)))))
      (let [fork (d/connect (assoc cfg :branch :agent-one))]
        (try
          (is (= "refs/heads/main" (repo/current-ref fork)))
          (repo/branch! fork "logical")
          (is (contains? (set (keys (repo/refs fork))) "refs/heads/logical"))
          (is (not (contains? (set (keys (repo/refs conn))) "refs/heads/logical")))
          (finally (d/release fork))))
      (finally (cleanup f)))))
