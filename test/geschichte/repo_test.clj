(ns geschichte.repo-test
  (:refer-clojure :exclude [read remove])
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [geschichte.repo :as repo]))

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
