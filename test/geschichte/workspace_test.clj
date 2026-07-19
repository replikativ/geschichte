(ns geschichte.workspace-test
  (:refer-clojure :exclude [read])
  (:require [clojure.core.async :as async]
            [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.versioning :as dv]
            [geschichte.query :as query]
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
    (let [canonical (d/connect cfg)]
      (repo/init! canonical)
      (repo/write! canonical "base.txt" (->bytes "base\n"))
      (repo/stage-all! canonical)
      (repo/commit! canonical {:message "base"})
      {:cfg cfg :canonical canonical})))

(defn- cleanup [{:keys [cfg canonical]}]
  (d/release canonical)
  (d/delete-database cfg))

(deftest isolated-workspaces-share-the-fork-snapshot
  (let [{:keys [cfg canonical] :as f} (fixture)]
    (try
      (let [fork-id (d/commit-id @canonical)]
        (workspace/fork! canonical "agent-a")
        (workspace/fork! canonical "agent-b")
        (is (= #{:geschichte.workspace/agent-a
                 :geschichte.workspace/agent-b}
               (workspace/list canonical)))
        (let [a (d/connect (assoc cfg :branch :geschichte.workspace/agent-a))
              b (d/connect (assoc cfg :branch :geschichte.workspace/agent-b))]
          (try
            (is (= fork-id (d/commit-id @a) (d/commit-id @b)))
            (is (= "refs/heads/main" (repo/current-ref a)
                   (repo/current-ref b)))
            (repo/write! a "only-a.txt" (->bytes "a\n"))
            (is (nil? (repo/read b "only-a.txt")))
            (is (nil? (repo/read canonical "only-a.txt")))
            (finally
              (d/release a)
              (d/release b))))
        (workspace/remove! canonical "agent-a")
        (workspace/remove! canonical "agent-b")
        (is (empty? (workspace/list canonical))))
      (finally (cleanup f)))))

(deftest publication-is-selective-atomic-and-gc-reachable
  (let [{:keys [cfg canonical] :as f} (fixture)]
    (try
      (workspace/fork! canonical "publisher")
      (let [worker (d/connect (assoc cfg :branch
                                     :geschichte.workspace/publisher))]
        (try
          (repo/set-config! worker "workspace.private" "yes")
          (repo/branch! worker "private-ref")
          (repo/write! worker "published.txt" (->bytes "published\n"))
          (repo/stage-all! worker)
          (let [commit (repo/commit! worker {:message "publish me"})
                commit-id (:geschichte.commit/id commit)
                snapshot (:geschichte.commit/snapshot commit)]
            ;; Mutable state after the commit must not hitchhike on publication.
            (repo/write! worker "uncommitted.txt" (->bytes "private\n"))
            (let [result (workspace/publish! canonical worker)]
              (is (= commit-id (:new result)))
              (is (= commit-id (get (repo/refs canonical)
                                    "refs/heads/main")))
              (is (= "published\n"
                     (text (repo/read-at canonical commit-id "published.txt"))))
              (is (nil? (get (repo/refs canonical)
                             "refs/heads/private-ref")))
              (is (nil? (get (repo/configuration canonical)
                             "workspace.private")))
              (is (nil? (get (query/worktree @canonical)
                             "uncommitted.txt"))))
            (workspace/remove! canonical "publisher")
            (async/<!! (d/gc-storage canonical))
            (is (some? (dv/commit-as-db canonical snapshot))))
          (finally (d/release worker))))
      (finally (cleanup f)))))

(deftest concurrent-workspace-publication-requires-fast-forward
  (let [{:keys [cfg canonical] :as f} (fixture)]
    (try
      (workspace/fork! canonical "first")
      (workspace/fork! canonical "second")
      (let [first-worker (d/connect (assoc cfg :branch
                                           :geschichte.workspace/first))
            second-worker (d/connect (assoc cfg :branch
                                            :geschichte.workspace/second))]
        (try
          (doseq [[worker path] [[first-worker "first.txt"]
                                 [second-worker "second.txt"]]]
            (repo/write! worker path (->bytes path))
            (repo/stage-all! worker)
            (repo/commit! worker {:message path}))
          (workspace/publish! canonical first-worker)
          (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                #"not a fast-forward"
                                (workspace/publish! canonical second-worker)))
          (let [forced (workspace/publish! canonical second-worker
                                           {:force? true})]
            (is (:forced? forced))
            (is (= (:new forced)
                   (get (repo/refs canonical) "refs/heads/main"))))
          (finally
            (d/release first-worker)
            (d/release second-worker))))
      (finally (cleanup f)))))

(deftest clean-workspace-can-advance-to-a-published-tip
  (let [{:keys [cfg canonical] :as f} (fixture)]
    (try
      (workspace/fork! canonical "producer")
      (workspace/fork! canonical "observer")
      (let [producer (d/connect (assoc cfg :branch
                                       :geschichte.workspace/producer))
            observer (d/connect (assoc cfg :branch
                                       :geschichte.workspace/observer))]
        (try
          (repo/write! producer "new.txt" (->bytes "new\n"))
          (repo/stage-all! producer)
          (repo/write! observer "scratch.tmp" (->bytes "untracked\n"))
          (let [commit (repo/commit! producer {:message "new"})]
            (workspace/publish! canonical producer)
            (let [advanced (workspace/advance! canonical observer)]
              (is (= (:geschichte.commit/id commit) (:new advanced)))
              (is (= "new\n" (text (repo/read observer "new.txt"))))
              (is (= "untracked\n" (text (repo/read observer "scratch.tmp"))))
              (is (= ["scratch.tmp"] (:untracked (repo/status observer))))))
          (finally
            (d/release producer)
            (d/release observer))))
      (finally (cleanup f)))))
