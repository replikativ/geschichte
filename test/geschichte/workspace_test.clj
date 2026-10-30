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

(deftest competing-ref-creation-preserves-the-winning-publication
  (let [{:keys [cfg canonical] :as f} (fixture)]
    (try
      (workspace/fork! canonical "first")
      (workspace/fork! canonical "second")
      (let [a (d/connect (assoc cfg :branch :geschichte.workspace/first))
            b (d/connect (assoc cfg :branch :geschichte.workspace/second))
            merge-var (ns-resolve 'geschichte.workspace 'merge-data)
            merge-data @merge-var
            injected? (atom false)
            winner (atom nil)
            after-winner (atom nil)]
        (try
          (doseq [[conn file] [[a "a.txt"] [b "b.txt"]]]
            (repo/write! conn file (->bytes file))
            (repo/stage-all! conn)
            (repo/commit! conn {:message file})
            (repo/branch! conn "topic"))
          (let [failure
                (with-redefs-fn
                  {merge-var (fn [conn parents tx]
                               ;; The outer call has already observed absence.
                               ;; Publish its competitor before it reaches the writer.
                               (when (compare-and-set! injected? false true)
                                 (reset! winner (workspace/publish! canonical b {:ref "topic" :create? true}))
                                 (reset! after-winner @canonical))
                               (merge-data conn parents tx))}
                  #(try (workspace/publish! canonical a {:ref "topic" :create? true})
                        nil (catch Exception error error)))]
            (is (= ::workspace/publication-ref-exists (:type (ex-data failure))))
            (is (nil? (:old @winner)))
            (is (false? (:forced? @winner)))
            (is (= (:new @winner) (get (repo/refs canonical) "refs/heads/topic")))
            (is (= @after-winner @canonical) "Losing transfer and ref changes are rolled back"))
          (finally (d/release a) (d/release b))))
      (finally (cleanup f)))))

(deftest publication-distinguishes-missing-and-unborn-refs
  (let [{:keys [cfg canonical] :as f} (fixture)]
    (try
      (workspace/fork! canonical "publisher")
      (let [worker (d/connect (assoc cfg :branch :geschichte.workspace/publisher))]
        (try
          (repo/branch! worker "topic")
          (is (thrown? Exception (workspace/publish! canonical worker {:ref "topic"})))
          (d/transact canonical [{:geschichte.ref/name "refs/heads/topic"}])
          (let [result (workspace/publish! canonical worker {:ref "topic"})]
            (is (nil? (:old result)))
            (is (= (:new result) (get (repo/refs canonical) "refs/heads/topic"))))
          (finally (d/release worker))))
      (finally (cleanup f)))))

(deftest ref-preparation-uses-one-canonical-snapshot
  (doseq [interleaving [:delete-existing :delete-unborn :recreate-existing :rename-existing :create-unborn]]
    (let [{:keys [cfg canonical] :as f} (fixture)]
      (try
        (workspace/fork! canonical "publisher")
        (let [worker (d/connect (assoc cfg :branch :geschichte.workspace/publisher))
              after-change (atom nil)
              tree-at repo/tree-at]
          (try
            (repo/branch! worker "topic")
            (when (#{:delete-existing :recreate-existing :rename-existing} interleaving)
              (repo/branch! canonical "topic"))
            (when (= :delete-unborn interleaving)
              (d/transact canonical [{:geschichte.ref/name "refs/heads/topic"}]))
            (let [failure
                  (with-redefs [repo/tree-at
                                (fn [conn commit]
                                  (cond
                                    (= :create-unborn interleaving)
                                    (d/transact canonical [{:geschichte.ref/name "refs/heads/topic"}])

                                    (= :rename-existing interleaving)
                                    (let [eid (d/q '[:find ?e . :where [?e :geschichte.ref/name "refs/heads/topic"]] @canonical)]
                                      (d/transact canonical [[:db/add eid :geschichte.ref/name "refs/heads/renamed"]]))

                                    :else (do
                                            (repo/delete-branch! canonical "topic" {:force? true})
                                            (when (= :recreate-existing interleaving)
                                              (repo/branch! canonical "topic"))))
                                  (reset! after-change @canonical)
                                  (tree-at conn commit))]
                    (try (workspace/publish! canonical worker
                                             {:ref "topic" :create? (= :create-unborn interleaving)})
                         nil (catch Exception error error)))]
              (is (= (if (= :create-unborn interleaving)
                       ::workspace/publication-ref-exists ::workspace/stale-publication-ref)
                     (:type (ex-data failure))))
              (is (= @after-change @canonical) "Publication must preserve intervening ref lifecycle changes"))
            (finally (d/release worker))))
        (finally (cleanup f))))))

(deftest explicit-force-publishes-to-the-current-logical-name
  (let [{:keys [cfg canonical] :as f} (fixture)]
    (try
      (repo/branch! canonical "topic")
      (workspace/fork! canonical "publisher")
      (let [worker (d/connect (assoc cfg :branch :geschichte.workspace/publisher))
            tree-at repo/tree-at]
        (try
          (let [result (with-redefs [repo/tree-at
                                     (fn [conn commit]
                                       (repo/delete-branch! canonical "topic" {:force? true})
                                       (tree-at conn commit))]
                         (workspace/publish! canonical worker {:ref "topic" :force? true}))]
            (is (:forced? result))
            (is (= (:new result) (get (repo/refs canonical) "refs/heads/topic"))))
          (finally (d/release worker))))
      (finally (cleanup f)))))

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

(deftest advancing-requires-the-current-ref-and-no-pending-merge
  (let [{:keys [cfg canonical] :as f} (fixture)]
    (try
      (repo/branch! canonical "other")
      (workspace/fork! canonical "observer")
      (let [observer (d/connect (assoc cfg :branch :geschichte.workspace/observer))]
        (try
          (let [before @observer
                error (try (workspace/advance! canonical observer {:ref "other"}) nil
                           (catch Exception e e))]
            (is (= :geschichte.workspace/noncurrent-ref (:type (ex-data error))))
            (is (= before @observer)))
          (let [head (get (repo/refs observer) "refs/heads/main")]
            (d/transact observer [[:db/add (:db/id (query/repository @observer))
                                   :geschichte.repo/merge-parent [:geschichte.commit/id head]]]))
          (let [before @observer
                error (try (workspace/advance! canonical observer) nil
                           (catch Exception e e))]
            (is (= :geschichte.workspace/pending-merge (:type (ex-data error))))
            (is (= before @observer)))
          (finally (d/release observer))))
      (finally (cleanup f)))))

(deftest advancing-uses-one-source-snapshot
  (let [{:keys [cfg canonical] :as f} (fixture)]
    (try
      (workspace/fork! canonical "observer")
      (let [observer (d/connect (assoc cfg :branch :geschichte.workspace/observer))]
        (try
          (repo/write! canonical "base.txt" (->bytes "remote\n"))
          (repo/stage-all! canonical)
          (let [tip (:geschichte.commit/id (repo/commit! canonical {:message "remote"}))
                checkpoint (d/commit-id @canonical)
                tree-at repo/tree-at
                result (with-redefs [repo/tree-at
                                     (fn [conn commit]
                                       (let [tree (tree-at conn commit)]
                                         (repo/write! canonical "base.txt" (->bytes "later\n"))
                                         (repo/stage-all! canonical)
                                         (repo/commit! canonical {:message "later"})
                                         tree))]
                         (workspace/advance! canonical observer))]
            (is (= tip (:new result)))
            (is (= checkpoint (:canonical-commit result)))
            (is (not= checkpoint (d/commit-id @canonical)))
            (is (= "remote\n" (text (repo/read observer "base.txt")))))
          (finally (d/release observer))))
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
            (let [advanced (workspace/advance! canonical observer
                                               {:expected-checkpoint (d/commit-id @observer)})]
              (is (= (:geschichte.commit/id commit) (:new advanced)))
              (is (= "new\n" (text (repo/read observer "new.txt"))))
              (is (= "untracked\n" (text (repo/read observer "scratch.tmp"))))
              (is (= ["scratch.tmp"] (:untracked (repo/status observer))))))
          (finally
            (d/release producer)
            (d/release observer))))
      (finally (cleanup f)))))

(deftest advance-refuses-a-stale-caller-checkpoint-before-preparation
  (doseq [change [:worktree :config :head]]
    (let [{:keys [cfg canonical] :as f} (fixture)]
      (try
        (workspace/fork! canonical "observer")
        (let [observer (d/connect (assoc cfg :branch :geschichte.workspace/observer))]
          (try
            (let [expected (d/commit-id @observer)
                  old-head (repo/refs observer)]
              (repo/write! canonical "incoming.txt" (->bytes "incoming\n"))
              (repo/stage-all! canonical)
              (repo/commit! canonical {:message "incoming"})
              ;; The host/human inspected expected. Another operation then
              ;; changes the destination before advance! captures its own DB.
              (case change
                :worktree (repo/write! observer "scratch.tmp" (->bytes "preserve\n"))
                :config (repo/set-config! observer "example.setting" "preserve")
                :head (do (repo/write! observer "local.txt" (->bytes "local\n"))
                          (repo/stage-all! observer)
                          (repo/commit! observer {:message "local"})))
              (let [before @observer
                    error (try (workspace/advance! canonical observer {:expected-checkpoint expected})
                               nil (catch Exception e e))]
                (is (= ::workspace/stale-advance (:type (ex-data error))))
                (is (= expected (:expected-checkpoint (ex-data error))))
                (is (= (d/commit-id before) (:actual-checkpoint (ex-data error))))
                (is (= before @observer))
                (when (not= :head change) (is (= old-head (repo/refs observer))))))
            (finally (d/release observer))))
        (finally (cleanup f))))))

(deftest invalid-advance-options-refuse-before-accessing-either-repository
  (doseq [options [{:expected-checkpoint nil} {:expected-checkpoint "not-a-checkpoint"}
                   {:expected-checkpoint 42} {:expected-checkpont (random-uuid)}]]
    (let [error (try (workspace/advance! nil nil options) nil (catch Exception e e))]
      (is (= ::workspace/invalid-advance-options (:type (ex-data error)))))))

(deftest advancing-refuses-an-intervening-worktree-edit
  (let [{:keys [cfg canonical] :as f} (fixture)]
    (try
      (workspace/fork! canonical "observer")
      (let [observer (d/connect (assoc cfg :branch :geschichte.workspace/observer))
            edited (atom nil)
            merge-var (ns-resolve 'geschichte.workspace 'merge-data)
            merge-data @merge-var]
        (try
          (let [old (repo/refs observer)]
            (repo/write! canonical "base.txt" (->bytes "remote\n"))
            (repo/stage-all! canonical)
            (repo/commit! canonical {:message "remote"})
            (let [error (with-redefs-fn
                          {merge-var (fn [conn parents tx]
                                       (repo/write! observer "base.txt" (->bytes "concurrent\n"))
                                       (reset! edited @observer)
                                       (merge-data conn parents tx))}
                          #(try (workspace/advance! canonical observer) nil
                                (catch Exception e e)))]
              (is (some? error))
              (is (= :geschichte.workspace/stale-advance (:type (ex-data error))))
              (is (= @edited @observer))
              (is (= "concurrent\n" (text (repo/read observer "base.txt"))))
              (is (= old (repo/refs observer)))))
          (finally (d/release observer))))
      (finally (cleanup f)))))

(deftest create-only-publication-refuses-all-existing-refs
  (let [{:keys [cfg canonical] :as f} (fixture)]
    (try
      (workspace/fork! canonical "only-publisher")
      (let [worker (d/connect (assoc cfg :branch :geschichte.workspace/only-publisher))]
        (try
          (repo/write! worker "candidate.txt" (->bytes "candidate\n"))
          (repo/stage-all! worker)
          (let [tip (:geschichte.commit/id (repo/commit! worker {:message "candidate"}))
                before-files (repo/worktree canonical)
                result (workspace/publish! canonical worker {:ref "review" :commit tip :create-only? true})]
            (is (true? (:created? result)))
            (is (nil? (:old result)))
            (is (= tip (:new result)))
            (is (= before-files (repo/worktree canonical)))
            (repo/branch! canonical "existing-review")
            (d/transact canonical [{:geschichte.ref/name "refs/heads/unborn-review"}])
            (doseq [ref ["review" "existing-review" "unborn-review"]]
              (let [before @canonical
                    failure (try (workspace/publish! canonical worker {:ref ref :commit tip :create-only? true})
                                 nil (catch Exception e e))]
                (is (= ::workspace/publication-ref-exists (:type (ex-data failure))))
                (is (= before @canonical)))))
          (finally (d/release worker))))
      (finally (cleanup f)))))

(deftest create-only-cannot-be-combined-with-force-before-source-access
  (is (= ::workspace/invalid-publication-options
         (:type (ex-data (try (workspace/publish! nil nil {:create-only? true :force? true})
                              nil (catch Exception e e)))))))

(deftest create-only-publication-refuses-a-racing-unborn-ref-in-the-writer
  (let [{:keys [cfg canonical] :as f} (fixture)]
    (try
      (workspace/fork! canonical "only-publisher")
      (let [worker (d/connect (assoc cfg :branch :geschichte.workspace/only-publisher))
            merge-var (ns-resolve 'geschichte.workspace 'merge-data)
            merge-data @merge-var after-creation (atom nil)]
        (try
          (repo/write! worker "candidate.txt" (->bytes "candidate\n"))
          (repo/stage-all! worker)
          (let [tip (:geschichte.commit/id (repo/commit! worker {:message "candidate"}))
                failure (with-redefs-fn
                          {merge-var (fn [conn parents tx]
                                       (d/transact canonical [{:geschichte.ref/name "refs/heads/review"}])
                                       (reset! after-creation @canonical)
                                       (merge-data conn parents tx))}
                          #(try (workspace/publish! canonical worker
                                                    {:ref "review" :commit tip :create-only? true})
                                nil (catch Exception e e)))]
            (is (= ::workspace/publication-ref-exists (:type (ex-data failure))))
            (is (= @after-creation @canonical) "The losing transfer is rolled back with its ref update")
            (is (contains? (repo/refs canonical) "refs/heads/review"))
            (is (nil? (get (repo/refs canonical) "refs/heads/review"))))
          (finally (d/release worker))))
      (finally (cleanup f)))))

(deftest publication-profile-positively-identifies-the-create-only-contract
  (is (= {:id ::workspace/guarded-publication :version 1 :create-only? true :expected-checkpoint? true}
         (workspace/publication-profile))))

(deftest expected-publication-checkpoint-refuses-before-projection
  (let [{:keys [canonical] :as f} (fixture)]
    (try
      (let [before @canonical calls (atom 0)
            forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Projection started")))
            failure (with-redefs-fn
                      {#'query/refs forbidden #'repo/current-ref forbidden
                       #'workspace/commit-index forbidden #'repo/tree-at forbidden}
                      #(try (workspace/publish! canonical nil {:expected-checkpoint (random-uuid)})
                            nil (catch Exception e e)))]
        (is (= ::workspace/stale-publication (:type (ex-data failure))))
        (is (zero? @calls))
        (is (= before @canonical)))
      (finally (cleanup f)))))

(deftest invalid-expected-publication-checkpoints-refuse-before-source-access
  (doseq [value [nil "invalid" 1]]
    (is (= ::workspace/invalid-publication-options
           (:type (ex-data (try (workspace/publish! nil nil {:expected-checkpoint value})
                                nil (catch Exception e e))))))))

(deftest expected-publication-checkpoint-guards-the-complete-writer-transaction
  (let [{:keys [cfg canonical] :as f} (fixture)]
    (try
      (workspace/fork! canonical "checkpoint-publisher")
      (let [worker (d/connect (assoc cfg :branch :geschichte.workspace/checkpoint-publisher))
            merge-var (ns-resolve 'geschichte.workspace 'merge-data)
            merge-data @merge-var after-edit (atom nil)]
        (try
          (repo/write! worker "candidate.txt" (->bytes "candidate\n"))
          (repo/stage-all! worker)
          (let [tip (:geschichte.commit/id (repo/commit! worker {:message "candidate"}))
                expected (d/commit-id @canonical)
                failure (with-redefs-fn
                          {merge-var (fn [conn parents tx]
                                       (repo/set-config! canonical "user.raced" "true")
                                       (reset! after-edit @canonical)
                                       (merge-data conn parents tx))}
                          #(try (workspace/publish! canonical worker
                                                    {:ref "guarded-review" :commit tip :create-only? true
                                                     :expected-checkpoint expected})
                                nil (catch Exception e e)))]
            (is (= ::workspace/stale-publication (:type (ex-data failure))))
            (is (= expected (:expected-checkpoint (ex-data failure))))
            (is (= @after-edit @canonical))
            (is (not (contains? (repo/refs canonical) "refs/heads/guarded-review")))
            (let [result (workspace/publish! canonical worker
                                             {:ref "guarded-review" :commit tip :create-only? true
                                              :expected-checkpoint (d/commit-id @canonical)})]
              (is (true? (:created? result)))
              (is (= tip (:new result)))
              (is (= (d/commit-id @after-edit) (:expected-checkpoint result)))))
          (finally (d/release worker))))
      (finally (cleanup f)))))
