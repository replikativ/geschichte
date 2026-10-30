(ns geschichte.historical-source-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [geschichte.bytes :as bytes]
            [geschichte.content :as content]
            [geschichte.historical-source-fixture :as fixture]
            [geschichte.repo :as repo]
            [geschichte.query :as query]
            [geschichte.workspace :as workspace]))

(deftest historical-materialization-preserves-identities-and-unrelated-paths
  (doseq [kind [:full :line-delta :chunks]
          operation [:checkout :restore :mixed :hard :soft]]
    (testing (str kind " " operation)
      (fixture/with-archive
        kind
        (fn [{:keys [conn canonical c1 c2 id text]}]
          (repo/write! conn "untracked.txt" (bytes/utf8 "keep\n"))
          (let [before (fixture/state conn) target-before (fixture/state canonical)]
            (case operation
              :checkout (repo/checkout! conn "archive")
              :restore (repo/restore-paths! conn ["old.txt"] {:source c1 :staged? true})
              (repo/reset! conn c1 {:mode operation}))
            (is (= target-before (fixture/state canonical)))
            (is (= "keep\n" (bytes/decode-utf8-strict (repo/read conn "untracked.txt"))))
            (if (= :soft operation)
              (do (is (= (:work before) (repo/worktree conn)))
                  (is (= (:stage before) (:stage (fixture/state conn)))))
              (do (is (= id (get-in (repo/index-tree conn) ["old.txt" :content])))
                  (is (= text (bytes/decode-utf8-strict (repo/read-entry conn (get (repo/index-tree conn) "old.txt")))))
                  (if (= :mixed operation)
                    (is (= (:work before) (repo/worktree conn)))
                    (is (= text (bytes/decode-utf8-strict (repo/read conn "old.txt")))))))
            (is (= (if (= :restore operation) c2 c1)
                   (:geschichte.commit/id (repo/head-commit conn))))))))))

(deftest refused-checkout-preserves-public-state
  (fixture/with-archive
    :line-delta
    (fn [{:keys [conn]}]
      (repo/write! conn "tip.txt" (bytes/utf8 "dirty\n"))
      (let [before (fixture/state conn)]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"overwrite" (repo/checkout! conn "archive")))
        (is (= before (fixture/state conn)))))))

(deftest selected-historical-publication-and-advance-use-snapshot-closure
  (doseq [kind [:full :line-delta :chunks]]
    (fixture/with-archive
      kind
      (fn [{:keys [conn canonical cfg base c1 id text]}]
        (repo/set-config! conn "private" "yes")
        (let [source-before (fixture/state conn)]
          (workspace/publish! canonical conn {:ref "archived" :commit c1 :create? true
                                              :expected-checkpoint (d/commit-id @canonical)})
          (is (= source-before (fixture/state conn)))
          (is (nil? (get (repo/configuration canonical) "private")))
          (is (nil? (get (repo/refs canonical) "refs/heads/archive")))
          (is (= text (bytes/decode-utf8-strict (repo/read-at canonical c1 "old.txt")))))
       ;; Start at a compatible ancestor and advance to an old selected ref.
        (workspace/fork! canonical "archival-reader")
        (let [reader (d/connect (assoc cfg :branch :geschichte.workspace/archival-reader))]
          (try
            (repo/reset! reader base {:mode :hard})
            (repo/reset! conn c1 {:mode :soft})
            (workspace/advance! conn reader {:expected-checkpoint (d/commit-id @reader)})
            (is (= id (get-in (repo/worktree reader) ["old.txt" :content])))
            (is (= text (bytes/decode-utf8-strict (repo/read reader "old.txt"))))
            (finally (d/release reader))))))))

(deftest content-preparation-failure-preserves-public-state
  (fixture/with-archive
    :chunks
    (fn [{:keys [conn c1 id]}]
      (let [before (fixture/state conn) writer content/transact-content!]
        (with-redefs [content/transact-content!
                      (fn [& args]
                        (apply writer args)
                        (throw (ex-info "injected after immutable write" {})))]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"injected"
                                (repo/restore-paths! conn ["old.txt"] {:source c1 :staged? true}))))
        (is (= before (fixture/state conn)))
        (is (some? (content/info conn id)))
       ;; Repeating preparation is safe and installs the same logical bytes.
        (repo/restore-paths! conn ["old.txt"] {:source c1 :staged? true})
        (is (= id (get-in (repo/index-tree conn) ["old.txt" :content])))))))

(deftest three-round-collaboration-preserves-the-byte-tree-model
  (fixture/with-archive
    :line-delta
    (fn [{:keys [conn canonical]}]
      (let [expected (atom {"base.txt" "base\n" "tip.txt" "tip\n"})]
        (doseq [round (range 3)]
          (let [path (str "round-" round ".txt") text (str "round " round " λ\n")]
            (repo/write! conn path (bytes/utf8 text) {:mode (if (even? round) 33261 33188)})
            (swap! expected assoc path text)
            (when (pos? round)
              (let [removed (str "round-" (dec round) ".txt")]
                (repo/remove! conn removed)
                (swap! expected dissoc removed)))
            (let [tip (fixture/commit! conn (str "round " round))]
              (workspace/publish! canonical conn {:expected-checkpoint (d/commit-id @canonical)})
              (workspace/advance! canonical conn {:expected-checkpoint (d/commit-id @conn)})
              (is (= tip (:geschichte.commit/id (repo/head-commit conn))))
              (is (= (repo/index-tree conn) (repo/tree-at conn tip)))
              (is (= (repo/worktree conn) (repo/index-tree conn)))
              (is (= @expected (into {} (map (fn [[p _]] [p (bytes/decode-utf8-strict (repo/read conn p))])) (repo/worktree conn))))
              (is (= @expected (into {} (map (fn [[p _]] [p (bytes/decode-utf8-strict (repo/read-at canonical tip p))])) (repo/tree-at canonical tip)))))))))))

(deftest entry-readers-do-not-project-the-current-tree
  (fixture/with-archive
    :full
    (fn [{:keys [conn c1 text]}]
      (let [historical (get (repo/tree-at conn c1) "old.txt")
            current (get (repo/worktree conn) "tip.txt")]
        (with-redefs [query/worktree (fn [& _] (throw (AssertionError. "Tree projection during entry read")))]
          (is (= "tip\n" (bytes/decode-utf8-strict (repo/read-entry conn current))))
          (is (= text (bytes/decode-utf8-strict (repo/read-entry conn historical c1)))))))))

(deftest lightweight-head-cannot-be-presented-as-an-empty-tree
  (fixture/with-archive
    :full
    (fn [{:keys [conn]}]
      (let [head (repo/head-commit conn)]
        (d/transact conn [[:db/retract (:db/id head) :geschichte.commit/snapshot (:geschichte.commit/snapshot head)]])
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown Geschichte commit" (repo/tree conn :head)))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown Geschichte commit" (repo/tree-at conn (:geschichte.commit/id head))))))))
