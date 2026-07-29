(ns geschichte.merge.content-test
  "`plan-trees`' content-merge hook, against a real repository.

   `resolve-path` decides a path from its tree entries, and its first three
   cases need nothing else. The fourth — both sides changed it — is where git
   merges the two blobs before declaring a conflict, and this is that step
   wired up: the planner stays pure and a resolver built from a connection
   supplies the reading and writing.

   What these pin is where the resolver must DECLINE. Each of those cases would
   otherwise produce a plausible wrong answer rather than an error: binary
   content line-merged into garbage, a mode change treated as a text edit, a
   huge blob pulled into memory during a merge."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [geschichte.bytes :as bytes]
            [geschichte.content :as content]
            [geschichte.merge :as gmerge]
            [geschichte.repo :as repo]))

(defn- fresh []
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false :commit-graph? true}]
    (d/create-database cfg)
    (doto (d/connect cfg) (repo/init! {:name "merge-test"}))))

(defn- commit! [conn path value msg]
  (repo/write! conn path value)
  (repo/stage-all! conn)
  (repo/commit! conn {:message msg :author "test"})
  (:geschichte.commit/id (repo/head-commit conn)))

(defn- two-sided
  "Base, then a divergent commit on each side. `f` writes each side's content.
   Returns `[conn ours theirs]`."
  [base-value ours-value theirs-value & [path]]
  (let [path (or path "f.txt")
        conn (fresh)
        base (commit! conn path base-value "base")]
    (repo/create-ref! conn "refs/heads/side" (repo/commit-by-id conn base))
    (let [ours (commit! conn path ours-value "ours")]
      (repo/checkout! conn "refs/heads/side")
      (let [theirs (commit! conn path theirs-value "theirs")]
        [conn ours theirs]))))

(defn- text-of [conn plan path]
  (String. (content/read-by-id conn (:content (get (:tree plan) path)))))

;; ---------------------------------------------------------------------------

(deftest disjoint-line-edits-no-longer-conflict
  ;; THE case. Both sides changed one file, in different places.
  (let [[conn ours theirs] (two-sided (bytes/utf8 "one\ntwo\nthree\nfour\n")
                                      (bytes/utf8 "ONE\ntwo\nthree\nfour\n")
                                      (bytes/utf8 "one\ntwo\nthree\nFOUR\n"))]
    (testing "structurally it is a conflict, and that is what it used to be"
      (let [p (gmerge/plan conn ours theirs {:merge-content? false})]
        (is (not (:clean? p)))
        (is (= ["f.txt"] (keys (:conflicts p))))))

    (testing "with the content merge it resolves, carrying BOTH edits"
      (let [p (gmerge/plan conn ours theirs)]
        (is (:clean? p))
        (is (= #{"f.txt"} (set (:merged p)))
            "and says which paths it reconciled, so a caller can name them")
        (is (= "ONE\ntwo\nthree\nFOUR\n" (text-of conn p "f.txt")))))))

(deftest competing-edits-to-one-line-still-conflict
  (let [[conn ours theirs] (two-sided (bytes/utf8 "a\nold\nc\n")
                                      (bytes/utf8 "a\nOURS\nc\n")
                                      (bytes/utf8 "a\nTHEIRS\nc\n"))
        p (gmerge/plan conn ours theirs)]
    (is (not (:clean? p)))
    (is (= ["f.txt"] (keys (:conflicts p)))
        "a real overlap must still reach a person — auto-resolving it is how a
         merge tool loses work")
    (is (empty? (:merged p)))))

(deftest the-structural-cases-are-untouched
  (testing "only one side changed the file"
    ;; theirs diverges on a DIFFERENT path — a side that wrote the same bytes
    ;; as base has nothing to commit, so it would not be a second branch at all
    (let [conn (fresh)
          base (commit! conn "f.txt" (bytes/utf8 "a\n") "base")
          _ (repo/create-ref! conn "refs/heads/side" (repo/commit-by-id conn base))
          ours (commit! conn "f.txt" (bytes/utf8 "CHANGED\n") "ours")
          _ (repo/checkout! conn "refs/heads/side")
          theirs (commit! conn "other.txt" (bytes/utf8 "unrelated\n") "theirs")
          p (gmerge/plan conn ours theirs)]
      (is (:clean? p))
      (is (empty? (:merged p))
          "resolved by the entry comparison, so the content merger never ran")
      (is (= "CHANGED\n" (text-of conn p "f.txt")))))
  (testing "both sides made the identical change"
    (let [[conn ours theirs] (two-sided (bytes/utf8 "a\n")
                                        (bytes/utf8 "SAME\n")
                                        (bytes/utf8 "SAME\n"))
          p (gmerge/plan conn ours theirs)]
      (is (:clean? p))
      (is (empty? (:merged p))))))

(deftest binary-content-is-declined-not-guessed
  ;; A NUL byte means the bytes are not lines. Merging them by line would
  ;; produce a file that is neither side's and is not valid either.
  (let [nul (byte-array [97 0 98 10])
        ours (byte-array [88 0 98 10])
        theirs (byte-array [97 0 89 10])
        [conn o t] (two-sided nul ours theirs "blob.bin")
        p (gmerge/plan conn o t)]
    (is (not (:clean? p)))
    (is (= ["blob.bin"] (keys (:conflicts p))))))

(deftest an-oversized-side-is-declined
  (let [big (apply str (repeat 300 "line of text\n"))
        [conn ours theirs] (two-sided (bytes/utf8 big)
                                      (bytes/utf8 (str "OURS\n" big))
                                      (bytes/utf8 (str big "THEIRS\n")))]
    (testing "under the ceiling it merges"
      (is (:clean? (gmerge/plan conn ours theirs))))
    (testing "above it the path conflicts rather than loading the blob"
      (let [p (geschichte.merge.core/plan-trees
               (gmerge/merge-base conn ours theirs) ours theirs
               (repo/tree-at conn (gmerge/merge-base conn ours theirs))
               (repo/tree-at conn ours)
               (repo/tree-at conn theirs)
               {:resolve-content (repo/content-merger conn {:max-bytes 10})})]
        (is (not (:clean? p)))))))

(deftest a-mode-change-is-not-a-text-change
  ;; Same lines on both sides but different modes: the disagreement is about
  ;; what the file IS, and a line merge has nothing to say about it.
  (let [conn (fresh)
        base (commit! conn "s" (bytes/utf8 "a\n") "base")]
    (repo/create-ref! conn "refs/heads/side" (repo/commit-by-id conn base))
    (let [ours (commit! conn "s" (bytes/utf8 "OURS\n") "ours")]
      (repo/checkout! conn "refs/heads/side")
      (let [theirs (commit! conn "s" (bytes/utf8 "THEIRS\n") "theirs")
            base-c (gmerge/merge-base conn ours theirs)
            bump (fn [tree] (update tree "s" assoc :mode 33261))
            p (geschichte.merge.core/plan-trees
               base-c ours theirs
               (repo/tree-at conn base-c)
               (bump (repo/tree-at conn ours))
               (repo/tree-at conn theirs)
               {:resolve-content (repo/content-merger conn)})]
        (is (not (:clean? p))
            "differing modes decline before any text is read")))))

(deftest no-resolver-is-the-old-behaviour
  (let [[conn ours theirs] (two-sided (bytes/utf8 "one\ntwo\n")
                                      (bytes/utf8 "ONE\ntwo\n")
                                      (bytes/utf8 "one\nTWO\n"))
        base (gmerge/merge-base conn ours theirs)
        p (geschichte.merge.core/plan-trees
           base ours theirs
           (repo/tree-at conn base)
           (repo/tree-at conn ours)
           (repo/tree-at conn theirs))]
    (is (not (:clean? p))
        "the six-arity plan is unchanged, so nothing that does not ask for
         content merging can be surprised by it")))

(deftest a-merged-plan-can-actually-be-applied
  ;; The planning tests above stop at the plan, and a plan is not a merge. This
  ;; one commits it — which is where a tree entry with a wrong value TYPE
  ;; surfaces, since datahike checks the class only on transact.
  (let [[conn ours theirs] (two-sided (bytes/utf8 "one\ntwo\nthree\nfour\n")
                                      (bytes/utf8 "ONE\ntwo\nthree\nfour\n")
                                      (bytes/utf8 "one\ntwo\nthree\nFOUR\n"))]
    (repo/checkout! conn "refs/heads/main")
    (let [p (gmerge/plan conn ours theirs)]
      (is (:clean? p))
      (repo/prepare-merge! conn p)
      (repo/commit! conn {:message "merge" :author "test"})
      (is (= "ONE\ntwo\nthree\nFOUR\n" (String. (repo/read conn "f.txt")))
          "both edits, materialized in the worktree")
      ;; re-resolve the tip: `head-commit` pulls the REF TARGET, and that pull
      ;; pattern carries no `:geschichte.commit/parents` — reading parents off
      ;; it answers 0 for every commit
      (is (= 2 (count (:geschichte.commit/parents
                       (repo/commit-by-id conn (:geschichte.commit/id
                                                (repo/head-commit conn))))))
          "and it is a real two-parent merge commit"))))
