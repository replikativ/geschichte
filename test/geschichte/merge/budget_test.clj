(ns geschichte.merge.budget-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.experimental.graph :as graph]
            [geschichte.budget :as budget]
            [geschichte.bytes :as bytes]
            [geschichte.content :as content]
            [geschichte.merge :as merge]
            [geschichte.merge.core :as core]
            [geschichte.repo :as repo]))

(defn- operation ([] (operation {}))
  ([limits] (budget/create (update (merge/recommended-budget-policy) :limits clojure.core/merge limits))))
(defn- failure [f] (try (f) nil (catch Exception error (ex-data error))))
(defn- fixture [refs?]
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :attribute-refs? refs? :schema-flexibility :write
             :keep-history? false :commit-graph? true}]
    (d/create-database cfg)
    (doto (d/connect cfg) (repo/init!))))
(defn- commit! [conn files]
  (doseq [[path text] files] (repo/write! conn path (bytes/utf8 text)))
  (repo/stage-all! conn)
  (repo/commit! conn {:message "fixture" :author "test"})
  (:geschichte.commit/id (repo/head-commit conn)))
(defn- divergent [refs?]
  (let [conn (fixture refs?)
        base (commit! conn {"f.txt" "one\ntwo\nthree\nfour\n"})]
    (repo/create-ref! conn "refs/heads/side" (repo/commit-by-id conn base))
    (let [ours (commit! conn {"f.txt" "ONE\ntwo\nthree\nfour\n"})]
      (repo/checkout! conn "refs/heads/side")
      [conn ours (commit! conn {"f.txt" "one\ntwo\nthree\nFOUR\n"})])))

(deftest finite-policy-refuses-before-source-access
  (let [source (reify clojure.lang.IDeref (deref [_] (throw (Exception. "must not read"))))
        ours (random-uuid) theirs (random-uuid)]
    (doseq [b [false (budget/create {})
               (budget/create (update (merge/recommended-budget-policy) :limits dissoc :text-work))]]
      (is (= ::budget/invalid-policy (:type (failure #(merge/plan source ours theirs {:merge-budget b}))))))
    (let [b (operation {:objects 0})]
      (failure #(budget/charge! b :objects 1))
      (is (= ::budget/exceeded (:type (failure #(merge/plan source ours theirs {:merge-budget b}))))))))

(deftest bounded-native-plan-in-both-schema-representations
  (doseq [refs? [false true]]
    (let [[conn ours theirs] (divergent refs?)]
      (try
        (let [b (operation {})
              p (merge/plan conn ours theirs {:merge-budget b})
              state (budget/snapshot b)
              value (content/read-by-id conn (get-in p [:tree "f.txt" :content]))]
          (is (:clean? p))
          (is (= "ONE\ntwo\nthree\nFOUR\n" (bytes/decode-utf8-strict value)))
          (is (= 4 (get-in state [:used :commit-visits])))
          (is (= 3 (get-in state [:used :tree-entries])))
          (is (= 3 (get-in state [:used :content-visits])))
          ;; Returned values must not retain an operation in custom comparators.
          (failure #(budget/exceed! b :text-work 0 1))
          (is (= #{"f.txt" "z"} (set (keys (assoc (:tree p) "z" {:size 0}))))))
        (finally (d/release conn))))))

(deftest resource-failures-never-become-ordinary-conflicts
  (doseq [[limits metric] [[{:commit-visits 0} :commit-visits]
                           [{:graph-edges 0} :graph-edges]
                           [{:tree-entries 2} :tree-entries]
                           [{:content-read-bytes 0} :content-read-bytes]
                           [{:text-work 1} :text-work]]]
    (let [[conn ours theirs] (divergent false) before @conn]
      (try
        (is (= metric (:metric (failure #(merge/plan conn ours theirs {:merge-budget (operation limits)})))))
        (is (= before @conn))
        (finally (d/release conn))))))

(deftest planner-reads-one-snapshot-even-while-private-content-is-written
  (let [[conn ours theirs] (divergent false)
        original graph/bfs-distances fired? (atom false)
        id (get-in (repo/tree-at conn ours) ["f.txt" :content])]
    (try
      (with-redefs [graph/bfs-distances
                    (fn [& args]
                      (let [answer (apply original args)]
                        (when (compare-and-set! fired? false true)
                          (d/transact conn [{:db/id [:geschichte.content/id id]
                                             :geschichte.content/kind :not-text}]))
                        answer))]
        (let [p (merge/plan conn ours theirs {:merge-budget (operation {})})]
          (is @fired?)
          (is (:clean? p))
          (is (= "ONE\ntwo\nthree\nFOUR\n"
                 (bytes/decode-utf8-strict (content/read-by-id conn (get-in p [:tree "f.txt" :content])))))))
      (finally (d/release conn)))))

(deftest recursive-content-limits-cover-deltas-and-chunks
  (let [conn (fixture false)
        base (apply str (map #(str "unchanged line " % " padding\n") (range 300)))]
    (try
      (repo/write! conn "delta.txt" (bytes/utf8 base))
      (let [value (.replace base "line 150" "line changed150")
            id (:content (repo/write! conn "delta.txt" (bytes/utf8 value)))]
        (is (= :line-delta (:geschichte.content/kind (content/info conn id))))
        (is (= value (bytes/decode-utf8-strict (content/read-by-id conn id {:merge-budget (operation {})}))))
        (is (= :content-depth (:metric (failure #(content/read-by-id conn id {:merge-budget (operation {:content-depth 1})}))))))
      (let [value (bytes/from-values (take 10000 (cycle (range 251))))
            id (:content (repo/write! conn "chunks.bin" value {:chunk-threshold 1024 :chunk-min-size 128
                                                               :chunk-size 257 :chunk-max-size 1024}))]
        (is (= :chunks (:geschichte.content/kind (content/info conn id))))
        (is (bytes/same-bytes? value (content/read-by-id conn id {:merge-budget (operation {})})))
        (is (= :content-parts (:metric (failure #(content/read-by-id conn id {:merge-budget (operation {:content-parts 1})}))))))
      (finally (d/release conn)))))

(deftest structural-only-plans-bound-logical-file-and-tree-bytes
  (doseq [opts [{} {:merge-content? false}]
          [limits metric] [[{:object-bytes 5} :object-bytes]
                           [{:tree-bytes 5} :tree-bytes]
                           [{:result-tree-bytes 5} :result-tree-bytes]]]
    (let [conn (fixture false)]
      (try
        (let [base (commit! conn {"base" "x"})
              ours (commit! conn {"large" "1234567890"})
              before @conn]
          (is (= metric (:metric (failure #(merge/plan conn ours base
                                                       (assoc opts :merge-budget (operation limits)))))))
          (is (= before @conn)))
        (finally (d/release conn))))))

(deftest stage-byte-count-must-match-captured-content-metadata
  (let [conn (fixture false)]
    (try
      (let [base (commit! conn {"base" "x"})]
        (repo/write! conn "bad" (bytes/utf8 "1234567890"))
        (repo/stage-all! conn)
        ;; Simulate pre-existing malformed metadata without changing payload.
        (d/transact conn [{:db/id [:geschichte.stage/path "bad"] :geschichte.stage/size 1}])
        (repo/commit! conn {:message "legacy malformed metadata" :author "test"})
        (let [tip (:geschichte.commit/id (repo/head-commit conn)) before @conn]
          (is (= ::merge/invalid-tree-entry
                 (:type (failure #(merge/plan conn tip base {:merge-budget (operation {}) :merge-content? false})))))
          (is (= before @conn))))
      (finally (d/release conn)))))

(deftest repeated-reads-share-one-cumulative-content-account
  (let [conn (fixture false)]
    (try
      (let [value (bytes/utf8 "abcdef") id (:content (repo/write! conn "f" value))
            b (operation {:content-read-bytes 10})]
        (is (bytes/same-bytes? value (content/read-by-id conn id {:merge-budget b})))
        (is (= :content-read-bytes
               (:metric (failure #(content/read-by-id conn id {:merge-budget b})))))
        (is (= 6 (get-in (budget/snapshot b) [:used :content-read-bytes]))))
      (finally (d/release conn)))))

(deftest cancellation-during-final-value-copy-cannot-return-success
  (let [cancelled? (atom false)
        b (budget/create (assoc (merge/recommended-budget-policy) :cancelled? #(deref cancelled?)))
        entry {:content (random-uuid) :size 1 :mode 33188}
        tree {"f" entry}
        original into]
    (with-redefs [clojure.core/into
                  (fn
                    ([] (original))
                    ([to] (original to))
                    ([to from]
                     (let [answer (original to from)]
                       (when (and (instance? clojure.lang.Sorted to)
                                  (instance? clojure.lang.Sorted from)
                                  (not (identical? (.comparator ^clojure.lang.Sorted to)
                                                   (.comparator ^clojure.lang.Sorted from))))
                         (reset! cancelled? true))
                       answer))
                    ([to xf from] (original to xf from)))]
      (is (= ::budget/cancelled
             (:type (failure #(core/plan-trees (random-uuid) (random-uuid) (random-uuid)
                                               tree tree tree {:merge-budget b}))))))
    (is @cancelled?)))
