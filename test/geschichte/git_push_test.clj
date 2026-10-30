(ns geschichte.git-push-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.content :as content]
            [geschichte.git.budget :as budget]
            [geschichte.git.object :as object]
            [geschichte.git.pack :as pack]
            [geschichte.git.project :as project]
            [geschichte.git.push :as push]
            [geschichte.git.pkt-line :as pkt]
            [geschichte.git.receive-pack :as receive]
            [geschichte.repo :as repo]))

(defn- fixture [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
             :keep-history? true :commit-graph? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (repo/init! conn {:branch "refs/heads/main"}) (f conn)
           (finally (d/release conn) (d/delete-database cfg))))))

(defn- commit! [conn path text]
  (repo/write! conn path (object/utf8 text)) (repo/stage-all! conn)
  (:geschichte.commit/id (repo/commit! conn {:message path :author "Test Author"})))

(defn- op
  ([] (op {}))
  ([limits] (budget/create (update (push/recommended-policy 30000) :limits merge limits))))

(defn- options [operation]
  {:ref "refs/heads/main" :expected-remote-oid receive/zero-oid :operation operation})

(deftest branch-refs-reject-empty-trailing-segments
  (is (push/ref? "refs/heads/main"))
  (is (not (push/ref? "refs/heads/main/"))))

(deftest encoded-object-limits-are-admitted-before-payload-allocation
  (fixture
   (fn [conn]
     (let [head (commit! conn "file.txt" "x")]
       (with-redefs [object/tree-payload (fn [& _] (throw (AssertionError. "Tree encoded before admission")))]
         (is (= :object-bytes
                (try (push/prepare conn head (options (op {:object-bytes 12}))) nil
                     (catch clojure.lang.ExceptionInfo e (:metric (ex-data e)))))))
       (with-redefs [object/commit-payload (fn [& _] (throw (AssertionError. "Commit encoded before admission")))]
         (is (= :object-bytes
                (try (push/prepare conn head (options (op {:object-bytes 80}))) nil
                     (catch clojure.lang.ExceptionInfo e (:metric (ex-data e)))))))))))

(deftest bounded-authored-export-is-deterministic-and-mapping-does-not-change-files
  (fixture
   (fn [conn]
     (commit! conn "base.txt" "Base\n")
     (let [head (commit! conn "next.txt" "Next\n")
           before @conn prepared (push/prepare conn head (options (op)))
           again (push/prepare conn head (options (op)))
           ordinary (project/project conn head)]
       (is (= (:new prepared) (:new again) (:oid ordinary)))
       (is (= (seq (:pack prepared)) (seq (:pack again))))
       (is (= 2 (count (:commits prepared))))
       (is (= before @conn))
       (let [files (repo/worktree conn)]
         (d/transact conn [[:db.fn/call push/mapping-tx (dissoc prepared :pack)]])
         (is (= files (repo/worktree conn)))
         (is (= (:new prepared) (:geschichte.commit/git-oid (repo/commit-by-id conn head))))
         (is (= (:new prepared) (:new (push/prepare conn head (options (op)))))))))))

(deftest receiver-known-ancestry-omits-existing-commits-and-preserves-their-oids
  (fixture
   (fn [conn]
     (let [base (commit! conn "base.txt" "Base\n")
           original (push/prepare conn base (options (op)))]
       (d/transact conn [[:db.fn/call push/mapping-tx (dissoc original :pack)]])
       (let [head (commit! conn "next.txt" "Next\n")
             prepared (push/prepare conn head (assoc (options (op)) :upstream-commit base :expected-remote-oid (:new original)))]
         (is (= #{head} (set (keys (:commits prepared)))))
         (is (= (:new original) (:old prepared)))
         (is (= (:oid (project/project conn head)) (:new prepared))))))))

(deftest frontier-is-only-a-boundary-when-the-receiver-already-knows-it
  (fixture
   (fn [conn]
     (let [head (commit! conn "file.txt" "File\n") frontier (random-uuid) oid (apply str (repeat 40 "a"))]
       (d/transact conn [{:geschichte.commit/id frontier :geschichte.commit/git-oid oid}
                         {:geschichte.commit/id head :geschichte.commit/parents [[:geschichte.commit/id frontier]]}])
       (is (push/oid? (:new (push/prepare conn head (assoc (options (op)) :upstream-commit frontier :expected-remote-oid oid)))))
       (is (= :missing-history
              (try (push/prepare conn head (options (op))) nil
                   (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))))

(deftest receiver-known-walk-follows-lightweight-imported-ancestors
  (fixture
   (fn [conn]
     (let [base (commit! conn "base.txt" "Base\n")
           middle (commit! conn "middle.txt" "Middle\n")
           upstream (commit! conn "upstream.txt" "Upstream\n")
           known (push/prepare conn upstream (options (op)))]
       (d/transact conn [[:db.fn/call push/mapping-tx (dissoc known :pack)]])
        ;; Imported ancestry retains verified parent metadata but ordinarily
        ;; materializes snapshots only for selected tips. This is NOT a frontier.
       (let [eid (:db/id (d/entity @conn [:geschichte.commit/id middle]))
             snapshot (:geschichte.commit/snapshot (d/entity @conn eid))]
         (d/transact conn [[:db/retract eid :geschichte.commit/snapshot snapshot]]))
       (let [head (commit! conn "local.txt" "Local draft\n")]
         (d/transact conn [[:db/add [:geschichte.commit/id head] :geschichte.commit/parents [:geschichte.commit/id base]]])
         (let [prepared (push/prepare conn head (assoc (options (op)) :upstream-commit upstream :expected-remote-oid (:new known)))]
           (is (= #{head} (set (keys (:commits prepared)))))
           (is (= (:new known) (:old prepared)))))))))

(deftest a-stored-git-identity-is-never-silently-replaced
  (fixture
   (fn [conn]
     (let [head (commit! conn "file.txt" "File\n")]
       (d/transact conn [{:geschichte.commit/id head :geschichte.commit/git-oid (apply str (repeat 40 "b"))}])
       (is (= :projection-mismatch
              (try (push/prepare conn head (options (op))) nil
                   (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))))

(deftest cyclic-authored-history-refuses-without-recursive-descent
  (fixture
   (fn [conn]
     (let [head (commit! conn "file.txt" "File\n")]
       (d/transact conn [[:db/add [:geschichte.commit/id head] :geschichte.commit/parents [:geschichte.commit/id head]]])
       (is (= :cyclic-history
              (try (push/prepare conn head (options (op))) nil
                   (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))))

(deftest graph-tree-and-pack-budgets-refuse-before-unbounded-growth
  (fixture
   (fn [conn]
     (let [head (commit! conn "file.txt" "File\n") reads (atom 0)]
       (with-redefs [content/read-by-id (fn [& _] (swap! reads inc) (throw (AssertionError. "Content read before tree admission")))]
         (is (= :tree-entries
                (try (push/prepare conn head (options (op {:tree-entries 0}))) nil
                     (catch clojure.lang.ExceptionInfo e (:metric (ex-data e)))))))
       (is (zero? @reads))
       (is (= :objects
              (try (push/prepare conn head (options (op {:objects 0}))) nil
                   (catch clojure.lang.ExceptionInfo e (:metric (ex-data e))))))
       (is (= :pack-bytes
              (try (push/prepare conn head (options (op {:pack-bytes 8}))) nil
                   (catch clojure.lang.ExceptionInfo e (:metric (ex-data e))))))))))

(defn- advertisement [oid]
  {:status 200 :body (pkt/encode [(str oid " refs/heads/main\u0000report-status object-format=sha1\n") :flush])})

(deftest stale-remote-refuses-before-projecting-or-accessing-the-source
  (let [calls (atom []) source (reify clojure.lang.IDeref (deref [_] (throw (AssertionError. "Source accessed"))))
        sender (fn [request] (swap! calls conj (:method request)) (advertisement (apply str (repeat 40 "b"))))]
    (is (= :remote-changed
           (try (push/prepare-http! sender source "https://example.test/repo.git" (random-uuid)
                                    (assoc (options (op)) :expected-remote-oid (apply str (repeat 40 "a"))))
                nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
    (is (= [:get] @calls))))

(deftest exact-wire-cas-and-one-positive-status-are-required
  (fixture
   (fn [conn]
     (let [head (commit! conn "file.txt" "File\n") prepared (assoc (push/prepare conn head (options (op))) :capabilities ["report-status"])
           calls (atom []) sender (fn [request]
                                    (swap! calls conj request)
                                    {:status 200 :body (pkt/encode ["unpack ok\n" "ok refs/heads/main\n" :flush])})]
       (is (= {:status :pushed :ref "refs/heads/main" :old receive/zero-oid :new (:new prepared)}
              (push/send-http! sender "https://example.test/repo.git" prepared (op))))
       (let [^bytes body (:body (first @calls))
             length (Integer/parseInt (String. body 0 4 "UTF-8") 16)
             command (String. body 4 (- length 4) "UTF-8")]
         (is (.startsWith command (str receive/zero-oid " " (:new prepared) " refs/heads/main"))))
       (doseq [report [["unpack ok\n" :flush]
                       ["unpack ok\n" "ok refs/heads/main\n" "ok refs/heads/main\n" :flush]
                       ["unpack ok\n" "ok refs/heads/other\n" :flush]]]
         (is (= :outcome-unconfirmed
                (try (push/send-http! (constantly {:status 200 :body (pkt/encode report)})
                                      "https://example.test/repo.git" prepared (op))
                     nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))))))

(deftest report-status-accepts-optional-line-feeds-without-relaxing-the-report-shape
  ;; gitprotocol-common requires receivers to accept non-binary pkt-lines
  ;; with or without their final LF. Keep exact single-ref acknowledgment.
  (let [prepared {:version 1 :projection push/projection :ref "refs/heads/main"
                  :old (apply str (repeat 40 "a")) :new (apply str (repeat 40 "b"))
                  :pack (byte-array 0) :capabilities ["report-status"]}]
    (doseq [unpack-lf ["" "\n"] status-lf ["" "\n"] status [:pushed :rejected]]
      (let [line (if (= :pushed status) "ok refs/heads/main" "ng refs/heads/main rejected")
            sender (constantly {:status 200 :body (pkt/encode [(str "unpack ok" unpack-lf)
                                                               (str line status-lf) :flush])})]
        (is (= status (:status (push/send-http! sender "https://example.test/repo.git" prepared (op)))))))
    (doseq [report [["unpack ok" "ok refs/heads/main" "ok refs/heads/main" :flush]
                    ["unpack ok" "ok refs/heads/other" :flush]
                    ["unpack ok" "ok refs/heads/main"]
                    ["unpack ok" "ok refs/heads/main" :flush :flush]
                    ["unpack ok\n\n" "ok refs/heads/main" :flush]
                    ["unpack ok" "ok refs/heads/main\n\n" :flush]
                    ["unpack ok" "ok refs/heads/main " :flush]
                    ["unpack error" "ok refs/heads/main" :flush]]]
      (is (= :outcome-unconfirmed
             (try (push/send-http! (constantly {:status 200 :body (pkt/encode report)})
                                   "https://example.test/repo.git" prepared (op))
                  nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))))

(deftest bounded-pack-keeps-the-existing-wire-format
  (let [payload (object/utf8 "Example\n") objects {(object/object-id :blob payload) {:type :blob :payload payload}}]
    (is (= (seq (pack/encode objects)) (seq (pack/encode-bounded objects (op)))))))

(deftest a-common-ancestor-does-not-substitute-for-fast-forward-ancestry
  (fixture
   (fn [conn]
     (let [base (commit! conn "base.txt" "Base\n")
           upstream (commit! conn "upstream.txt" "Upstream\n")
           known (push/prepare conn upstream (options (op)))]
       (d/transact conn [[:db.fn/call push/mapping-tx (dissoc known :pack)]])
       (let [head (commit! conn "local.txt" "Local\n") eid (:db/id (d/entity @conn [:geschichte.commit/id head]))]
         (d/transact conn [[:db/retract eid :geschichte.commit/parents [:geschichte.commit/id upstream]]
                           [:db/add eid :geschichte.commit/parents [:geschichte.commit/id base]]])
         (is (= :not-fast-forward
                (try (push/prepare conn head (assoc (options (op)) :upstream-commit upstream :expected-remote-oid (:new known))) nil
                     (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))))))

(deftest canonical-parent-order-is-independent-of-native-enumeration
  (fixture
   (fn [conn]
     (let [base (commit! conn "base.txt" "Base\n")
           left (commit! conn "left.txt" "Left\n")
           head (commit! conn "head.txt" "Head\n")
           _ (d/transact conn [[:db/add [:geschichte.commit/id head] :geschichte.commit/parents [:geschichte.commit/id base]]])
           normal (push/prepare conn head (options (op)))
           reader @#'push/commit!]
       (is (= 2 (count (:parents (reader @conn head (op))))))
       (with-redefs-fn {#'push/commit! (fn [& args] (update (apply reader args) :parents #(vec (reverse %))))}
         #(is (= (:new normal) (:new (push/prepare conn head (options (op)))))))))))

(deftest mappings-refuse-an-intervening-source-write
  (fixture
   (fn [conn]
     (let [head (commit! conn "file.txt" "File\n") prepared (push/prepare conn head (options (op)))]
       (repo/write! conn "new.txt" (object/utf8 "New\n"))
       (let [before @conn]
         (is (= :source-changed
                (try (d/transact conn [[:db.fn/call push/mapping-tx (dissoc prepared :pack)]]) nil
                     (catch clojure.lang.ExceptionInfo e
                       (some :reason (map ex-data (take-while some? (iterate ex-cause e))))))))
         (is (= before @conn)))))))

(deftest incomplete-policies-refuse-before-network-or-source-access
  (doseq [policy [(dissoc (push/recommended-policy) :timeout-ms)
                  (update (push/recommended-policy) :limits dissoc :objects)]]
    (let [source (reify clojure.lang.IDeref (deref [_] (throw (AssertionError. "Source accessed"))))]
      (is (thrown? clojure.lang.ExceptionInfo
                   (push/prepare-http! (fn [& _] (throw (AssertionError. "Network accessed"))) source
                                       "https://example.test/repo.git" (random-uuid) (options (budget/create policy))))))))

(deftest a-thrown-dispatch-is-never-retried
  (fixture
   (fn [conn]
     (let [head (commit! conn "file.txt" "File\n") prepared (assoc (push/prepare conn head (options (op))) :capabilities ["report-status"])
           calls (atom 0)]
       (is (thrown? clojure.lang.ExceptionInfo
                    (push/send-http! (fn [_] (swap! calls inc) (throw (ex-info "Response lost" {})))
                                     "https://example.test/repo.git" prepared (op))))
       (is (= 1 @calls))))))
