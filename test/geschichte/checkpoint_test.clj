(ns geschichte.checkpoint-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.versioning :as versioning]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [geschichte.workspace :as workspace]))

(defn- with-repo [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
             :keep-history? true :commit-graph? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (repo/init! conn) (f conn cfg)
           (finally (d/release conn) (d/delete-database cfg))))))

(defn- write! [conn path text] (repo/write! conn path (.getBytes ^String text "UTF-8")))
(defn- text [conn path] (some-> (repo/read conn path) (String. "UTF-8")))
(defn- origin [db]
  {:version 1 :kind :datahike :commit (d/commit-id db)
   :source {:store (get-in db [:config :store :id])
            :repository (:geschichte.repo/id (query/repository db))}})
(defn- type-of [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(deftest descriptor-is-repository-specific-closed-data
  (let [value {:version 1 :kind :datahike :commit (random-uuid)
               :source {:store (random-uuid) :repository (random-uuid)}}]
    (is (workspace/checkpoint-origin? value))
    (doseq [bad [nil {} (assoc value :version 2) (assoc value :kind :git)
                 (assoc value :commit (str (:commit value))) (assoc value :conn :native)
                 (assoc-in value [:source :path] "/private") (dissoc value :source)
                 (assoc-in value [:source :repository] nil)]]
      (is (not (workspace/checkpoint-origin? bad))))))

(deftest direct-workspace-fork-preserves-dirty-and-staged-checkpoint-state
  (with-repo
    (fn [conn cfg]
      (write! conn "state.txt" "C1 staged")
      (repo/stage-all! conn)
      (write! conn "dirty.txt" "C1 dirty")
      (let [c1 @conn descriptor (origin c1) db (workspace/resolve-checkpoint conn descriptor)]
        (write! conn "state.txt" "C2")
        (write! conn "future.txt" "Only C2")
        (let [parent @conn branch (workspace/fork-at-checkpoint! conn "exact" descriptor db)]
          (is (= :geschichte.workspace/exact branch))
          (let [child (d/connect (assoc cfg :branch branch))]
            (try
              (is (= @child (workspace/verify-checkpoint-workspace! db @child branch)))
              (is (= (query/worktree c1) (query/worktree @child)))
              (is (= (query/stage c1) (query/stage @child)))
              (is (= "C1 staged" (text child "state.txt")))
              (is (= "C1 dirty" (text child "dirty.txt")))
              (is (nil? (text child "future.txt")))
              (write! child "own.txt" "private child edit")
              (is (nil? (text conn "own.txt")))
              (is (= parent @conn))
              (finally (d/release child) (workspace/remove! conn branch)))))))))

(deftest invalid-or-unavailable-source-and-modes-refuse-before-allocation
  (with-repo
    (fn [conn _]
      (let [db @conn descriptor (origin db)
            refuse (fn [& _] (throw (AssertionError. "No allocation or connect")))]
        (with-redefs [versioning/branch! refuse d/connect refuse]
          (is (= db (workspace/resolve-checkpoint conn descriptor)))
          (doseq [bad [(assoc descriptor :commit (random-uuid))
                       (assoc-in descriptor [:source :store] (random-uuid))
                       (assoc-in descriptor [:source :repository] (random-uuid))
                       (assoc descriptor :extra true)]]
            (is (thrown? clojure.lang.ExceptionInfo (workspace/resolve-checkpoint conn bad))))
          (is (thrown? clojure.lang.ExceptionInfo
                       (workspace/fork-at-checkpoint! conn "bad" (assoc descriptor :commit (random-uuid)) db))))
        (with-redefs [versioning/commit-as-db refuse versioning/branch! refuse]
          (doseq [opts [{:sync? false} {:sync? nil} {:sync? :yes} {:mode :frozen} []]]
            (is (= ::workspace/invalid-checkpoint-mode
                   (type-of #(workspace/resolve-checkpoint conn descriptor opts))))
            (is (= ::workspace/invalid-checkpoint-mode
                   (type-of #(workspace/fork-at-checkpoint! conn "bad" descriptor db opts))))))
        (is (= #{:db} (d/branches conn)))))))

(deftest existing-destination-is-preserved-and-changed-child-refuses
  (with-repo
    (fn [conn cfg]
      (let [db @conn descriptor (origin db)
            branch (workspace/fork-at-checkpoint! conn "existing" descriptor db)
            child (d/connect (assoc cfg :branch branch))]
        (try
          (is (= ::workspace/checkpoint-child-changed
                 (type-of #(workspace/verify-checkpoint-workspace! db (update @child :max-eid inc) branch))))
          (is (= ::workspace/checkpoint-child-changed
                 (type-of #(workspace/verify-checkpoint-workspace! db @child "wrong-branch"))))
          (write! child "keep.txt" "existing child")
          (let [before @child]
            (is (thrown? clojure.lang.ExceptionInfo
                         (workspace/fork-at-checkpoint! conn "existing" descriptor db)))
            (is (= before @child))
            (is (= "existing child" (text child "keep.txt"))))
          (finally (d/release child) (workspace/remove! conn branch)))))))

(deftest unfinished-secondary-generations-refuse-without-allocation
  (with-repo
    (fn [conn _]
      (let [descriptor (origin @conn)
            building (assoc-in @conn [:schema :fixture/building] {:db.secondary/status :building})]
        (with-redefs [versioning/commit-as-db (fn [& _] building)
                      versioning/branch! (fn [& _] (throw (AssertionError. "No branch for building index")))]
          (is (= ::workspace/checkpoint-building-index
                 (type-of #(workspace/resolve-checkpoint conn descriptor)))))))))
