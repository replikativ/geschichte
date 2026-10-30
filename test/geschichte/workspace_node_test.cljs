(ns geschichte.workspace-node-test
  (:require [cljs.core.async :refer [<!] :refer-macros [go]]
            [cljs.test :refer-macros [async deftest is]]
            [datahike.api :as d]
            [geschichte.bytes :as bytes]
            [geschichte.repo :as repo]
            [geschichte.workspace :as workspace]
            [is.simm.partial-cps.core-async :as bridge]))

(defn- take-cps [computation]
  (bridge/->chan computation))

(deftest node-checkpoint-mode-is-checked-before-source-access
  (async done
         (go
           (try
             (doseq [operation [(fn [] (workspace/resolve-checkpoint nil nil {:sync? true}))
                                (fn [] (workspace/fork-at-checkpoint! nil "unused" nil nil {:sync? true}))]]
               (let [failure (try (bridge/unwrap-result (<! (take-cps (operation)))) nil
                                  (catch :default error error))]
                 (is (= ::workspace/invalid-checkpoint-mode (:type (ex-data failure))))))
             (finally (done))))))

(deftest node-invalid-advance-options-refuse-before-source-access
  (async done
         (go
           (try
             (doseq [options [{:expected-checkpoint nil} {:expected-checkpoint "invalid"}
                              {:expected-checkpont (random-uuid)}]]
               (let [failure (try
                               (bridge/unwrap-result (<! (take-cps (workspace/advance! nil nil options))))
                               nil (catch :default error error))]
                 (is (= ::workspace/invalid-advance-options (:type (ex-data failure))))))
             (finally (done))))))

(deftest node-workspace-fork-and-publish
  (async done
         (go
           (let [cfg {:store {:backend :memory :id (random-uuid)}
                      :schema-flexibility :write
                      :keep-history? true
                      :commit-graph? true}]
             (try
               (<! (d/create-database cfg))
               (let [canonical (<! (d/connect cfg {:sync? false}))]
                 (bridge/unwrap-result (<! (take-cps (repo/init! canonical))))
                 (bridge/unwrap-result
                  (<! (take-cps
                       (repo/write! canonical "base.txt"
                                    (bytes/utf8 "base\n")))))
                 (bridge/unwrap-result
                  (<! (take-cps (repo/stage-all! canonical))))
                 (bridge/unwrap-result
                  (<! (take-cps
                       (repo/commit! canonical {:message "base"}))))
                 (is (= :geschichte.workspace/node-agent
                        (bridge/unwrap-result
                         (<! (take-cps
                              (workspace/fork! canonical "node-agent"))))))
                 (bridge/unwrap-result
                  (<! (take-cps (workspace/fork! canonical "node-observer"))))
                 (let [worker (<! (d/connect
                                   (assoc cfg :branch
                                          :geschichte.workspace/node-agent)
                                   {:sync? false}))]
                   (bridge/unwrap-result
                    (<! (take-cps
                         (repo/write! worker "node.txt"
                                      (bytes/utf8 "node\n")))))
                   (bridge/unwrap-result
                    (<! (take-cps (repo/stage-all! worker))))
                   (let [commit (bridge/unwrap-result
                                 (<! (take-cps
                                      (repo/commit! worker
                                                    {:message "node"}))))
                         published (bridge/unwrap-result
                                    (<! (take-cps
                                         (workspace/publish! canonical worker))))
                         historical (bridge/unwrap-result
                                     (<! (take-cps
                                          (repo/read-at
                                           canonical
                                           (:geschichte.commit/id commit)
                                           "node.txt"))))]
                     (is (= (:geschichte.commit/id commit) (:new published)))
                     (is (= "node\n" (bytes/decode-utf8 historical)))
                     (let [created (bridge/unwrap-result
                                    (<! (take-cps
                                         (workspace/publish! canonical worker
                                                             {:ref "node-review" :create-only? true
                                                              :expected-checkpoint (d/commit-id @canonical)
                                                              :commit (:geschichte.commit/id commit)}))))
                           before @canonical
                           failure (try
                                     (bridge/unwrap-result
                                      (<! (take-cps
                                           (workspace/publish! canonical worker
                                                               {:ref "node-review" :create-only? true
                                                                :commit (:geschichte.commit/id commit)}))))
                                     nil (catch :default error error))]
                       (is (true? (:created? created)))
                       (is (= (:geschichte.commit/id commit) (:new created)))
                       (is (= ::workspace/publication-ref-exists (:type (ex-data failure))))
                       (is (= before @canonical)))
                     (let [observer (<! (d/connect
                                         (assoc cfg :branch :geschichte.workspace/node-observer)
                                         {:sync? false}))]
                       (try
                         (let [expected (d/commit-id @observer)]
                           (bridge/unwrap-result
                            (<! (take-cps (repo/set-config! observer "example.setting" "changed"))))
                           (let [before @observer
                                 failure (try
                                           (bridge/unwrap-result
                                            (<! (take-cps
                                                 (workspace/advance! canonical observer
                                                                     {:expected-checkpoint expected}))))
                                           nil
                                           (catch :default error error))]
                             (is (= ::workspace/stale-advance (:type (ex-data failure))))
                             (is (= expected (:expected-checkpoint (ex-data failure))))
                             (is (= before @observer))))
                         (let [merge-db! d/merge-db!
                               edited (atom nil)
                               failure
                               (with-redefs [d/merge-db!
                                             (fn [conn parents tx]
                                               (go
                                                 (bridge/unwrap-result
                                                  (<! (take-cps (repo/write! observer "base.txt" (bytes/utf8 "concurrent\n")))))
                                                 (reset! edited @observer)
                                                 (<! (merge-db! conn parents tx))))]
                                 (try
                                   (bridge/unwrap-result
                                    (<! (take-cps (workspace/advance! canonical observer))))
                                   nil
                                   (catch :default error error)))]
                           (is (= ::workspace/stale-advance (:type (ex-data failure))))
                           (is (= @edited @observer))
                           ;; Restore the private edit before the successful advance.
                           (bridge/unwrap-result
                            (<! (take-cps (repo/write! observer "base.txt" (bytes/utf8 "base\n"))))))
                         (let [advanced (bridge/unwrap-result
                                         (<! (take-cps (workspace/advance! canonical observer
                                                                           {:expected-checkpoint (d/commit-id @observer)}))))
                               observed (bridge/unwrap-result
                                         (<! (take-cps (repo/read observer "node.txt"))))]
                           (is (= (:geschichte.commit/id commit) (:new advanced)))
                           (is (= "node\n" (bytes/decode-utf8 observed))))
                         (bridge/unwrap-result
                          (<! (take-cps (repo/write! worker "next.txt" (bytes/utf8 "next\n")))))
                         (bridge/unwrap-result (<! (take-cps (repo/stage-all! worker))))
                         (let [next (bridge/unwrap-result
                                     (<! (take-cps (repo/commit! worker {:message "next"}))))
                               merge-db! d/merge-db!
                               injected? (atom false)
                               after-winner (atom nil)
                               failure
                               (with-redefs [d/merge-db!
                                             (fn [conn parents tx]
                                               (go
                                                 (when (compare-and-set! injected? false true)
                                                   (bridge/unwrap-result
                                                    (<! (take-cps
                                                         (workspace/publish! canonical worker
                                                                             {:ref "topic" :create? true
                                                                              :commit (:geschichte.commit/id next)}))))
                                                   (reset! after-winner @canonical))
                                                 (<! (merge-db! conn parents tx))))]
                                 (try
                                   (bridge/unwrap-result
                                    (<! (take-cps
                                         (workspace/publish! canonical observer
                                                             {:ref "topic" :create? true
                                                              :commit (:geschichte.commit/id commit)}))))
                                   nil
                                   (catch :default error error)))]
                           (is (= ::workspace/publication-ref-exists (:type (ex-data failure))))
                           (is (= (:geschichte.commit/id next) (get (repo/refs canonical) "refs/heads/topic")))
                           (is (= @after-winner @canonical))
                           (<! (d/transact! canonical [{:geschichte.ref/name "refs/heads/unborn"}]))
                           (let [after-delete (atom nil)
                                 failure
                                 (with-redefs [d/merge-db!
                                               (fn [conn parents tx]
                                                 (go
                                                   (bridge/unwrap-result
                                                    (<! (take-cps (repo/delete-branch! canonical "unborn" {:force? true}))))
                                                   (reset! after-delete @canonical)
                                                   (<! (merge-db! conn parents tx))))]
                                   (try
                                     (bridge/unwrap-result
                                      (<! (take-cps
                                           (workspace/publish! canonical worker
                                                               {:ref "unborn" :commit (:geschichte.commit/id next)}))))
                                     nil (catch :default error error)))]
                             (is (= ::workspace/stale-publication-ref (:type (ex-data failure))))
                             (is (= @after-delete @canonical))))
                         (finally (d/release observer)))))
                   (d/release worker))
                 (d/release canonical))
               (catch :default error
                 (is false (str "Node workspace test failed: "
                                (.-message error))))
               (finally
                 (<! (d/delete-database cfg))
                 (done)))))))

(deftest node-create-only-force-refuses-before-source-access
  (async done
         (go
           (try
             (let [failure (try
                             (bridge/unwrap-result
                              (<! (take-cps (workspace/publish! nil nil {:create-only? true :force? true}))))
                             nil (catch :default error error))]
               (is (= ::workspace/invalid-publication-options (:type (ex-data failure)))))
             (finally (done))))))

(deftest node-invalid-publication-checkpoint-refuses-before-source-access
  (async done
         (go
           (try
             (doseq [value [nil "invalid"]]
               (let [failure (try
                               (bridge/unwrap-result
                                (<! (take-cps (workspace/publish! nil nil {:expected-checkpoint value}))))
                               nil (catch :default error error))]
                 (is (= ::workspace/invalid-publication-options (:type (ex-data failure))))))
             (finally (done))))))
