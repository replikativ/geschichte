(ns geschichte.historical-source-fixture
  (:require [clojure.test :refer [is]]
            [datahike.api :as d]
            [geschichte.bytes :as bytes]
            [geschichte.content :as content]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [geschichte.workspace :as workspace]))

(defn commit! [conn message]
  (repo/stage-all! conn)
  (:geschichte.commit/id (repo/commit! conn {:message message})))

(defn state [conn]
  {:work (repo/worktree conn) :stage (query/stage @conn)
   :refs (repo/refs conn) :head (repo/current-ref conn)
   :merge-parent (:geschichte.repo/merge-parent (query/repository @conn))})

(defn with-archive
  "Natural selected-tip publication leaves C1 and its representation historical."
  ([kind f] (with-archive kind {} f))
  ([kind options f]
   (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
              :keep-history? true :commit-graph? true}]
     (d/create-database cfg)
     (let [canonical (d/connect cfg)]
       (try
         (repo/init! canonical)
         (repo/write! canonical "base.txt" (bytes/utf8 "base\n"))
         (let [base (commit! canonical "base")]
           (workspace/fork! canonical "worker")
           (workspace/fork! canonical "observer")
           (let [worker (d/connect (assoc cfg :branch :geschichte.workspace/worker))
                 observer (d/connect (assoc cfg :branch :geschichte.workspace/observer))]
             (try
               (let [original (or (:original options) (apply str (map #(str "unchanged line " % " padding\n") (range 300))))
                     text (or (:text options) (.replace original "line 150" "changed line 150"))
                     base-content (:content (repo/write! worker "old.txt" (bytes/utf8 original)))
                     id (:content (repo/write! worker "old.txt" (bytes/utf8 text)
                                               (case kind
                                                 :chunks {:chunk-threshold 32 :chunk-min-size 64 :chunk-size 128 :chunk-max-size 256}
                                                 :full {:delta-ratio 0}
                                                 nil)))
                     c1 (commit! worker "add old")]
                 (is (= kind (:geschichte.content/kind (content/info worker id))))
                 (repo/remove! worker "old.txt")
                 (repo/write! worker (or (:tip-path options) "tip.txt") (bytes/utf8 (or (:tip-text options) "tip\n")) {:delta-ratio 0})
                 (let [c2 (commit! worker "replace old")]
                   (workspace/publish! canonical worker)
                   (workspace/advance! canonical observer {:expected-checkpoint (d/commit-id @observer)})
                   (repo/create-ref! observer "archive" (repo/commit-by-id observer c1))
                   (is (= text (bytes/decode-utf8-strict (repo/read-at observer c1 "old.txt"))))
                   (doseq [missing [id base-content]]
                     (is (nil? (d/q '[:find ?e . :in $ ?id :where [?e :geschichte.content/id ?id]] @observer missing))))
                   (f {:conn observer :canonical canonical :worker worker :cfg cfg
                       :base base :c1 c1 :c2 c2 :id id :text text
                       :tip-path (or (:tip-path options) "tip.txt") :tip-text (or (:tip-text options) "tip\n")})))
               (finally (d/release observer) (d/release worker)))))
         (finally (d/release canonical) (d/delete-database cfg)))))))
