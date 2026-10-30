(ns geschichte.historical-retention-test
  (:require [clojure.core.async :as async]
            [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.bytes :as bytes]
            [geschichte.content :as content]
            [geschichte.git.object :as object]
            [geschichte.git.pack :as pack]
            [geschichte.git.store :as store]
            [geschichte.historical-source-fixture :as fixture]
            [geschichte.repo :as repo]
            [geschichte.workspace :as workspace]))

(deftest published-historical-bytes-survive-source-removal-reconnect-and-default-gc
  (doseq [kind [:full :line-delta :chunks]]
    (let [id (random-uuid)
          cfg {:store {:backend :file :path (str "/tmp/geschichte-retention-" id) :id id}
               :schema-flexibility :write :keep-history? true :commit-graph? true}
          retained (atom nil)]
      (try
        (d/create-database cfg)
        (let [canonical (d/connect cfg)]
          (try
            (repo/init! canonical)
            (repo/write! canonical "base" (bytes/utf8 "base\n"))
            (fixture/commit! canonical "base")
            (workspace/fork! canonical "publisher")
            (let [worker (d/connect (assoc cfg :branch :geschichte.workspace/publisher))]
              (try
                (let [git-value (object/utf8 "retained exact packed bytes")
                      git-oid (object/object-id :blob git-value)
                      _ (store/import-pack! worker (pack/encode {git-oid {:type :blob :payload git-value}}))
                      base (apply str (repeat 300 "retained original line with padding\n"))
                      text (str base "changed tail\n")
                      base-id (:content (repo/write! worker "old" (bytes/utf8 base)))
                      selected (:content (repo/write! worker "old" (bytes/utf8 text)
                                                      (case kind :full {:delta-ratio 0}
                                                            :chunks {:chunk-threshold 1 :chunk-min-size 64 :chunk-size 128 :chunk-max-size 256}
                                                            nil)))
                      c1 (fixture/commit! worker "historical")]
                  (is (= kind (:geschichte.content/kind (content/info worker selected))))
                  (repo/remove! worker "old")
                  (repo/write! worker "tip" (bytes/utf8 "tip\n"))
                  (fixture/commit! worker "tip")
                  (workspace/publish! canonical worker)
                  (reset! retained {:c1 c1 :id selected :base-id base-id :text text :base base :git-oid git-oid :git-value git-value}))
                (finally (d/release worker))))
            (workspace/remove! canonical "publisher")
            (finally (d/release canonical))))
        ;; All source handles are closed before collection/reconstruction.
        (let [conn (d/connect cfg)]
          (try
            (let [result (async/<!! (d/gc-storage conn (java.util.Date. 0) {:min-age-ms 0}))]
              (is (not (instance? Throwable result))))
            (finally (d/release conn))))
        (let [conn (d/connect cfg) {:keys [c1 id base-id text base git-oid git-value]} @retained]
          (try
            (is (= text (bytes/decode-utf8-strict (repo/read-at conn c1 "old"))))
            (let [view (repo/source-view conn c1)]
              (is (= id (get-in view [:tree "old" :content])))
              (let [exact (store/read-object (atom (:db view)) git-oid)]
                (is (= :blob (:geschichte.git.object/type exact)))
                (is (= (seq git-value) (seq (:payload exact))))
                (is (= git-oid (object/object-id :blob (:payload exact)))))
              (is (= base (bytes/decode-utf8-strict (content/read-by-id (atom (:db view)) base-id)))))
            (finally (d/release conn))))
        ;; Characterize the unsupported pruning policy explicitly. A logical
        ;; snapshot UUID is not a physical GC root in Datahike 0.8.1910.
        (let [conn (d/connect cfg)]
          (try
            (async/<!! (d/gc-storage conn (java.util.Date. Long/MAX_VALUE) {:min-age-ms 0}))
            (finally (d/release conn))))
        (let [conn (d/connect cfg)]
          (try
            (is (thrown? Exception (repo/read-at conn (:c1 @retained) "old")))
            (is (some? (repo/commit-by-id conn (:c1 @retained))))
            (finally (d/release conn))))
        (finally (when (d/database-exists? cfg) (d/delete-database cfg)))))))
