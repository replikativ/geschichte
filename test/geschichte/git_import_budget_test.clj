(ns geschichte.git-import-budget-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [geschichte.git.budget :as budget]
            [geschichte.git.checkout :as checkout]
            [geschichte.git.client :as client]
            [geschichte.git.http :as http]
            [geschichte.git.object :as object]
            [geschichte.git.pack :as pack]
            [geschichte.git.pack-read :as pack-read]
            [geschichte.git.pack-source :as pack-source]
            [geschichte.git.pkt-line :as pkt]
            [geschichte.git.ssh :as ssh]
            [geschichte.git.store :as store]
            [geschichte.repo :as repo])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream]
           [java.nio ByteBuffer ByteOrder]
           [java.security MessageDigest]
           [java.util.zip DeflaterOutputStream]))

(defn- refusal [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defmacro with-repo [[conn] & body]
  `(let [cfg# {:store {:backend :memory :id (random-uuid)}
               :schema-flexibility :write :keep-history? true :commit-graph? true}]
     (d/create-database cfg#)
     (let [~conn (d/connect cfg#)]
       (try (repo/init! ~conn) ~@body
            (finally (d/release ~conn) (d/delete-database cfg#))))))

(defn- commit-graph [tree objects]
  (let [tree-oid (object/object-id :tree tree)
        commit (object/commit-payload {:tree tree-oid :author "T <t@example.test> 0 +0000"
                                       :committer "T <t@example.test> 0 +0000" :message "fixture"})
        oid (object/object-id :commit commit)]
    {:oid oid :commits {}
     :objects (assoc objects tree-oid {:type :tree :payload tree}
                     oid {:type :commit :payload commit})}))

(defn- repeated-tree-graph []
  (let [bytes (object/utf8 "payload") blob (object/object-id :blob bytes)
        tree (object/tree-payload [{:name "file.md" :mode "100644" :oid blob}])
        tree-oid (object/object-id :tree tree)
        root (object/tree-payload (mapv #(hash-map :name (str "branch" %) :mode "40000" :oid tree-oid)
                                        (range 4)))]
    (commit-graph root {blob {:type :blob :payload bytes} tree-oid {:type :tree :payload tree}})))

(defn- int32 [value]
  (-> (ByteBuffer/allocate 4) (.order ByteOrder/BIG_ENDIAN) (.putInt (int value)) .array))

(defn- thin-pack [base-oid payload]
  (let [out (ByteArrayOutputStream.)
        _ (with-open [z (DeflaterOutputStream. out)] (.write z ^bytes payload))
        ;; Small fixture delta (<16 bytes) needs just one entry header byte.
        body (object/concat-bytes (object/utf8 "PACK") (int32 2) (int32 1)
                                  (byte-array [(unchecked-byte (bit-or 0x70 (alength ^bytes payload)))])
                                  (object/hex->bytes base-oid) (.toByteArray out))]
    (object/concat-bytes body (.digest (MessageDigest/getInstance "SHA-1") body))))

(deftest declared-and-actual-inflation-limits-cover-both-scanners
  (let [payload (byte-array (* 256 1024)) oid (object/object-id :blob payload)
        packed (pack/encode {oid {:type :blob :payload payload}})]
    (doseq [threshold [0 Integer/MAX_VALUE]
            [metric cap] [[:objects 0] [:object-bytes 1024] [:inflated-bytes 1024] [:index-bytes 1]]]
      (let [b (budget/create {:limits {metric cap}})
            error (refusal #(pack-read/scan (pack-source/byte-array-source packed)
                                            {:primitive-index-threshold threshold :import-budget b}))]
        (is (= ::budget/exceeded (:type error)))
        (is (= metric (:metric error)))))))

(deftest declared-delta-target-is-refused-before-output-allocation
  (let [b (budget/create {:limits {:delta-result-bytes 32}})
        ;; Empty base, target 1024, deliberately no instructions. Budget refusal
        ;; must precede allocating the target and the eventual size mismatch.
        error (refusal #(pack-read/apply-delta (byte-array 0)
                                               (byte-array [0 (unchecked-byte 0x80) 8]) b))]
    (is (= ::budget/exceeded (:type error)))
    (is (= :delta-result-bytes (:metric error))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"exceeds declared size"
                        (pack-read/apply-delta (byte-array 0) (byte-array [0 0 1 42]))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"signed-long"
                        (pack-read/apply-delta (byte-array 0) (byte-array (repeat 12 (unchecked-byte 0x80)))))))

(deftest ingestion-refusal-never-publishes-membership-or-tracking-refs
  (doseq [limits [{:pack-bytes 8} {:inflated-bytes 2} {:objects 0}]]
    (with-repo [conn]
      (let [bytes (object/utf8 "larger than budget") oid (object/object-id :blob bytes)
            packed (pack/encode {oid {:type :blob :payload bytes}})
            before @conn
            error (refusal #(store/import-pack! conn packed
                                                {:import-budget (budget/create {:limits limits})
                                                 :remote "origin" :refs [{:oid oid :ref "refs/heads/main"}]}))]
        (is (= ::budget/exceeded (:type error)))
        (is (= before @conn))
        (is (nil? (store/object conn oid)))))))

(deftest repeated-tree-expansion-and-output-are-counted-per-path
  (doseq [[metric cap] [[:tree-entries 3] [:tree-depth 0] [:checkout-files 1] [:checkout-bytes 7]]]
    (with-repo [conn]
      (let [graph (repeated-tree-graph)]
        (store/persist-graph! conn graph)
        (let [head (repo/current-ref conn)
              error (refusal #(checkout/materialize! conn (:oid graph)
                                                     {:import-budget (budget/create {:limits {metric cap}})}))]
          (is (= ::budget/exceeded (:type error)))
          (is (= metric (:metric error)))
          ;; Preparation may contain partial work, but no successful selected
          ;; logical commit/ref is advertised, and no live target is involved.
          (is (= head (repo/current-ref conn)))
          (is (nil? (repo/head-commit conn))))))))

(deftest lazy-reconstruction-cannot-borrow-an-unbudgeted-cache-hit
  (with-repo [conn]
    (let [bytes (object/utf8 "cached blob") oid (object/object-id :blob bytes)]
      (store/import-pack! conn (pack/encode {oid {:type :blob :payload bytes}}))
      (with-open [objects (store/open conn)]
        (is (= (seq bytes) (seq (:payload (store/read-object objects oid)))))
        (let [b (budget/create {:limits {:inflated-bytes (alength bytes)}})]
          (store/read-object objects oid {:import-budget b})
          (is (= ::budget/exceeded
                 (:type (refusal #(store/read-object objects oid {:import-budget b}))))))
        (is (= ::budget/exceeded
               (:type (refusal #(store/read-object objects oid
                                                   {:import-budget (budget/create {:limits {:object-bytes 0}})})))))))))

(deftest external-delta-depth-does-not-reset-between-packs
  (doseq [threshold [0 Integer/MAX_VALUE]]
    (with-repo [conn]
      (let [a (object/utf8 "a") a-oid (object/object-id :blob a)
            b (object/utf8 "ab") b-oid (object/object-id :blob b)
            delta1 (byte-array [1 2 (unchecked-byte 0x90) 1 1 98])
            delta2 (byte-array [2 3 (unchecked-byte 0x90) 2 1 99])]
        (store/import-pack! conn (pack/encode {a-oid {:type :blob :payload a}}))
        (store/import-pack! conn (thin-pack a-oid delta1))
        (let [before @conn
              error (refusal #(store/import-pack! conn (thin-pack b-oid delta2)
                                                  {:primitive-index-threshold threshold
                                                   :import-budget (budget/create {:limits {:delta-depth 1}})}))]
          (is (= ::budget/exceeded (:type error)))
          (is (= :delta-depth (:metric error)))
          (is (= before @conn)))))))

(deftest cancellation-happens-before-publication
  (with-repo [conn]
    (let [bytes (object/utf8 "blob") oid (object/object-id :blob bytes)
          packed (pack/encode {oid {:type :blob :payload bytes}})
          cancelled (atom false) before @conn
          error (refusal #(store/import-pack-producer!
                           conn (fn [write!] (write! packed) (reset! cancelled true))
                           {:import-budget (budget/create {:cancelled? (fn [] @cancelled)})}))]
      (is (= ::budget/cancelled (:type error)))
      (is (= before @conn)))))

(deftest http-budget-includes-discovery-and-owns-all-response-bodies
  (doseq [status [200 500]
          type ["text/html" "application/x-git-upload-pack-advertisement"]]
    (let [closed (atom false)
          body (proxy [ByteArrayInputStream] [(byte-array 32)] (close [] (reset! closed true)))
          send (fn [request]
                 (is (pos? (:timeout-ms request)))
                 {:status status :headers {"content-type" type} :body body})
          b (budget/create {:timeout-ms 10000 :limits {:response-bytes 4}})]
      (is (some? (refusal #(http/advertise! send "https://example.test/repo" "git-upload-pack"
                                            {:import-budget b}))))
      (is @closed)))
  (let [bytes (pkt/encode ["version 2\n" "ls-refs\n" :flush])
        b (budget/create {:limits {:response-bytes (alength bytes)}})
        send (fn [_] {:status 200 :body bytes :headers {}})]
    (http/advertise! send "https://example.test/repo" "git-upload-pack" {:import-budget b})
    (is (= ::budget/exceeded
           (:type (refusal #(http/advertise! send "https://example.test/repo" "git-upload-pack"
                                             {:import-budget b})))))))

(deftest budgeted-process-transport-refuses-before-launch
  (let [launched (atom false)]
    (is (= ::ssh/unsupported-import-budget
           (:type (refusal #(ssh/fetch! nil "origin" (fn [_] (reset! launched true) [])
                                        {:import-budget (budget/create {})})))))
    (is (false? @launched))))

(deftest https-clone-shares-one-budget-through-discovery-and-checkout
  (with-repo [conn]
    (let [graph (repeated-tree-graph)
          advertisement (pkt/encode ["version 2\n" "ls-refs=unborn\n" "fetch\n" :flush])
          refs (pkt/encode [(str (:oid graph) " HEAD symref-target:refs/heads/main\n")
                            (str (:oid graph) " refs/heads/main\n") :flush])
          fetched (pkt/encode ["packfile\n"
                               (object/concat-bytes (byte-array [1]) (pack/encode (:objects graph))) :flush])
          requests (atom [])
          send (fn [{:keys [method body] :as request}]
                 (swap! requests conj request)
                 {:status 200 :headers {}
                  :body (cond (= :get method) advertisement
                              (= "command=ls-refs\n" (pkt/text (first (pkt/decode body)))) refs
                              :else fetched)})
          b (budget/create {:timeout-ms 30000 :limits {:checkout-files 4}})
          result (http/clone! send conn "origin" "https://example.test/repo"
                              {:import-budget b})
          used (:used (budget/snapshot b))]
      (is (= 4 (:files result)))
      (is (= 3 (count @requests)))
      (is (every? #(pos? (:timeout-ms %)) @requests))
      ;; HTTP/client stream wrappers must not double-charge the pack response.
      (is (= (+ (alength advertisement) (alength refs) (alength fetched)) (:response-bytes used)))
      (is (= 4 (:checkout-files used)))
      (is (= 8 (:tree-entries used)))
      (is (= 28 (:checkout-bytes used)))
      (is (= 4 (count (repo/files conn)))))))

(deftest initialized-and-fast-forward-pull-forward-the-original-budget
  (doseq [initialized? [false true]]
    (with-repo [conn]
      (let [old (commit-graph (object/tree-payload []) {})
            graph (repeated-tree-graph)
            tree (:tree (object/parse-commit (get-in graph [:objects (:oid graph) :payload])))
            commit (object/commit-payload {:tree tree :parents [(:oid old)]
                                           :author "T <t@example.test> 1 +0000"
                                           :committer "T <t@example.test> 1 +0000" :message "new"})
            oid (object/object-id :commit commit)
            combined {:oid oid :commits {} :objects (assoc (merge (:objects old) (:objects graph))
                                                           oid {:type :commit :payload commit})}]
        (store/persist-graph! conn combined)
        (when initialized? (checkout/materialize! conn (:oid old)))
        (let [before-head (:geschichte.commit/id (repo/head-commit conn))
              b (budget/create {:limits {:checkout-files 0}})
              error (refusal #(client/apply-pull! conn "origin" {:refs [{:ref "HEAD" :oid oid}]}
                                                  {:import-budget b}))]
          (is (= ::budget/exceeded (:type error)))
          (is (= :checkout-files (:metric error)))
          (is (= before-head (:geschichte.commit/id (repo/head-commit conn)))))))))

(deftest late-cancellation-refuses-membership-after-index-preparation
  (with-repo [conn]
    (let [bytes (object/utf8 "blob") oid (object/object-id :blob bytes)
          cancelled (atom false) before @conn
          error (refusal #(store/import-pack!
                           conn (pack/encode {oid {:type :blob :payload bytes}})
                           {:import-budget (budget/create {:cancelled? (fn [] @cancelled)})
                            :phase-fn (fn [{:keys [event phase]}]
                                        (when (and (= :start event) (= :publish-pack phase))
                                          (reset! cancelled true)))}))]
      (is (= ::budget/cancelled (:type error)))
      (is (= before @conn))
      (is (nil? (store/object conn oid))))))

(deftest failed-decoder-settles-siblings-before-shared-storage-release
  (let [active (promise) finish (promise) released (atom false)
        slow (future (deliver active true) @finish :complete)
        failed (future @active (throw (ex-info "decoder failed" {:type ::decoder-failure})))
        owner (future
                (try
                  ((ns-resolve 'geschichte.git.pack-read 'worker-values) [failed slow])
                  (catch clojure.lang.ExceptionInfo error (ex-data error))
                  (finally (reset! released true))))]
    (try
      @active
      ;; The first decoder has failed, but the second still owns the resource.
      (is (= ::waiting (deref owner 50 ::waiting)))
      (is (false? @released))
      (deliver finish true)
      (is (= ::decoder-failure (:type (deref owner 1000 {:stalled true}))))
      (is @released)
      (finally (deliver finish true) @slow))))
