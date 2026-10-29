(ns geschichte.budget-content-node-test
  (:require [cljs.core.async :as core-async :refer [<!] :refer-macros [go]]
            [cljs.test :refer-macros [async deftest is]]
            [geschichte.budget :as budget]
            [geschichte.bytes :as bytes]
            [geschichte.merge.policy :as policy]
            [geschichte.store.blob :as payload]
            [is.simm.partial-cps.core-async :as bridge]
            [konserve.core :as k]
            [konserve.memory :as memory]
            [konserve.node-filestore :as filestore]
            ["fs" :as fs]
            ["os" :as os]
            ["stream/consumers" :as consumers]))

(defn- unwrap [result] (bridge/unwrap-result result))
(defn- refusal [result]
  (try (unwrap result) nil (catch :default error (ex-data error))))

(defn- raw-read [store id]
  (k/bget store id
          (fn [{:keys [input-stream]}]
            (let [out (core-async/promise-chan)]
              (.then (.buffer consumers input-stream)
                     #(core-async/put! out %)
                     #(core-async/put! out %))
              out))
          {:sync? false}))

(deftest borrowed-node-stream-refusal-preserves-owner-cleanup
  (async done
         (go
           (let [path (.mkdtempSync fs (str (.tmpdir os) "/geschichte-budget-node-"))]
             (try
               (let [store (<! (filestore/connect-fs-store path :opts {:sync? false}))
                     conn (atom {:store store})
                     id (random-uuid)
                     value (bytes/from-values (take 100000 (cycle (range 251))))]
                 (<! (k/bassoc store id value {:sync? false}))
                 (doseq [[operation expected]
                         [[(budget/create (policy/recommended-policy)) ::budget/unsupported]
                          [(budget/create (assoc (policy/recommended-policy) :cancelled? (constantly true)))
                           ::budget/cancelled]]]
                   (let [result (<! (bridge/->chan (payload/get-bytes conn id {:merge-budget operation})))]
                     (is (= expected (:type (refusal result))))
                ;; Real owner closes its borrowed FD and releases key lock before
                ;; bget returns. No destroy/pause/read/drain is borrowed here.
                     (is (bytes/same-bytes? value (<! (raw-read store id)))))
                   (<! (k/bassoc store id value {:sync? false})))
                 (is (bytes/same-bytes? value (<! (raw-read store id)))))
               (catch :default error (is false (str "Node filestore lifecycle: " error)))
               (finally (.rmSync fs path #js {:recursive true :force true}) (done)))))))

(deftest node-memory-buffers-are-accounted-before-retention
  (async done
         (go
           (try
             (let [store (<! (memory/new-mem-store)) conn (atom {:store store})
                   id (random-uuid) value (bytes/utf8 "abcdef")
                   b (budget/create (policy/recommended-policy))]
               (<! (k/bassoc store id value {:sync? false}))
               (is (bytes/same-bytes? value (unwrap (<! (bridge/->chan (payload/get-bytes conn id {:merge-budget b}))))))
               (is (= 6 (get-in (budget/snapshot b) [:used :content-read-bytes])))
               (let [small (budget/create (update (policy/recommended-policy) :limits assoc :object-bytes 2))]
                 (is (= ::budget/exceeded
                        (:type (refusal (<! (bridge/->chan (payload/get-bytes conn id {:merge-budget small})))))))))
             (catch :default error (is false (str "Node memory budget: " error)))
             (finally (done))))))
