(ns geschichte.bounded-blob-test
  (:require [clojure.test :refer [deftest is]]
            [geschichte.budget :as budget]
            [geschichte.bytes :as bytes]
            [geschichte.merge.policy :as policy]
            [geschichte.store.blob :as payload]
            [konserve.core :as k]
            [konserve.filestore :as filestore])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- refusal [f] (try (f) nil (catch Exception error (ex-data error))))

(deftest jvm-file-read-refusal-releases-backend-lock-and-stream
  (let [path (.toFile (Files/createTempDirectory "geschichte-budget-file" (make-array FileAttribute 0)))
        store (filestore/connect-fs-store (.getPath path) :opts {:sync? true})
        conn (atom {:store store}) id (random-uuid)
        value (bytes/from-values (take 100000 (cycle (range 251))))]
    (try
      (k/bassoc store id value {:sync? true})
      (let [b (budget/create (policy/recommended-policy))]
        (is (bytes/same-bytes? value (payload/get-bytes conn id {:merge-budget b})))
        (is (= 100000 (get-in (budget/snapshot b) [:used :content-read-bytes]))))
      (doseq [[options expected]
              [[(update (policy/recommended-policy) :limits assoc :object-bytes 10) ::budget/exceeded]
               [(assoc (policy/recommended-policy) :cancelled? (constantly true)) ::budget/cancelled]]]
        (is (= expected (:type (refusal #(payload/get-bytes conn id {:merge-budget (budget/create options)})))))
        (is (bytes/same-bytes? value (payload/get-bytes conn id)))
        (k/bassoc store id value {:sync? true}))
      (finally
        (doseq [file (reverse (file-seq path))] (.delete file))))))
