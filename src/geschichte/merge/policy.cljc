(ns geschichte.merge.policy
  "Finite resource profile for the opt-in bounded native merge planner."
  (:require [geschichte.budget :as budget]))

(def recommended-limits
  {:commit-visits 16384 :graph-edges 65536
   :tree-bytes 67108864 :result-tree-bytes 33554432
   :tree-entries 8192 :tree-path-bytes 4194304 :path-bytes 4096
   :content-visits 16384 :content-parts 65536
   :content-read-bytes 67108864 :content-decoded-bytes 134217728
   :object-bytes 8388608 :content-depth 16
   :text-lines 200000 :text-work 64000000 :text-trace-cells 8000000
   :text-frontier-cells 400003 :delta-forms 200000
   :delta-edn-depth 32 :delta-token-chars 128 :merge-output-bytes 33554432})

(defn profile []
  {:id :geschichte.merge/bounded-plan :version 1
   :required-limits (vec (sort (keys recommended-limits)))
   :deadline-required? true})

(defn recommended-policy
  "A canonical finite baseline. Merge these limits into the same import policy
  before constructing its operation; this never creates or resets a handle."
  ([] (recommended-policy 30000))
  ([timeout-ms] {:limits recommended-limits :timeout-ms timeout-ms}))

(defn require! [operation]
  (budget/require-limits! operation (:required-limits (profile))))
