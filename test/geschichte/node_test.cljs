(ns geschichte.node-test
  (:require [cljs.test :as test]
            [geschichte.async-node-test]
            [geschichte.chunk-test]
            [geschichte.content-node-test]
            [geschichte.diff-test]
            [geschichte.fs-node-test]
            [geschichte.ignore-node-test]
            [geschichte.git-object-test]
            [geschichte.git-pack-index-test]
            [geschichte.git-pkt-line-test]
            [geschichte.git-wire-test]
            [geschichte.merge-core-test]
            [geschichte.query-node-test]
            [geschichte.repo-node-test]
            [geschichte.workspace-node-test]))

(defmethod test/report [::test/default :end-run-tests] [summary]
  (test/inc-report-counter! :test)
  (println (str "Node CLJS: " (:test summary) " tests, "
                (:pass summary) " passes, " (:fail summary) " failures, "
                (:error summary) " errors"))
  (.exit js/process (if (test/successful? summary) 0 1)))

(defn -main [& _]
  (test/run-tests 'geschichte.async-node-test
                  'geschichte.chunk-test
                  'geschichte.content-node-test
                  'geschichte.diff-test
                  'geschichte.fs-node-test
                  'geschichte.ignore-node-test
                  'geschichte.git-object-test
                  'geschichte.git-pack-index-test
                  'geschichte.git-pkt-line-test
                  'geschichte.git-wire-test
                  'geschichte.merge-core-test
                  'geschichte.query-node-test
                  'geschichte.repo-node-test
                  'geschichte.workspace-node-test))
