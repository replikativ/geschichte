(ns geschichte.ignore-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.bytes :as bytes]
            [geschichte.ignore :as ignore]
            [geschichte.repo :as repo]))

(defn- repository [files]
  (let [config {:store {:backend :memory :id (random-uuid)}
                :schema-flexibility :write}]
    (d/create-database config)
    (let [conn (d/connect config)]
      (repo/init! conn)
      (doseq [[path content] files]
        (repo/write! conn path (bytes/utf8 content)))
      {:conn conn :close! #(do (d/release conn) (d/delete-database config))})))

(deftest root-and-nested-ignore-rules
  (let [{:keys [conn close!]}
        (repository {".gitignore" "*.log\ntarget/\n!important.log\n/build\n"
                     "src/.gitignore" "generated/**\n!generated/keep.clj\n"})]
    (try
      (let [rules (ignore/rules conn)]
        (is (ignore/ignored? rules "debug.log"))
        (is (ignore/ignored? rules "nested/debug.log"))
        (is (not (ignore/ignored? rules "important.log")))
        (is (ignore/ignored? rules "target/classes/a.class"))
        (is (ignore/ignored? rules "build/output.js"))
        (is (not (ignore/ignored? rules "nested/build/output.js")))
        (is (ignore/ignored? rules "src/generated/a.clj"))
        (is (not (ignore/ignored? rules "src/generated/keep.clj"))))
      (finally (close!)))))
