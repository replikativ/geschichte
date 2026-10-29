(ns geschichte.fs-test
  (:refer-clojure :exclude [read])
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [geschichte.fs :as fs]
            [geschichte.repo :as repo]))

(defn- connection []
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write
             :keep-history? true}]
    (d/create-database cfg)
    (d/connect cfg)))

(deftest virtual-filesystem-operations
  (let [conn (connection)]
    (repo/init! conn)
    (testing "empty and implicit directories"
      (is (= "src" (fs/mkdir! conn "/src")))
      (is (= :dir (:type (fs/stat conn "src"))))
      (fs/mkdir! conn "src/demo")
      (fs/write! conn "src/demo/core.clj" (.getBytes "(ns demo.core)\n" "UTF-8"))
      (is (= #{["src" :dir]}
             (set (map (juxt :name :type) (fs/list-dir conn "/")))))
      (is (= #{["demo" :dir]}
             (set (map (juxt :name :type) (fs/list-dir conn "src")))))
      (is (= "(ns demo.core)\n"
             (String. ^bytes (fs/read conn "/src/demo/core.clj") "UTF-8"))))
    (testing "rename retains content identity"
      (let [before (:content (fs/stat conn "src/demo/core.clj"))]
        (fs/rename! conn "src/demo" "src/lib")
        (is (nil? (fs/stat conn "src/demo/core.clj")))
        (is (= before (:content (fs/stat conn "src/lib/core.clj"))))))
    (testing "non-empty directories are protected"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not empty"
                            (fs/delete! conn "src/lib")))
      (is (true? (fs/delete! conn "src/lib/core.clj")))
      (is (true? (fs/delete! conn "src/lib")))
      (is (= [] (fs/list-dir conn "src"))))))

(deftest paths-are-clamped
  (let [conn (connection)]
    (repo/init! conn)
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"escapes"
                          (fs/write! conn "../secret" (byte-array 0))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"does not exist"
                          (fs/mkdir! conn "missing/child")))))

(deftest inferred-parents-and-file-collisions
  (let [conn (connection)]
    (try
      (repo/init! conn)
      (fs/write! conn "derived/deep/file" (.getBytes "value" "UTF-8"))
      (is (= :dir (:type (fs/stat conn "derived"))))
      (is (empty? (d/q '[:find ?p :where [_ :geschichte.work-dir/path ?p]] @conn)))
      (fs/delete! conn "derived/deep/file")
      (is (nil? (fs/stat conn "derived")))
      (fs/write! conn "derived" (.getBytes "file now" "UTF-8"))
      (let [before @conn]
        (is (thrown? Exception (fs/write! conn "derived/child" (byte-array 0))))
        (is (thrown? Exception (fs/mkdir! conn (str "bad" (char 0) "dir"))))
        (is (thrown? Exception (fs/rename! conn "derived" (str "bad" (char 0) "file"))))
        (is (= before @conn)))
      (fs/mkdir! conn "authored")
      (fs/write! conn "authored/file" (byte-array 0))
      (fs/delete! conn "authored/file")
      (is (= :dir (:type (fs/stat conn "authored"))))
      (fs/write! conn "parent/seed" (byte-array 0))
      (fs/mkdir! conn "parent/kept")
      (fs/delete! conn "parent/seed")
      (is (= :dir (:type (fs/stat conn "parent"))))
      (is (= ["kept"] (mapv :name (fs/list-dir conn "parent"))))
      (fs/delete! conn "parent/kept")
      (is (nil? (fs/stat conn "parent")))
      (finally (let [cfg (:config @conn)] (d/release conn) (d/delete-database cfg))))))
