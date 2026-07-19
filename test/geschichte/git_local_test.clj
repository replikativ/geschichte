(ns geschichte.git-local-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [geschichte.git.local :as local]
            [geschichte.git.store :as store]
            [geschichte.repo :as repo])
  (:import [java.io File]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temporary-directory [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- git! [^File directory & args]
  (let [command (into ["git"] args)
        process (-> (ProcessBuilder. ^java.util.List command)
                    (.directory directory)
                    (.redirectErrorStream true)
                    .start)
        output (slurp (.getInputStream process))
        exit (.waitFor process)]
    (when-not (zero? exit)
      (throw (ex-info "native Git fixture command failed"
                      {:command command :directory (.getPath directory)
                       :exit exit :output output})))
    output))

(defn- native-repository
  ([] (native-repository :sha1))
  ([object-format]
   (let [parent (temporary-directory "geschichte-native-source-")
         root (File. parent "source")]
     (git! parent "init" "-q" "-b" "main"
           (str "--object-format=" (name object-format)) (.getPath root))
     (git! root "config" "user.name" "Ada")
     (git! root "config" "user.email" "ada@example.test")
     (git! root "remote" "add" "upstream" "https://example.test/upstream.git")
     (spit (File. root "README.md") "one\n")
     (git! root "add" "README.md")
     (git! root "commit" "-q" "-m" "one")
     (spit (File. root "README.md") "two\n")
     (git! root "commit" "-q" "-am" "two")
     root)))

(defn- database []
  (let [config {:store {:backend :memory :id (random-uuid)}
                :schema-flexibility :write
                :keep-history? true
                :commit-graph? true}]
    (d/create-database config)
    (let [conn (d/connect config)]
      (repo/init! conn)
      {:config config :conn conn})))

(defn- close! [{:keys [config conn]}]
  (d/release conn)
  (d/delete-database config))

(deftest imports-loose-history-refs-and-useful-config
  (let [source (native-repository)
        {:keys [conn] :as target} (database)]
    (try
      (git! source "branch" "feature" "HEAD~1")
      (git! source "tag" "v1" "HEAD~1")
      (let [result (local/import! conn source {:remote "origin"})]
        (is (= :sha1 (:object-format result)))
        (is (pos? (:loose-objects result)))
        (is (= "two\n" (String. ^bytes (repo/read conn "README.md") "UTF-8")))
        (is (= ["two" "one"] (mapv :geschichte.commit/message (repo/log conn))))
        (is (= "https://example.test/upstream.git"
               (get (repo/configuration conn) "remote.upstream.url")))
        (is (contains? (store/refs conn) "refs/remotes/origin/main"))
        (is (contains? (store/refs conn) "refs/remotes/origin/feature"))
        (is (contains? (store/refs conn) "refs/tags/v1")))
      (finally (close! target)))))

(deftest imports-an-unborn-native-repository
  (let [parent (temporary-directory "geschichte-empty-native-")
        source (File. parent "empty")
        {:keys [conn] :as target} (database)]
    (try
      (git! parent "init" "-q" "-b" "trunk" (.getPath source))
      (let [result (local/import! conn source {:remote "origin"})]
        (is (true? (get-in result [:checkout :empty?])))
        (is (= "refs/heads/trunk" (repo/current-ref conn)))
        (is (empty? (repo/files conn))))
      (finally (close! target)))))

(deftest no-checkout-import-publishes-exact-objects-without-logical-materialization
  (let [source (native-repository)
        {:keys [conn] :as target} (database)]
    (try
      (let [result (local/import! conn source {:remote "origin"
                                               :clone? true
                                               :no-checkout? true})]
        (is (true? (get-in result [:checkout :skipped?])))
        (is (empty? (repo/files conn)))
        (is (empty? (repo/log conn)))
        (is (contains? (store/refs conn) "refs/remotes/origin/main")))
      (finally (close! target)))))

(deftest imports-packed-sha1-and-sha256-repositories
  (doseq [format [:sha1 :sha256]]
    (testing (name format)
      (let [source (native-repository format)
            {:keys [conn] :as target} (database)]
        (try
          ;; Give Git enough similar content to produce a real OFS_DELTA chain;
          ;; the compact scanner is forced below so this is a regression test
          ;; for dependency-lifetime payload retention, not only envelopes.
          (dotimes [revision 12]
            (spit (File. source "delta.txt")
                  (apply str
                         (for [line (range 1500)]
                           (str "mostly stable line " line " revision "
                                (if (= line revision) revision 0) "\n"))))
            (git! source "add" "delta.txt")
            (git! source "commit" "-q" "-m" (str "delta " revision)))
          (git! source "gc" "--aggressive" "--prune=now")
          (let [events (atom [])
                result (local/import! conn source
                                      {:remote "origin"
                                       :primitive-index-threshold 0
                                       :delta-frontier-bytes 1
                                       :delta-parallelism 4
                                       :phase-fn #(swap! events conj %)})]
            (is (= format (:object-format result)))
            (is (pos? (:packs result)))
            (is (pos? (:pack-objects result)))
            (is (pos? (get (:storage-types result) :ofs-delta 0)))
            (is (:delta-spilled? result))
            (is (pos? (:delta-spill-peak-live-bytes result)))
            (is (pos? (:delta-spill-file-bytes result)))
            (is (= 4 (:delta-parallelism result)))
            (is (every? pos? (vals (:timings-ms result))))
            (is (every? (set (map :phase
                                  (filter #(= :complete (:event %)) @events)))
                        [:receive-pack :verify-checksum :discover-objects
                         :resolve-deltas :scan-pack :publish-pack]))
            (is (= 1 (:rounds
                      (some #(when (and (= :resolve-deltas (:phase %))
                                        (= :complete (:event %)))
                               %)
                            @events))))
            (is (= "two\n"
                   (String. ^bytes (repo/read conn "README.md") "UTF-8"))))
          (finally (close! target)))))))

(deftest imports-packed-refs-and-linked-worktree-head
  (let [source (native-repository)
        linked (File. (.getParentFile source) "linked")
        {:keys [conn] :as target} (database)]
    (try
      (git! source "branch" "archived" "HEAD~1")
      (git! source "pack-refs" "--all")
      (git! source "worktree" "add" "-q" "-b" "linked" (.getPath linked))
      (spit (File. linked "linked.txt") "linked\n")
      (git! linked "add" "linked.txt")
      (git! linked "commit" "-q" "-m" "linked")
      (let [discovered (local/require-repository linked)
            result (local/import! conn linked {:remote "origin"})]
        (is (not= (:git-dir discovered) (:common-dir discovered)))
        (is (= "refs/heads/linked" (repo/current-ref conn)))
        (is (= "linked\n"
               (String. ^bytes (repo/read conn "linked.txt") "UTF-8")))
        (is (contains? (store/refs conn) "refs/remotes/origin/archived"))
        (is (= "refs/heads/linked"
               (get-in result [:checkout :fetched :refs 0
                               :attributes :symref-target]))))
      (finally (close! target)))))

(deftest imports-bare-repositories-and-object-alternates
  (let [source (native-repository)
        parent (.getParentFile source)
        bare (File. parent "bare.git")
        shared (File. parent "shared")]
    (git! parent "clone" "-q" "--bare" (.getPath source) (.getPath bare))
    (git! parent "clone" "-q" "--shared" (.getPath source) (.getPath shared))
    (is (.isFile (File. shared ".git/objects/info/alternates")))
    (doseq [repository [bare shared]]
      (let [{:keys [conn] :as target} (database)]
        (try
          (let [result (local/import! conn repository {:remote "origin"})]
            (is (= "two\n"
                   (String. ^bytes (repo/read conn "README.md") "UTF-8")))
            (is (= "two"
                   (:geschichte.commit/message
                    (repo/commit-by-id conn (get-in result [:checkout :commit]))))))
          (finally (close! target)))))))
