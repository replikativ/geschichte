(ns geschichte.cli-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [geschichte.cli :as cli]
            [geschichte.git.transport :as transport]
            [geschichte.projection :as projection]
            [geschichte.repo :as repo]))

(deftest navigable-help-does-not-require-a-repository
  (let [top-level (cli/run ["--help"])
        status (cli/run ["status" "--help"])
        query (cli/run ["help" "db" "query"])
        alias (cli/run ["help" "query"])
        group (cli/run ["workspace" "--help"])
        after-globals (cli/run ["-C" "/tmp" "git" "-c" "color.ui=false"
                                "log" "--help"])
        missing (cli/run ["help" "imaginary-command"])]
    (is (= 0 (:exit top-level)))
    (is (str/includes? (:stdout top-level) "●━━●━┯━●  Geschichte"))
    (is (str/includes? (:stdout top-level) "ges help [COMMAND]"))
    (is (= 0 (:exit status)))
    (is (str/includes? (:stdout status) "Usage: ges status"))
    (is (= 0 (:exit query)))
    (is (str/includes? (:stdout query) "Usage: ges db query DATALOG"))
    (is (= (:stdout query) (:stdout alias)))
    (is (str/includes? (:stdout group) "workspace publish"))
    (is (str/includes? (:stdout after-globals) "Usage: ges log"))
    (is (= 129 (:exit missing)))
    (is (str/includes? (:stderr missing) "no help for 'imaginary-command'"))))

(deftest explicit-repository-cli-lifecycle
  (let [directory (.toFile
                   (java.nio.file.Files/createTempDirectory
                    "geschichte-cli-" (make-array java.nio.file.attribute.FileAttribute 0)))
        config-file (java.io.File. directory "repo.edn")
        config {:store {:backend :file
                        :path (.getAbsolutePath (java.io.File. directory "store"))
                        :id (random-uuid)}
                :schema-flexibility :write
                :keep-history? true
                :commit-graph? true}]
    (spit config-file (pr-str config))
    (try
      (is (zero? (:exit (cli/run ["--repo" (.getPath config-file) "init"]))))
      (let [status (cli/run ["--repo" (.getPath config-file)
                             "repo" "status"])]
        (is (zero? (:exit status)))
        (is (:clean? (edn/read-string (:stdout status)))))
      (let [result (cli/run
                    ["--repo" (.getPath config-file) "query"
                     "[:find ?head . :where [?repo :geschichte.repo/head ?head]]"])]
        (is (= "refs/heads/main" (edn/read-string (:stdout result)))))
      (let [result (cli/run ["--repo" (.getPath config-file)
                             "git" "rev-parse" "--is-inside-work-tree"])]
        (is (= 0 (:exit result)))
        (is (= "true\n" (:stdout result))))
      (finally
        (when (d/database-exists? config) (d/delete-database config))))))

(deftest discovered-physical-projection-lifecycle
  (let [directory (.toFile
                   (java.nio.file.Files/createTempDirectory
                    "geschichte-projection-"
                    (make-array java.nio.file.attribute.FileAttribute 0)))
        nested (doto (java.io.File. directory "src/example") .mkdirs)
        readme (java.io.File. directory "README.md")
        source (java.io.File. nested "core.clj")]
    (spit readme "one\n")
    (spit source "(def value 1)\n")
    (let [init (cli/run ["-C" (.getPath directory) "init"])]
      (is (zero? (:exit init)) init)
      (let [marker (java.io.File. directory ".geschichte/repo.edn")
            config (:datahike/config (edn/read-string (slurp marker)))]
        (is (.isFile marker))
        (is (= 256 (get-in config [:index-config :diff-buf-size])))
        (is (true? (:fuse-index-roots? config)))))
    (let [status (cli/run ["-C" (.getPath nested) "status" "--short"])]
      (is (= 0 (:exit status)) status)
      (is (= "?? README.md\n?? src/example/core.clj\n" (:stdout status))))
    (is (= 0 (:exit (cli/run ["-C" (.getPath directory) "add" "-A"]))))
    (is (= 0 (:exit (cli/run ["-C" (.getPath directory)
                              "-c" "user.name=Ada"
                              "commit" "-m" "initial"]))))
    (spit readme "two\n")
    (spit source "(def value 2)\n")
    (let [status (cli/run ["-C" (.getPath directory) "status" "--short"])]
      (is (= 0 (:exit status)) status)
      (is (= " M README.md\n M src/example/core.clj\n" (:stdout status))))
    (testing "diff pathspecs are relative to -C and support common magic"
      (let [relative (cli/run ["-C" (.getPath nested) "diff" "--" "core.clj"])
            top (cli/run ["-C" (.getPath nested) "diff" "--"
                          ":(top)README.md"])
            excluded (cli/run ["-C" (.getPath nested) "diff" "--"
                               ":(top)*" ":(top,exclude)README.md"])
            shallow-glob (cli/run ["-C" (.getPath directory) "diff" "--"
                                   ":(top,glob)src/*.clj"])
            recursive-glob (cli/run ["-C" (.getPath directory) "diff" "--"
                                     ":(top,glob)src/**/core.clj"])]
        (is (str/includes? (:stdout relative) "src/example/core.clj"))
        (is (not (str/includes? (:stdout relative) "README.md")))
        (is (str/includes? (:stdout top) "README.md"))
        (is (str/includes? (:stdout excluded) "src/example/core.clj"))
        (is (not (str/includes? (:stdout excluded) "README.md")))
        (is (= "" (:stdout shallow-glob)))
        (is (str/includes? (:stdout recursive-glob)
                           "src/example/core.clj"))))
    (is (not (str/includes?
              (:stdout (cli/run ["-C" (.getPath directory) "ls-files"]))
              ".geschichte")))))

(deftest clone-creates-and-materializes-a-projection
  (let [parent (.toFile
                (java.nio.file.Files/createTempDirectory
                 "geschichte-clone-"
                 (make-array java.nio.file.attribute.FileAttribute 0)))
        destination (java.io.File. parent "checkout")
        clone! (fn [{:keys [conn]}]
                 (repo/write! conn "README.md" (.getBytes "cloned\n" "UTF-8"))
                 (repo/stage-all! conn)
                 (repo/commit! conn {:message "remote" :author "Ada"}))]
    (with-redefs [transport/operations {:clone clone!}]
      (let [result (cli/run ["-C" (.getPath parent) "clone"
                             "https://example.test/repo.git" "checkout"])]
        (is (= 0 (:exit result)) result)
        (is (= "cloned\n" (slurp (java.io.File. destination "README.md"))))
        (when (contains? (.supportedFileAttributeViews
                          (java.nio.file.FileSystems/getDefault))
                         "posix")
          (let [permissions
                (java.nio.file.Files/getPosixFilePermissions
                 (.toPath (java.io.File. destination "README.md"))
                 (make-array java.nio.file.LinkOption 0))]
            (is (contains? permissions
                           java.nio.file.attribute.PosixFilePermission/GROUP_READ))
            (is (contains? permissions
                           java.nio.file.attribute.PosixFilePermission/OTHERS_READ))))
        (is (.isFile (java.io.File. destination ".geschichte/repo.edn")))
        (is (= "remote\n"
               (:stdout (cli/run ["-C" (.getPath destination)
                                  "log" "-1" "--format=%s"]))))))))

(deftest clone-no-checkout-preserves-an-unmaterialized-repository
  (let [parent (.toFile
                (java.nio.file.Files/createTempDirectory
                 "geschichte-clone-no-checkout-"
                 (make-array java.nio.file.attribute.FileAttribute 0)))
        destination (java.io.File. parent "checkout")
        received (atom nil)
        clone! (fn [{:keys [options] :as request}]
                 (reset! received request)
                 (is (true? (:no-checkout? options))))]
    (with-redefs [transport/operations {:clone clone!}]
      (let [result (cli/run ["-C" (.getPath parent)
                             "-c" "geschichte.git.primitive-index-threshold=0"
                             "-c" "geschichte.git.delta-parallelism=3"
                             "clone" "--no-checkout"
                             "https://example.test/repo.git" "checkout"])]
        (is (= 0 (:exit result)) result)
        (is (.isFile (java.io.File. destination ".geschichte/repo.edn")))
        (is (not (.exists (java.io.File. destination "README.md"))))
        (is (= 0 (get-in @received
                         [:options :primitive-index-threshold])))
        (is (= 3 (get-in @received [:options :delta-parallelism])))))))

(deftest physical-workspaces-are-isolated-and-publish-explicitly
  (let [parent (.toFile
                (java.nio.file.Files/createTempDirectory
                 "geschichte-workspaces-"
                 (make-array java.nio.file.attribute.FileAttribute 0)))
        root (java.io.File. parent "root")
        first (java.io.File. parent "agent-one")
        second (java.io.File. parent "agent-two")
        run #(cli/run (into ["-C" (.getPath %1)] %2))]
    (.mkdirs root)
    (is (zero? (:exit (run root ["init"]))))
    (spit (java.io.File. root "base.txt") "base\n")
    (is (zero? (:exit (run root ["add" "-A"]))))
    (is (zero? (:exit (run root ["-c" "user.name=Ada"
                                 "commit" "-m" "base"]))))
    ;; Unlike Git, both agents may independently check out the same ref.
    (is (zero? (:exit (run root ["worktree" "add" (.getPath first) "main"]))))
    (is (zero? (:exit (run root ["worktree" "add" (.getPath second) "main"]))))
    (is (= "base\n" (slurp (java.io.File. first "base.txt"))))
    (let [root-marker (projection/read-marker (projection/marker-file root))
          first-marker (projection/read-marker (projection/marker-file first))
          second-marker (projection/read-marker (projection/marker-file second))]
      (is (= (:canonical-marker root-marker)
             (:canonical-marker first-marker)
             (:canonical-marker second-marker)))
      (is (= 3 (count (set (map :workspace-branch
                                [root-marker first-marker second-marker]))))))
    (is (= 3 (count (edn/read-string
                     (:stdout (run root ["workspace" "list"]))))))
    (spit (java.io.File. first "published.txt") "from agent\n")
    (is (zero? (:exit (run first ["add" "-A"]))))
    (is (zero? (:exit (run first ["-c" "user.name=Ada"
                                  "commit" "-m" "agent"]))))
    (is (not (.exists (java.io.File. root "published.txt"))))
    (is (zero? (:exit (run first ["workspace" "publish"]))))
    ;; Publication moves canonical history, observation remains explicit.
    (is (not (.exists (java.io.File. root "published.txt"))))
    (is (zero? (:exit (run root ["workspace" "advance"]))))
    (is (= "from agent\n" (slurp (java.io.File. root "published.txt"))))
    (is (zero? (:exit (run root ["worktree" "remove" (.getPath first)]))))
    (is (zero? (:exit (run root ["worktree" "remove" (.getPath second)]))))
    (is (= 1 (count (edn/read-string
                     (:stdout (run root ["workspace" "list"]))))))))

(deftest physical-workspace-removal-and-pruning-are-conservative
  (let [parent (.toFile
                (java.nio.file.Files/createTempDirectory
                 "geschichte-workspace-lifecycle-"
                 (make-array java.nio.file.attribute.FileAttribute 0)))
        root (java.io.File. parent "root")
        dirty (java.io.File. parent "dirty")
        stale (java.io.File. parent "stale")
        run #(cli/run (into ["-C" (.getPath %1)] %2))]
    (.mkdirs root)
    (is (zero? (:exit (run root ["init"]))))
    (spit (java.io.File. root "base.txt") "base\n")
    (is (zero? (:exit (run root ["add" "-A"]))))
    (is (zero? (:exit (run root ["-c" "user.name=Ada"
                                 "commit" "-m" "base"]))))
    (is (zero? (:exit (run root ["worktree" "add" (.getPath dirty)]))))
    (spit (java.io.File. dirty "untracked.txt") "keep me\n")
    (let [refused (run root ["worktree" "remove" (.getPath dirty)])]
      (is (= 128 (:exit refused)))
      (is (str/includes? (:stderr refused) "modified or untracked"))
      (is (.exists (java.io.File. dirty "untracked.txt"))))
    (is (zero? (:exit (run root ["worktree" "remove" "--force"
                                 (.getPath dirty)]))))
    (is (not (.exists dirty)))
    (is (zero? (:exit (run root ["worktree" "add" (.getPath stale)]))))
    ;; A missing marker represents an externally removed/interrupted projection.
    (is (.delete (projection/marker-file stale)))
    (is (zero? (:exit (run root ["worktree" "prune"]))))
    (is (not (.exists stale)))
    (is (= 1 (count (edn/read-string
                     (:stdout (run root ["workspace" "list"]))))))))

(defn- native-git-result [directory & args]
  (let [process (-> (ProcessBuilder. ^java.util.List (into ["git"] args))
                    (.directory ^java.io.File directory)
                    (.redirectErrorStream true)
                    .start)
        output (slurp (.getInputStream process))
        exit (.waitFor process)]
    {:exit exit :output output}))

(defn- native-git! [directory & args]
  (let [{:keys [exit output]} (apply native-git-result directory args)]
    (when-not (zero? exit)
      (throw (ex-info "native Git fixture failed"
                      {:args args :exit exit :output output})))
    output))

(deftest no-index-diff-does-not-require-a-repository
  (let [directory (.toFile
                   (java.nio.file.Files/createTempDirectory
                    "geschichte-no-index-"
                    (make-array java.nio.file.attribute.FileAttribute 0)))
        left (java.io.File. directory "before.txt")
        right (java.io.File. directory "after.txt")]
    (spit left "one\nsame\n")
    (spit right "two\nsame\n")
    (doseq [args [["diff" "--no-index" "before.txt" "after.txt"]
                  ["diff" "--no-index" "-U0" "before.txt" "after.txt"]
                  ["diff" "--no-index" "--quiet" "before.txt" "after.txt"]
                  ["diff" "--no-index" "--quiet" "--exit-code"
                   "before.txt" "after.txt"]]]
      (let [native (apply native-git-result directory args)
            ours (cli/run (into ["-C" (.getPath directory)] args))]
        (is (= (:exit native) (:exit ours)) args)
        (is (= (:output native) (:stdout ours)) args)))
    (let [before (doto (java.io.File. directory "before") .mkdirs)
          after (doto (java.io.File. directory "after") .mkdirs)]
      (spit (java.io.File. before "changed.txt") "before\n")
      (spit (java.io.File. after "changed.txt") "after\n")
      (spit (java.io.File. before "deleted.txt") "deleted\n")
      (spit (java.io.File. after "added.txt") "added\n")
      (spit (java.io.File. before "mode.txt") "mode\n")
      (spit (java.io.File. after "mode.txt") "mode\n")
      (is (.setExecutable (java.io.File. after "mode.txt") true false))
      (let [args ["diff" "--no-index" "before" "after"]
            native (apply native-git-result directory args)
            ours (cli/run (into ["-C" (.getPath directory)] args))]
        (is (= (:exit native) (:exit ours)))
        (is (= (:output native) (:stdout ours)))))))

(deftest detects-and-directly-imports-native-git-repositories
  (let [parent (.toFile
                (java.nio.file.Files/createTempDirectory
                 "geschichte-cli-import-"
                 (make-array java.nio.file.attribute.FileAttribute 0)))
        source (doto (java.io.File. parent "source") .mkdirs)
        checkout (java.io.File. parent "checkout")
        file-checkout (java.io.File. parent "file-checkout")]
    (native-git! parent "init" "-q" "-b" "main" (.getPath source))
    (native-git! source "config" "user.name" "Ada")
    (native-git! source "config" "user.email" "ada@example.test")
    (spit (java.io.File. source "README.md") "native\n")
    (native-git! source "add" "README.md")
    (native-git! source "commit" "-q" "-m" "native history")
    (let [detected (cli/run ["-C" (.getPath source) "init"])]
      (is (= 128 (:exit detected)))
      (is (str/includes? (:stderr detected) "ges import-git")))
    (is (zero? (:exit (cli/run ["-C" (.getPath source)
                                "import-git" "--force" "."]))))
    (is (.isDirectory (java.io.File. source ".git")))
    (is (not (str/includes?
              (:stdout (cli/run ["-C" (.getPath source) "ls-files"]))
              ".git/")))
    (is (= "native history\n"
           (:stdout (cli/run ["-C" (.getPath source)
                              "log" "-1" "--format=%s"]))))
    (is (zero? (:exit (cli/run ["-C" (.getPath parent) "clone"
                                (.getPath source) "checkout"]))))
    (is (= "native\n" (slurp (java.io.File. checkout "README.md"))))
    (is (zero? (:exit (cli/run ["-C" (.getPath parent) "clone"
                                (str (.toURI source)) "file-checkout"]))))
    (is (= "native\n" (slurp (java.io.File. file-checkout "README.md"))))))
