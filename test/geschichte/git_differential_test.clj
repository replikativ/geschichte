(ns geschichte.git-differential-test
  "Small semantic oracles against the installed native Git executable.

  Geschichte uses UUID commit identities and Datahike storage, so differential
  tests compare stable user-visible semantics rather than serialized objects."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [geschichte.cli :as cli]
            [geschichte.git.command :as command]
            [geschichte.repo :as repo])
  (:import [java.nio.file Files Path]
           [java.nio.file.attribute FileAttribute]))

(defn- process [directory & args]
  (let [argv (into-array String (concat ["git" "-C" (str directory)] args))
        process (-> (ProcessBuilder. argv)
                    (.redirectErrorStream true)
                    (.start))
        output (slurp (.getInputStream process))
        exit (.waitFor process)]
    {:exit exit :output output}))

(defn- process-input [directory input & args]
  (let [argv (into-array String (concat ["git" "-C" (str directory)] args))
        process (-> (ProcessBuilder. argv) .start)]
    (with-open [writer (java.io.OutputStreamWriter. (.getOutputStream process)
                                                    "UTF-8")]
      (.write writer ^String input))
    (let [stdout (slurp (.getInputStream process))
          stderr (slurp (.getErrorStream process))
          exit (.waitFor process)]
      {:exit exit :output stdout :error stderr})))

(defn- git-available? []
  (try
    (zero? (:exit (process "." "--version")))
    (catch java.io.IOException _ false)))

(defn- delete-tree! [^Path root]
  (doseq [path (sort-by #(count (str %)) > (file-seq (.toFile root)))]
    (Files/deleteIfExists (.toPath path))))

(defn- native-history! [^Path root]
  (is (zero? (:exit (process root "init" "-q" "-b" "main"))))
  (is (zero? (:exit (process root "config" "user.name" "Ada"))))
  (is (zero? (:exit (process root "config" "user.email" "ada@example.test"))))
  (doseq [[text message] [["one\n" "one"] ["two\n" "two"]
                          ["three\n" "three"]]]
    (spit (.toFile (.resolve root "story.txt")) text)
    (is (zero? (:exit (process root "add" "story.txt"))))
    (is (zero? (:exit (process root "commit" "-q" "-m" message))))))

(defn- geschichte-history! [conn run]
  (repo/init! conn)
  (doseq [[text message] [["one\n" "one"] ["two\n" "two"]
                          ["three\n" "three"]]]
    (repo/write! conn "story.txt" (.getBytes text "UTF-8"))
    (is (zero? (:exit (run ["add" "story.txt"]))))
    (is (zero? (:exit (run ["commit" "-m" message]))))))

(deftest native-git-revision-range-oracle
  (if-not (git-available?)
    (is true "native Git is unavailable; differential oracle skipped")
    (let [root (Files/createTempDirectory
                "geschichte-git-oracle-" (make-array FileAttribute 0))
          cfg {:store {:backend :memory :id (random-uuid)}
               :schema-flexibility :write
               :keep-history? true :commit-graph? true}]
      (d/create-database cfg)
      (let [conn (d/connect cfg)
            run #(command/execute
                  {:conn conn :root "/project"
                   :config (atom {"user.name" "Ada"
                                  "user.email" "ada@example.test"})
                   :repo-relative identity}
                  %)]
        (try
          (native-history! root)
          (geschichte-history! conn run)
          (testing "two-dot log and diff select the same commits and paths"
            (is (= (:output (process root "log" "--format=%s"
                                     "HEAD~2..HEAD"))
                   (:stdout (run ["log" "--format=%s" "HEAD~2..HEAD"]))))
            (is (= (:output (process root "diff" "--name-status"
                                     "HEAD~2..HEAD"))
                   (:stdout (run ["diff" "--name-status"
                                  "HEAD~2..HEAD"])))))
          (testing "ancestry queries use Git's exit-status contract"
            (is (= (:exit (process root "merge-base" "--is-ancestor"
                                   "HEAD~2" "HEAD"))
                   (:exit (run ["merge-base" "--is-ancestor"
                                "HEAD~2" "HEAD"]))))
            (is (= (:exit (process root "merge-base" "--is-ancestor"
                                   "HEAD" "HEAD~2"))
                   (:exit (run ["merge-base" "--is-ancestor"
                                "HEAD" "HEAD~2"])))))
          (testing "three-dot diff compares the right tip with the merge base"
            (is (zero? (:exit (process root "branch" "feature" "HEAD~1"))))
            (is (zero? (:exit (process root "checkout" "-q" "feature"))))
            (spit (.toFile (.resolve root "feature.txt")) "feature\n")
            (is (zero? (:exit (process root "add" "feature.txt"))))
            (is (zero? (:exit (process root "commit" "-q" "-m" "feature"))))
            (is (zero? (:exit (process root "checkout" "-q" "main"))))
            (is (zero? (:exit (run ["branch" "feature" "HEAD~1"]))))
            (is (zero? (:exit (run ["checkout" "feature"]))))
            (repo/write! conn "feature.txt" (.getBytes "feature\n" "UTF-8"))
            (is (zero? (:exit (run ["add" "feature.txt"]))))
            (is (zero? (:exit (run ["commit" "-m" "feature"]))))
            (is (zero? (:exit (run ["checkout" "main"]))))
            (is (= (:output (process root "diff" "--name-status"
                                     "main...feature"))
                   (:stdout (run ["diff" "--name-status"
                                  "main...feature"])))))
          (testing "path filtering applies before max-count"
            (spit (.toFile (.resolve root "other.txt")) "other\n")
            (is (zero? (:exit (process root "add" "other.txt"))))
            (is (zero? (:exit (process root "commit" "-q" "-m" "other"))))
            (repo/write! conn "other.txt" (.getBytes "other\n" "UTF-8"))
            (is (zero? (:exit (run ["add" "other.txt"]))))
            (is (zero? (:exit (run ["commit" "-m" "other"]))))
            (is (= (:output (process root "log" "-1" "--format=%s"
                                     "--" "story.txt"))
                   (:stdout (run ["log" "-1" "--format=%s"
                                  "--" "story.txt"])))))
          (finally
            (d/release conn)
            (d/delete-database cfg)
            (delete-tree! root)))))))

(deftest native-git-diff-format-oracle
  (if-not (git-available?)
    (is true "native Git is unavailable; differential oracle skipped")
    (let [root (Files/createTempDirectory
                "geschichte-git-diff-oracle-" (make-array FileAttribute 0))
          cfg {:store {:backend :memory :id (random-uuid)}
               :schema-flexibility :write
               :keep-history? true :commit-graph? true}]
      (d/create-database cfg)
      (let [conn (d/connect cfg)
            run #(command/execute
                  {:conn conn :root "/project"
                   :config (atom {"user.name" "Ada"
                                  "user.email" "ada@example.test"})
                   :repo-relative identity}
                  %)
            story (.toFile (.resolve root "story.txt"))]
        (try
          (is (zero? (:exit (process root "init" "-q" "-b" "main"))))
          (is (zero? (:exit (process root "config" "user.name" "Ada"))))
          (is (zero? (:exit (process root "config" "user.email"
                                     "ada@example.test"))))
          (repo/init! conn)
          (spit story "one\nsame\nend\n")
          (repo/write! conn "story.txt" (.getBytes "one\nsame\nend\n" "UTF-8"))
          (is (zero? (:exit (process root "add" "story.txt"))))
          (is (zero? (:exit (process root "commit" "-q" "-m" "initial"))))
          (is (zero? (:exit (run ["add" "story.txt"]))))
          (is (zero? (:exit (run ["commit" "-m" "initial"]))))

          (spit story "two\nsame\nend\n")
          (repo/write! conn "story.txt" (.getBytes "two\nsame\nend\n" "UTF-8"))
          (testing "ordinary patches, context controls, raw and numstat match Git"
            (doseq [args [["diff"] ["diff" "-U0"]
                          ["diff" "-u"]
                          ["diff" "--unified=1"] ["diff" "--numstat"]
                          ["diff" "--raw"] ["diff" "--stat"]
                          ["diff" "--shortstat"]
                          ["diff" "--patch-with-raw"]
                          ["diff" "--patch-with-stat"]
                          ["diff" "--raw" "--abbrev=12"]
                          ["diff" "--raw" "-z"]
                          ["diff" "--name-status" "-z"]
                          ["diff" "--name-only" "-z"]
                          ["diff" "-R"]
                          ["diff" "--full-index"] ["diff" "--stat" "-p"]
                          ["diff" "--color=never"]
                          ["diff" "--color=always"]]]
              (let [native (apply process root args)
                    ours (run args)]
                (is (= (:exit native) (:exit ours)) args)
                (is (= (:output native) (:stdout ours)) args))))
          (testing "quiet and exit-code have Git's distinct output contracts"
            (doseq [args [["diff" "--quiet"] ["diff" "--exit-code"]]]
              (let [native (apply process root args)
                    ours (run args)]
                (is (= (:exit native) (:exit ours)) args)
                (is (= (:output native) (:stdout ours)) args))))

          (is (zero? (:exit (process root "add" "story.txt"))))
          (is (zero? (:exit (run ["add" "story.txt"]))))
          (testing "cached patches match Git"
            (is (= (:output (process root "diff" "--cached"))
                   (:stdout (run ["diff" "--cached"])))))

          (is (zero? (:exit (process root "commit" "-q" "-m" "changed"))))
          (is (zero? (:exit (run ["commit" "-m" "changed"]))))
          (spit story "two   \nsame\nend\n")
          (repo/write! conn "story.txt" (.getBytes "two   \nsame\nend\n" "UTF-8"))
          (testing "whitespace comparison modes match Git"
            (doseq [args [["diff" "-w"] ["diff" "-b"]
                          ["diff" "--ignore-space-at-eol"]
                          ["diff" "--check"]]]
              (is (= (:output (apply process root args))
                     (:stdout (run args))) args)))
          (spit story "two\nsame\nend\n")
          (is (.setExecutable story true false))
          (repo/write! conn "story.txt" (.getBytes "two\nsame\nend\n" "UTF-8")
                       {:mode 33261})
          (testing "mode-only worktree and staged patches match Git"
            (is (= (:output (process root "diff"))
                   (:stdout (run ["diff"]))))
            (is (= (:output (process root "diff" "--raw"))
                   (:stdout (run ["diff" "--raw"]))))
            (is (zero? (:exit (process root "add" "story.txt"))))
            (is (zero? (:exit (run ["add" "story.txt"]))))
            (is (= (:output (process root "diff" "--cached"))
                   (:stdout (run ["diff" "--cached"])))))

          (is (zero? (:exit (process root "commit" "-q" "-m" "mode"))))
          (is (zero? (:exit (run ["commit" "-m" "mode"]))))
          (let [binary (.toFile (.resolve root "data.bin"))
                payload (byte-array (map unchecked-byte (range 256)))]
            (with-open [out (java.io.FileOutputStream. binary)]
              (.write out payload))
            (repo/write! conn "data.bin" payload
                         {:chunk-threshold 1
                          :chunk-min-size 32 :chunk-size 64
                          :chunk-max-size 128})
            (is (zero? (:exit (process root "add" "data.bin"))))
            (is (zero? (:exit (run ["add" "data.bin"]))))
            (testing "added binary headers and numeric summaries match Git"
              (doseq [args [["diff" "--cached"]
                            ["diff" "--cached" "--numstat"]]]
                (is (= (:output (apply process root args))
                       (:stdout (run args))) args)))
            (testing "native git apply accepts Geschichte literal binary patches"
              (let [apply-root (Files/createTempDirectory
                                "geschichte-binary-apply-"
                                (make-array FileAttribute 0))]
                (try
                  (is (zero? (:exit (process apply-root "init" "-q"))))
                  (let [patch (with-redefs
                               [repo/read-entry
                                (fn [& _]
                                  (throw (ex-info
                                          "binary diff materialized an entry"
                                          {})))]
                                (:stdout (run ["diff" "--cached" "--binary"])))
                        applied (process-input apply-root patch "apply" "-")]
                    (is (zero? (:exit applied)) applied)
                    (when (zero? (:exit applied))
                      (is (= (seq payload)
                             (seq (Files/readAllBytes
                                   (.resolve apply-root "data.bin")))))))
                  (finally (delete-tree! apply-root))))))

          (is (zero? (:exit (process root "commit" "-q" "-m" "binary"))))
          (is (zero? (:exit (run ["commit" "-m" "binary"]))))
          (is (zero? (:exit (process root "mv" "story.txt" "renamed.txt"))))
          (is (zero? (:exit (run ["mv" "story.txt" "renamed.txt"]))))
          (testing "exact renames are planned from content identity"
            (doseq [args [["diff" "--cached"]
                          ["diff" "--cached" "--name-status"]
                          ["diff" "--cached" "--raw"]]]
              (is (= (:output (apply process root args))
                     (:stdout (run args))) args)))

          (is (zero? (:exit (process root "commit" "-q" "-m" "rename"))))
          (is (zero? (:exit (run ["commit" "-m" "rename"]))))
          (is (zero? (:exit (process root "mv" "renamed.txt" "similar.txt"))))
          (is (zero? (:exit (run ["mv" "renamed.txt" "similar.txt"]))))
          (spit (.toFile (.resolve root "similar.txt")) "changed\nsame\nend\n")
          (repo/write! conn "similar.txt"
                       (.getBytes "changed\nsame\nend\n" "UTF-8") {:mode 33261})
          (is (zero? (:exit (process root "add" "similar.txt"))))
          (is (zero? (:exit (run ["add" "similar.txt"]))))
          (testing "bounded span-hash similarity matches Git rename scoring"
            (doseq [args [["diff" "--cached" "--name-status"]
                          ["diff" "--cached"]
                          ["diff" "--cached" "--name-status" "-M90%"]
                          ["diff" "--cached" "--name-status"
                           "--find-renames=90%"]
                          ["diff" "--cached" "--name-status" "--no-renames"]]]
              (is (= (:output (apply process root args))
                     (:stdout (run args))) args)))
          (finally
            (d/release conn)
            (d/delete-database cfg)
            (delete-tree! root)))))))

(deftest physical-projection-status-oracle
  (if-not (git-available?)
    (is true "native Git is unavailable; differential oracle skipped")
    (let [git-root (Files/createTempDirectory
                    "geschichte-native-worktree-" (make-array FileAttribute 0))
          ges-root (Files/createTempDirectory
                    "geschichte-physical-worktree-" (make-array FileAttribute 0))
          ges #(cli/run (into ["-C" (str ges-root)] %))]
      (try
        (is (zero? (:exit (process git-root "init" "-q" "-b" "main"))))
        (is (zero? (:exit (ges ["init" "-q" "-b" "main"]))))
        (doseq [root [git-root ges-root]]
          (spit (.toFile (.resolve root "tracked.txt")) "one\n")
          (spit (.toFile (.resolve root "ignored.log")) "ignored\n")
          (spit (.toFile (.resolve root ".gitignore")) "*.log\n"))
        (testing "porcelain status and ignore semantics agree"
          (is (= (:output (process git-root "status" "--short"))
                 (:stdout (ges ["status" "--short"])))))
        (doseq [root [git-root ges-root]]
          (Files/createDirectories (.resolve root "src/nested")
                                   (make-array FileAttribute 0))
          (spit (.toFile (.resolve root "src/nested/core.clj"))
                "(def value 1)\n"))
        (is (zero? (:exit (process git-root "add" "-A"))))
        (is (zero? (:exit (ges ["add" "-A"]))))
        (is (zero? (:exit (process git-root "-c" "user.name=Ada"
                                   "-c" "user.email=ada@example.test"
                                   "commit" "-q" "-m" "initial"))))
        (is (zero? (:exit (ges ["-c" "user.name=Ada"
                                "-c" "user.email=ada@example.test"
                                "commit" "-q" "-m" "initial"]))))
        (doseq [root [git-root ges-root]]
          (spit (.toFile (.resolve root "tracked.txt")) "two\n")
          (spit (.toFile (.resolve root "src/nested/core.clj"))
                "(def value 2)\n"))
        (testing "rescanned changes preserve Git's stable read surface"
          (is (= (:output (process git-root "status" "--short"))
                 (:stdout (ges ["status" "--short"]))))
          (is (= (:output (process git-root "diff" "--name-status"))
                 (:stdout (ges ["diff" "--name-status"]))))
          (is (= (:output (process git-root "-C" "src/nested" "diff"
                                   "--name-status" "--" "core.clj"))
                 (:stdout (ges ["-C" "src/nested" "diff"
                                "--name-status" "--" "core.clj"]))))
          (is (= (:output (process git-root "-C" "src/nested" "diff"
                                   "--name-status" "--" ":(top)*"
                                   ":(top,exclude)tracked.txt"))
                 (:stdout (ges ["-C" "src/nested" "diff"
                                "--name-status" "--" ":(top)*"
                                ":(top,exclude)tracked.txt"])))))
        (finally
          (delete-tree! git-root)
          (delete-tree! ges-root))))))
