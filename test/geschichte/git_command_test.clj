(ns geschichte.git-command-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.git.command :as command]
            [geschichte.git.object :as object]
            [geschichte.repo :as repo]))

(defn- fixture []
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write
             :keep-history? true
             :commit-graph? true}]
    (d/create-database cfg)
    {:cfg cfg :conn (d/connect cfg)}))

(defn- cleanup [{:keys [cfg conn]}]
  (d/release conn)
  (d/delete-database cfg))

(deftest shared-git-command-lifecycle
  (is (= {:args ["status" "--short"]
          :directories ["repo" "sub"] :config {}}
         (command/parse-global
          ["--no-pager" "-C" "repo" "-C=sub" "status" "--short"])))
  (is (= {:origin "upstream" :branch "release" :quiet? true
          :url "https://example.test/team/project.git" :path "checkout"}
         (command/parse-clone
          ["-q" "-o" "upstream" "--branch=release"
           "https://example.test/team/project.git" "checkout"])))
  (is (= "project"
         (:path (command/parse-clone
                 ["https://example.test/team/project.git"]))))
  (is (true? (:no-checkout?
              (command/parse-clone
               ["--no-checkout" "https://example.test/team/project.git"]))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"shallow clone"
                        (command/parse-clone ["--depth" "1" "repo"])))
  (is (= {"user.name" "Ada" "user.email" "ada@example.test"}
         (:config (command/parse-global
                   ["-c" "user.name=Ada" "-c=user.email=ada@example.test"
                    "commit" "-m" "message"]))))
  (let [{:keys [conn] :as f} (fixture)
        config (atom {})
        run (fn [argv]
              (command/execute
               {:conn conn
                :root "/project"
                :config config
                :repo-relative (fn [path] (if (= "." path) "" path))}
               argv))]
    (try
      (repo/init! conn)
      (repo/write! conn "README.md" (.getBytes "hello\n" "UTF-8"))
      (is (= "?? README.md\n" (:stdout (run ["status" "--short"]))))
      (is (zero? (:exit (run ["config" "user.name" "Ada"]))))
      (is (zero? (:exit (run ["add" "."]))))
      (is (zero? (:exit (run ["commit" "-m" "initial"]))))
      (repo/write! conn "README.md" (.getBytes "changed\n" "UTF-8"))
      (is (str/includes? (:stdout (run ["diff" "--" "README.md"]))
                         "+changed"))
      (is (str/includes? (:stdout (run ["--no-pager" "log" "--oneline"]))
                         "initial"))
      (is (= "/project\n"
             (:stdout (run ["rev-parse" "--show-toplevel"]))))
      (finally (cleanup f)))))

(deftest revision-expressions-and-strict-options
  (let [{:keys [conn] :as f} (fixture)
        config (atom {"user.name" "Ada"})
        run #(command/execute
              {:conn conn :root "/project" :config config
               :repo-relative (fn [path] (if (= "." path) "" path))}
              %)]
    (try
      (repo/init! conn)
      (doseq [[text message] [["one\n" "one"] ["two\n" "two"]
                              ["three\n" "three"]]]
        (repo/write! conn "story.txt" (.getBytes text "UTF-8"))
        (is (zero? (:exit (run ["add" "story.txt"]))))
        (is (zero? (:exit (run ["commit" "-m" message])))))
      (is (str/includes? (:stdout (run ["log" "--oneline" "HEAD~1"]))
                         "two"))
      (is (not (str/includes? (:stdout (run ["log" "--oneline" "HEAD~1"]))
                              "three")))
      (is (= "three\n"
             (:stdout (run ["log" "-1" "--format=%s"]))))
      (is (str/includes? (:stdout (run ["log" "-1" "--stat"]))
                         "story.txt | 2 +-"))
      (is (str/includes? (:stdout (run ["diff" "HEAD~2" "HEAD" "--"
                                        "story.txt"]))
                         "+three"))
      (is (= "three\ntwo\n"
             (:stdout (run ["log" "--format=%s" "HEAD~2..HEAD"]))))
      (is (= "three\ntwo\n"
             (:stdout (run ["log" "--format=%s" "HEAD~2...HEAD"]))))
      (is (str/includes? (:stdout (run ["diff" "HEAD~2..HEAD"
                                        "--" "story.txt"]))
                         "+three"))
      (is (= (:stdout (run ["rev-parse" "HEAD~2"]))
             (:stdout (run ["merge-base" "HEAD~2" "HEAD"]))))
      (is (zero? (:exit (run ["merge-base" "--is-ancestor"
                              "HEAD~2" "HEAD"]))))
      (is (= 1 (:exit (run ["merge-base" "--is-ancestor"
                            "HEAD" "HEAD~2"]))))
      (is (str/includes? (:stdout (run ["show" "HEAD"])) "+three"))
      (is (str/includes? (:stdout (run ["show" "--stat" "HEAD"]))
                         "story.txt | 2 +-"))
      (is (not (str/includes?
                (:stdout (run ["show" "--format=%s" "--no-patch" "HEAD"]))
                "diff --git")))
      (is (zero? (:exit (run ["branch" "historical" "HEAD~1"]))))
      (is (str/includes? (:stdout (run ["log" "-1" "--oneline" "historical"]))
                         "two"))
      (is (zero? (:exit (run ["tag" "v1" "HEAD~1"]))))
      (is (= "v1\n" (:stdout (run ["tag"]))))
      (is (= "two\n"
             (:stdout (run ["show" "--format=%s" "--no-patch" "v1"]))))
      (let [head-before (:stdout (run ["rev-parse" "HEAD"]))
            amended (run ["commit" "--amend" "-m" "corrected"])]
        (is (zero? (:exit amended)))
        (is (not= head-before (:stdout (run ["rev-parse" "HEAD"]))))
        (is (= "corrected\n"
               (:stdout (run ["log" "-1" "--format=%s"])))))
      (is (= 129 (:exit (run ["status" "--porcelain=v2"]))))
      (repo/write! conn "other.txt" (.getBytes "other\n" "UTF-8"))
      (is (zero? (:exit (run ["add" "other.txt"]))))
      (is (zero? (:exit (run ["commit" "-m" "other"]))))
      (is (= "corrected\n"
             (:stdout (run ["log" "-1" "--format=%s" "--"
                            "story.txt"]))))
      (finally (cleanup f)))))

(deftest persistent-config-and-injected-remote-commands
  (let [{:keys [conn] :as f} (fixture)
        calls (atom [])
        remote-ops
        {:fetch (fn [request]
                  (swap! calls conj [:fetch request]) {:persisted 3})
         :pull (fn [request]
                 (swap! calls conj [:pull request]) {:pull/status :up-to-date})
         :push (fn [request]
                 (swap! calls conj [:push request])
                 {:report {:unpack "ok"
                           :refs {"refs/heads/main" {:status :ok}}}})
         :ls-remote (fn [request]
                      (swap! calls conj [:ls-remote request])
                      [{:oid (apply str (repeat 40 "a"))
                        :ref "refs/heads/main" :attributes {}}])}
        context (fn [] {:conn conn :root "/project" :config (atom {})
                        :remote-ops remote-ops :repo-relative identity})
        run #(command/execute (context) %)]
    (try
      (repo/init! conn)
      (repo/write! conn "README.md" (.getBytes "hello\n" "UTF-8"))
      (repo/stage-all! conn)
      (repo/commit! conn {:message "initial" :author "Ada"})
      (is (zero? (:exit (run ["config" "user.email" "ada@example.test"]))))
      ;; A fresh host config atom still observes repository-local configuration.
      (is (= "ada@example.test\n"
             (:stdout (run ["config" "--get" "user.email"]))))
      (is (zero? (:exit (run ["remote" "add" "origin"
                              "https://example.test/repo.git"]))))
      (is (str/includes? (:stdout (run ["remote" "-v"]))
                         "origin\thttps://example.test/repo.git (fetch)"))
      (is (zero? (:exit (run ["fetch" "origin"]))))
      (is (zero? (:exit (run ["pull" "--ff-only" "origin"]))))
      (is (zero? (:exit (run ["push" "-u" "origin" "main"]))))
      (is (str/includes? (:stdout (run ["ls-remote" "--heads" "origin"]))
                         "refs/heads/main"))
      (is (= [:fetch :pull :push :ls-remote] (mapv first @calls)))
      (is (= "origin"
             (get (repo/configuration conn) "branch.main.remote")))
      (finally (cleanup f)))))

(deftest common-agent-mutation-conveniences
  (let [{:keys [conn] :as f} (fixture)
        run #(command/execute
              {:conn conn :root "/project" :config (atom {})
               :repo-relative (fn [path] (if (= "." path) "" path))}
              %)]
    (try
      (repo/init! conn)
      (repo/write! conn "old.txt" (.getBytes "one\n" "UTF-8"))
      (is (zero? (:exit (run ["add" "."]))))
      (is (zero? (:exit (run ["commit" "-m" "initial"]))))
      (is (= "## main\n" (:stdout (run ["status" "-sb"]))))
      (repo/write! conn "old.txt" (.getBytes "two\n" "UTF-8"))
      (is (zero? (:exit (run ["commit" "-am" "tracked update"]))))
      (is (zero? (:exit (run ["mv" "old.txt" "new.txt"]))))
      (is (= "A  new.txt\nD  old.txt\n"
             (:stdout (run ["status" "--short"]))))
      (repo/write! conn "scratch.txt" (.getBytes "temporary\n" "UTF-8"))
      (is (str/includes? (:stdout (run ["clean" "-n"]))
                         "Would remove scratch.txt"))
      (is (some #{"scratch.txt"} (repo/files conn)))
      (is (str/includes? (:stdout (run ["clean" "-f"]))
                         "Removing scratch.txt"))
      (is (not (some #{"scratch.txt"} (repo/files conn))))
      (finally (cleanup f)))))

(deftest common-agent-read-and-commit-commands
  (let [{:keys [conn] :as f} (fixture)
        run #(command/execute
              {:conn conn :root "/project" :config (atom {"user.name" "Ada"})
               :read-message (fn [path]
                               (if (= path "-") "from stdin\n"
                                   (throw (ex-info "unexpected path" {:path path}))))
               :repo-relative identity} %)]
    (try
      (repo/init! conn)
      (repo/write! conn ".gitignore" (.getBytes "target/\n" "UTF-8"))
      (repo/write! conn "src/a.clj" (.getBytes "(def answer 42)\n" "UTF-8"))
      (is (zero? (:exit (run ["add" "-A"]))))
      (is (zero? (:exit (run ["commit" "-F-"]))))
      (is (= "from stdin\n" (:stdout (run ["log" "-1" "--format=%s"]))))
      (is (= 8 (count (str/trim (:stdout (run ["rev-parse" "--short" "HEAD"]))))))
      (is (= "1\n" (:stdout (run ["rev-list" "--count" "HEAD"]))))
      (is (= ".gitignore\nsrc/a.clj\n"
             (:stdout (run ["ls-tree" "-r" "--name-only" "HEAD"]))))
      (is (= "src/a.clj\n" (:stdout (run ["ls-files" "src/a.clj"]))))
      (let [oid (object/object-id :blob (.getBytes "(def answer 42)\n" "UTF-8"))]
        (is (str/includes? (:stdout (run ["ls-tree" "-r" "HEAD"]))
                           (str "blob " oid "\tsrc/a.clj")))
        (is (= (str "100644 " oid " 0\tsrc/a.clj\n")
               (:stdout (run ["ls-files" "--stage" "src/a.clj"])))))
      (is (= "from stdin\n"
             (:stdout (run ["log" "--format=%s" "--grep=stdin"]))))
      (is (= "from stdin\n"
             (:stdout (run ["log" "--format=%s" "-S" "answer"]))))
      (is (str/includes? (:stdout (run ["grep" "-nE" "answer" "--" "src"]))
                         "src/a.clj:1:"))
      (is (= "blob\n" (:stdout (run ["cat-file" "-t" "HEAD:src/a.clj"]))))
      (is (zero? (:exit (run ["check-ignore" "target/output.bin"]))))
      (is (= 1 (:exit (run ["check-ignore" "src/a.clj"]))))
      (is (zero? (:exit (run ["checkout" "-q" "-b" "old" "HEAD"]))))
      (is (= "old\n" (:stdout (run ["branch" "--show-current"]))))
      (is (zero? (:exit (run ["branch" "-m" "renamed"]))))
      (is (= "renamed\n" (:stdout (run ["branch" "--show-current"]))))
      (repo/write! conn "src/a.clj" (.getBytes "(def   answer  42)\n" "UTF-8"))
      (is (= "" (:stdout (run ["diff" "-w" "--" "src/a.clj"]))))
      (finally (cleanup f)))))

(deftest stash-preserves-index-and-worktree-shapes
  (let [{:keys [conn] :as f} (fixture)
        run #(command/execute
              {:conn conn :root "/project" :config (atom {"user.name" "Ada"})
               :repo-relative identity} %)]
    (try
      (repo/init! conn)
      (doseq [path ["staged.txt" "unstaged.txt"]]
        (repo/write! conn path (.getBytes "base\n" "UTF-8")))
      (repo/stage-all! conn)
      (repo/commit! conn {:message "base" :author "Ada"})
      (repo/write! conn "staged.txt" (.getBytes "staged\n" "UTF-8"))
      (repo/stage! conn ["staged.txt"])
      (repo/write! conn "unstaged.txt" (.getBytes "unstaged\n" "UTF-8"))
      (is (zero? (:exit (run ["stash" "push" "-m" "agent work"]))))
      (is (:clean? (repo/status conn)))
      (is (str/includes? (:stdout (run ["stash" "list"])) "agent work"))
      (is (zero? (:exit (run ["stash" "pop"]))))
      (is (= "staged\n" (String. ^bytes (repo/read conn "staged.txt") "UTF-8")))
      (is (= "unstaged\n" (String. ^bytes (repo/read conn "unstaged.txt") "UTF-8")))
      (is (= ["staged.txt"] (:staged (repo/status conn))))
      (is (= ["unstaged.txt"] (:unstaged (repo/status conn))))
      (is (= "" (:stdout (run ["stash" "list"]))))
      (repo/stage-all! conn)
      (repo/commit! conn {:message "saved" :author "Ada"})
      (repo/write! conn "staged.txt" (.getBytes "selected v2\n" "UTF-8"))
      (repo/stage! conn ["staged.txt"])
      (repo/write! conn "unstaged.txt" (.getBytes "unselected v2\n" "UTF-8"))
      (is (zero? (:exit (run ["stash" "push" "--" "staged.txt"]))))
      (is (= "staged\n"
             (String. ^bytes (repo/read conn "staged.txt") "UTF-8")))
      (is (= "unselected v2\n"
             (String. ^bytes (repo/read conn "unstaged.txt") "UTF-8")))
      (is (zero? (:exit (run ["stash" "pop"]))))
      (is (= "selected v2\n"
             (String. ^bytes (repo/read conn "staged.txt") "UTF-8")))
      (is (= "unselected v2\n"
             (String. ^bytes (repo/read conn "unstaged.txt") "UTF-8")))
      (finally (cleanup f)))))

(deftest worktree-commands-delegate-to-the-host-workspace-adapter
  (let [{:keys [conn] :as f} (fixture)
        calls (atom [])
        records (atom [{:path "/project" :head "abc" :branch "refs/heads/main"}])
        run (fn [argv]
              (command/execute
               {:conn conn :root "/project" :config (atom {})
                :repo-relative identity
                :workspace-ops
                {:list (fn [] @records)
                 :add (fn [opts]
                        (swap! calls conj [:add opts])
                        (swap! records conj {:path (:path opts) :head "def"
                                             :branch "refs/heads/feature"}))
                 :remove (fn [opts] (swap! calls conj [:remove opts]))
                 :prune (fn [opts] (swap! calls conj [:prune opts]))}}
               argv))]
    (try
      (repo/init! conn)
      (is (zero? (:exit (run ["worktree" "add" "-b" "feature"
                              "/feature" "main"]))))
      (is (= [:add {:force? false :quiet? false :detach? false
                    :new-branch "feature" :reset-branch? false
                    :path "/feature" :target "main"}]
             (first @calls)))
      (is (str/includes? (:stdout (run ["worktree" "list" "--porcelain"]))
                         "worktree /feature\nHEAD def\nbranch refs/heads/feature"))
      (is (zero? (:exit (run ["worktree" "remove" "--force" "/feature"]))))
      (is (= [:remove {:path "/feature" :force? true}] (second @calls)))
      (is (zero? (:exit (run ["worktree" "prune" "-v"]))))
      (is (= [:prune {:verbose? true}] (nth @calls 2)))
      (finally (cleanup f)))))

(deftest clean-cherry-pick-and-rebase
  (let [{:keys [conn] :as f} (fixture)
        run #(command/execute
              {:conn conn :root "/project" :config (atom {"user.name" "Ada"})
               :repo-relative identity} %)
        commit-file! (fn [path text message]
                       (repo/write! conn path (.getBytes text "UTF-8"))
                       (repo/stage! conn [path])
                       (repo/commit! conn {:message message :author "Ada"}))]
    (try
      (repo/init! conn)
      (commit-file! "base.txt" "base\n" "base")
      (repo/branch! conn "feature")
      (repo/checkout! conn "feature")
      (commit-file! "feature.txt" "feature\n" "feature")
      (repo/checkout! conn "main")
      (commit-file! "main.txt" "main\n" "main")
      (repo/checkout! conn "feature")
      (is (zero? (:exit (run ["rebase" "main"]))))
      (is (= #{"base.txt" "feature.txt" "main.txt"}
             (set (repo/files conn))))
      (repo/checkout! conn "main")
      (is (zero? (:exit (run ["cherry-pick" "feature"]))))
      (is (= "feature\n"
             (String. ^bytes (repo/read conn "feature.txt") "UTF-8")))
      (finally (cleanup f)))))

(deftest clean-two-parent-merge-command
  (let [{:keys [conn] :as f} (fixture)
        run #(command/execute
              {:conn conn :root "/project"
               :config (atom {"user.name" "Ada"})
               :repo-relative identity} %)
        commit-file! (fn [path text message]
                       (repo/write! conn path (.getBytes text "UTF-8"))
                       (repo/stage! conn [path])
                       (repo/commit! conn {:message message :author "Ada"}))]
    (try
      (repo/init! conn)
      (commit-file! "base.txt" "base\n" "base")
      (repo/branch! conn "feature")
      (commit-file! "main.txt" "main\n" "main")
      (repo/checkout! conn "feature")
      (commit-file! "feature.txt" "feature\n" "feature")
      (repo/checkout! conn "main")
      (let [result (run ["merge" "feature"])]
        (is (zero? (:exit result)) (:stderr result))
        (is (str/includes? (:stdout result) "Merge 'feature'"))
        (is (= 2 (count (:geschichte.commit/parents
                         (repo/commit-by-id
                          conn (:geschichte.commit/id (repo/head-commit conn)))))))
        (is (= "feature\n" (String. ^bytes (repo/read conn "feature.txt")
                                    "UTF-8"))))
      (finally (cleanup f)))))
