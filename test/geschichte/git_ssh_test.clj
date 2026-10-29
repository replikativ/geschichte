(ns geschichte.git-ssh-test
  (:require [clojure.test :refer [deftest is]]
            [geschichte.git.ssh :as ssh]
            [geschichte.git.client :as client]
            [geschichte.git.transport :as transport]
            [geschichte.repo :as repo]
            [datahike.api :as d]))

(deftest ssh-and-local-argv-are-explicit
  (is (= ["ssh" "-p" "2222" "-i" "/key" "-o" "BatchMode=yes"
          "-o" "SendEnv=GIT_PROTOCOL" "git@example.test"
          "git-upload-pack '/repo with spaces.git'"]
         (ssh/ssh-argv
          {:host "git@example.test" :path "/repo with spaces.git"
           :port 2222 :identity-file "/key" :options ["-o" "BatchMode=yes"]})))
  (is (= ["git" "upload-pack" "/tmp/repo"]
         (ssh/local-argv {:path "/tmp/repo"}))))

(deftest native-upload-pack-incremental-fetch
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "geschichte-remote-sync-"
                       (make-array java.nio.file.attribute.FileAttribute 0)))
        git! (fn [& args]
               (let [process (-> (ProcessBuilder. ^java.util.List
                                  (into ["git" "-c" "core.hooksPath=/dev/null"
                                         "-c" "commit.gpgsign=false"] args))
                                 (.directory root) (.redirectErrorStream true) .start)
                     output (slurp (.getInputStream process))]
                 (when-not (zero? (.waitFor process))
                   (throw (ex-info "Git fixture failed" {:args args :output output})))
                 output))
        cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? true :commit-graph? true}]
    (try
      (git! "init" "-q" "-b" "main")
      (git! "config" "user.name" "Ada")
      (git! "config" "user.email" "ada@example.test")
      (spit (java.io.File. root "README.md") "one\n")
      (git! "add" "README.md")
      (git! "commit" "-q" "-m" "base")
      (d/create-database cfg)
      (let [conn (d/connect cfg)
            argv-fn #(ssh/local-argv {:path (.getPath root) :service %})]
        (try
          (repo/init! conn)
          (let [cloned (ssh/clone! conn "origin" argv-fn {:ref "refs/heads/local"})
                haves [(:git-oid cloned)]
                unchanged (ssh/fetch! conn "origin" argv-fn {:haves haves})]
            (is (= 0 (:persisted unchanged)))
            (spit (java.io.File. root "notes.md") "two\n")
            (git! "add" "notes.md")
            (git! "commit" "-q" "-m" "update")
            (let [fetched (ssh/fetch! conn "origin" argv-fn {:haves haves})]
              (is (= 3 (:persisted fetched)))
              (is (nil? (repo/read conn "notes.md")))
              (is (= :fast-forward (:pull/status (client/apply-pull! conn "origin" fetched))))
              (is (= "one\n" (String. ^bytes (repo/read conn "README.md") "UTF-8")))
              (is (= "two\n" (String. ^bytes (repo/read conn "notes.md") "UTF-8"))))
            (repo/set-config! conn "branch.local.remote" "origin")
            (repo/set-config! conn "branch.local.merge" "refs/heads/main")
            (let [pull! (fn [options]
                          (with-redefs [ssh/ssh-argv (fn [{:keys [service]}] (argv-fn service))]
                            (transport/pull! {:conn conn :remote "origin"
                                              :url "git@example.test:repo.git"
                                              :options options})))
                  outcome (fn [options]
                            (try (pull! options)
                                 (catch clojure.lang.ExceptionInfo error (ex-data error))))]
              ;; Changing the remote's default branch must not redirect sync.
              (git! "switch" "-q" "-c" "docs")
              (spit (java.io.File. root "docs.md") "different branch\n")
              (git! "add" "docs.md")
              (git! "commit" "-q" "-m" "docs")
              (is (= :up-to-date (:pull/status (pull! nil))))
              (is (= "refs/heads/local" (repo/current-ref conn)))
              (is (nil? (repo/read conn "docs.md")))
              (git! "switch" "-q" "main")
              (spit (java.io.File. root "main.md") "tracked branch\n")
              (git! "add" "main.md")
              (git! "commit" "-q" "-m" "main update")
              (git! "switch" "-q" "docs")
              (is (= :fast-forward (:pull/status (pull! nil))))
              (is (= "refs/heads/local" (repo/current-ref conn)))
              (is (= "tracked branch\n" (String. ^bytes (repo/read conn "main.md") "UTF-8")))
              (is (nil? (repo/read conn "docs.md")))
              (let [before (repo/worktree conn)]
                (is (= :diverged (:pull/status (outcome {:branch "docs"}))))
                (is (= before (repo/worktree conn))))
              (git! "branch" "-D" "main")
              (is (= :missing-branch (:pull/status (outcome nil))))
              (is (nil? (repo/read conn "docs.md")))
              (repo/write! conn "draft.md" (.getBytes "local draft\n" "UTF-8"))
              (is (= :local-changes (:pull/status (outcome nil)))))
            (repo/write! conn "draft.md" (.getBytes "local draft\n" "UTF-8"))
            (let [fetched (ssh/fetch! conn "origin" argv-fn {:haves haves})]
              (is (thrown-with-msg? clojure.lang.ExceptionInfo #"clean worktree"
                                    (client/apply-pull! conn "origin" fetched)))
              (is (= "local draft\n" (String. ^bytes (repo/read conn "draft.md") "UTF-8")))))
          (finally (d/release conn))))
      (finally
        (when (d/database-exists? cfg) (d/delete-database cfg))
        (doseq [file (reverse (file-seq root))] (.delete ^java.io.File file))))))
