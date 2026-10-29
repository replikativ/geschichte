(ns geschichte.git-ssh-test
  (:require [clojure.test :refer [deftest is]]
            [geschichte.git.ssh :as ssh]
            [geschichte.git.client :as client]
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
          (let [cloned (ssh/clone! conn "origin" argv-fn nil)
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
            (repo/write! conn "draft.md" (.getBytes "local draft\n" "UTF-8"))
            (let [fetched (ssh/fetch! conn "origin" argv-fn {:haves haves})]
              (is (thrown-with-msg? clojure.lang.ExceptionInfo #"clean worktree"
                                    (client/apply-pull! conn "origin" fetched)))
              (is (= "local draft\n" (String. ^bytes (repo/read conn "draft.md") "UTF-8")))))
          (finally (d/release conn))))
      (finally
        (when (d/database-exists? cfg) (d/delete-database cfg))
        (doseq [file (reverse (file-seq root))] (.delete ^java.io.File file))))))
