(ns geschichte.git-ssh-test
  (:require [clojure.test :refer [deftest is]]
            [geschichte.git.ssh :as ssh]))

(deftest ssh-and-local-argv-are-explicit
  (is (= ["ssh" "-p" "2222" "-i" "/key" "-o" "BatchMode=yes"
          "-o" "SendEnv=GIT_PROTOCOL" "git@example.test"
          "git-upload-pack '/repo with spaces.git'"]
         (ssh/ssh-argv
          {:host "git@example.test" :path "/repo with spaces.git"
           :port 2222 :identity-file "/key" :options ["-o" "BatchMode=yes"]})))
  (is (= ["git" "upload-pack" "/tmp/repo"]
         (ssh/local-argv {:path "/tmp/repo"}))))
