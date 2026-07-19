(ns geschichte.git.compatibility
  "Executable compatibility ledger for Geschichte's Git-shaped command API.

  This is intentionally portable data. JVM hosts, Node/browser UIs, scenario
  tooling, and documentation can classify the same normalized argv shape.
  `:behavior` means the shape is intended to execute compatibly. `:transparent`
  means Geschichte recognizes the family but deliberately returns a fatal,
  non-mutating explanation rather than guessing."
  (:require [clojure.string :as str]))

(def implemented-options
  {"init" #{"-q" "--quiet" "-b" "--initial-branch" "--initial-branch=<value>"}
   "clone" #{"-q" "--quiet" "-b" "--branch" "--branch=<value>"
             "-o" "--origin" "--origin=<value>" "--depth" "--depth=<value>"}
   "config" #{"--global" "--local" "--get" "--unset" "--list" "-l"}
   "status" #{"-s" "--short" "--porcelain" "--porcelain=v1" "-b"
              "--branch" "-sb" "--ignored" "-u" "--untracked-files"
              "--untracked-files=<value>" "-uno" "-unormal" "-uall"}
   "add" #{"-A" "--all" "-u" "--update" "-f" "--force"}
   "commit" #{"-m" "--message" "--author" "-F" "--file" "-F-"
              "-a" "--all" "-q" "--quiet" "-am" "--no-verify"
              "--amend" "--no-edit"}
   "diff" #{"--cached" "--staged" "--quiet" "--exit-code" "--name-only"
            "--name-status" "--stat" "--shortstat" "--numstat" "--raw" "--binary"
            "--full-index" "-p" "--patch" "--check"
            "-u" "--patch-with-raw" "--patch-with-stat" "-z" "-R"
            "--abbrev" "--abbrev=<value>"
            "--no-index"
            "--diff-filter" "-U" "-U<n>" "--unified" "--unified=<value>"
            "-M" "-M<n>" "--find-renames" "--find-renames=<value>"
            "--no-renames"
            "--no-color" "--color" "--color=<value>"
            "--diff-filter=<value>" "-w" "--ignore-all-space"
            "-b" "--ignore-space-change" "--ignore-space-at-eol"}
   "log" #{"--oneline" "--stat" "--all" "-n" "--max-count" "-<n>"
           "--max-count=<value>" "--format=<value>" "--pretty=<value>"
           "--grep" "--grep=<value>" "-S" "-i" "--regexp-ignore-case"
           "--all-match" "--reverse" "-p" "--patch" "--follow"
           "--name-only" "--name-status" "--diff-filter"
           "--diff-filter=<value>"}
   "show" #{"--no-patch" "-s" "--oneline" "--stat" "--format=<value>"
            "--pretty=<value>" "--name-only" "--diff-filter"
            "--diff-filter=<value>"}
   "grep" #{"-n" "--line-number" "-E" "--extended-regexp" "-F"
            "--fixed-strings" "-i" "--ignore-case" "-l"
            "--files-with-matches" "-q" "--quiet" "-c" "--count" "-I"
            "-w" "--word-regexp" "-r"}
   "ls-files" #{"-z" "-o" "--others" "-i" "--ignored"
                "--exclude-standard" "--error-unmatch" "-s" "--stage"}
   "ls-tree" #{"-r" "--recursive" "--name-only" "--name-status" "-z"}
   "check-ignore" #{"-v" "--verbose" "-q" "--quiet"}
   "rev-list" #{"--count" "--left-right" "--max-count" "--max-count=<value>"
                "-n" "-<n>" "-n1" "--all"}
   "cat-file" #{"-e" "-t" "-p"}
   "describe" #{"--tags" "--always" "--abbrev" "--abbrev=<value>"}
   "symbolic-ref" #{"--short" "-q"}
   "for-each-ref" #{"--format" "--format=<value>" "--sort"
                    "--sort=<value>" "--count" "--count=<value>"}
   "show-ref" #{"--heads" "--tags" "--verify" "--quiet" "-q"}
   "branch" #{"--show-current" "-d" "-D" "--delete" "-a" "--all"
              "-r" "--remotes" "-v" "-vv" "--verbose" "--contains"
              "--sort" "--sort=<value>" "--format" "--format=<value>"
              "--list" "-m" "-M" "-q" "--quiet" "-f" "--force"}
   "tag" #{"-d" "--delete" "-l" "--list" "--sort" "--sort=<value>"
           "--contains" "--points-at"}
   "merge" #{"--ff-only" "--no-commit" "--no-edit" "-m" "--message"}
   "merge-base" #{"--is-ancestor"}
   "checkout" #{"-f" "--force" "-q" "--quiet" "-b" "-B" "-c" "-C"}
   "switch" #{"-f" "--force" "-q" "--quiet" "-b" "-B" "-c" "-C"}
   "restore" #{"--source" "--staged" "--worktree"}
   "reset" #{"--soft" "--mixed" "--hard" "-q" "--quiet"}
   "rm" #{"--cached" "-q" "--quiet" "-r" "-f" "--force"
          "--ignore-unmatch" "-rq" "-rf" "-rfq" "-qf" "-fq"}
   "mv" #{}
   "clean" #{"-n" "--dry-run" "-f" "--force" "-d"}
   "stash" #{"-q" "--quiet" "-u" "--include-untracked" "-m" "--message"
             "-p" "--patch" "--stat"}
   "cherry-pick" #{"-x" "--no-commit" "-n"}
   "rebase" #{"--onto" "--continue" "--skip" "--abort" "--quit"
              "-q" "--quiet"}
   "rev-parse" #{"--show-toplevel" "--is-inside-work-tree" "--abbrev-ref"
                 "--symbolic-full-name"
                 "--short" "--short=<value>" "--verify" "--quiet" "-q"
                 "--git-dir" "--show-prefix" "--show-cdup"}
   "remote" #{"-v" "--verbose"}
   "fetch" #{"-q" "--quiet" "--all" "--prune" "--tags" "--no-tags"}
   "pull" #{"--ff-only" "-q" "--quiet"}
   "push" #{"-u" "--set-upstream" "-q" "--quiet" "-f" "--force"
            "--force-with-lease" "--delete"}
   "ls-remote" #{"--heads" "-h" "--tags" "--refs" "--symref"
                 "--exit-code"}
   "worktree" #{"--porcelain" "-f" "--force" "-q" "--quiet"
                "-b" "-B" "--detach" "-v" "--verbose"}})

(def action-operands
  {"stash" #{"push" "pop" "apply" "list" "show" "drop" "clear"}
   "remote" #{"add" "remove" "rm" "get-url" "set-url"}
   "worktree" #{"add" "list" "prune" "remove"}})

(def action-universe
  {"stash" #{"push" "pop" "apply" "list" "show" "drop" "clear"
             "create" "store" "branch"}
   "remote" #{"add" "remove" "rm" "rename" "get-url" "set-url"
              "show" "prune" "update"}
   "worktree" #{"add" "list" "lock" "move" "prune" "remove" "repair"
                "unlock"}})

(defn- option? [token]
  (and (string? token) (str/starts-with? token "-") (not= token "--")))

(defn- accepted-option? [command allowed option]
  (or (contains? allowed option)
      (when-let [equals (str/index-of option "=")]
        (contains? allowed (str (subs option 0 equals) "=<value>")))
      (and (= command "grep") (boolean (re-matches #"-[nEFilqcrIw]+" option)))
      (and (= command "commit") (boolean (re-matches #"-[aq]*F-?" option)))
      (and (= command "diff") (boolean (re-matches #"-U\d+" option)))
      (and (= command "diff") (boolean (re-matches #"-M\d*%?" option)))
      (and (= command "push") (boolean (re-matches #"-[qfu]+" option)))
      (and (= command "log") (boolean (re-matches #"-[0-9]+" option)))
      (and (= command "log") (str/starts-with? option "-S"))
      (and (= command "rev-list") (boolean (re-matches #"-n[0-9]+" option)))
      (and (= command "rm") (boolean (re-matches #"-[qrf]+" option)))))

(defn classify-shape
  "Classify a normalized shape such as `[\"commit\" \"-m\" \"<arg>\"]`."
  [[command & args :as shape]]
  (let [allowed (get implemented-options command)
        action-set (get action-operands command)
        candidate (when action-set (first args))
        action (when (contains? (get action-universe command) candidate)
                 candidate)
        options (filter option? args)
        unknown-options (remove #(accepted-option? command allowed %) options)]
    (cond
      (nil? allowed)
      {:status :transparent :reason :command-not-implemented :shape shape}

      (seq unknown-options)
      {:status :transparent :reason :unsupported-options
       :options (vec unknown-options) :shape shape}

      (and action (not (contains? action-set action)))
      {:status :transparent :reason :unsupported-action :action action :shape shape}

      (and (= command "worktree") (nil? action))
      {:status :transparent :reason :unsupported-action :action nil :shape shape}

      (and (= command "worktree") (some #{"--detach"} args))
      {:status :transparent :reason :detached-worktree-not-implemented
       :shape shape}

      (and (= command "clone")
           (some #(or (= "--depth" %)
                      (str/starts-with? % "--depth=")) args))
      {:status :transparent :reason :shallow-history-not-implemented
       :shape shape}

      (and (= command "fetch") (some #{"--all" "--prune"} args))
      {:status :transparent :reason :fetch-ref-maintenance-not-implemented
       :shape shape}

      (and (= command "push") (some #{"--force-with-lease"} args))
      {:status :transparent :reason :force-lease-not-implemented
       :shape shape}

      (and (= command "rebase")
           (some #{"--continue" "--skip" "--abort" "--quit"} args))
      {:status :transparent :reason :rebase-state-not-present :shape shape}

      :else {:status :behavior :shape shape})))
