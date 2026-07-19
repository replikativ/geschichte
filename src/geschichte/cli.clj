(ns geschichte.cli
  "Native Geschichte and Git-compatible command-line entrypoint."
  (:gen-class)
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.query.execute]
            [geschichte.git.command :as git-command]
            [geschichte.git.local :as git-local]
            [geschichte.git.no-index :as git-no-index]
            [geschichte.git.transport :as git-transport]
            [geschichte.projection :as projection]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [geschichte.workspace-physical :as physical-workspace])
  (:import [java.nio.file Files Path]))

(set! *warn-on-reflection* true)

(def ^:private command-specs
  [{:path ["init"] :category :start :synopsis "init [-q] [-b BRANCH] [DIRECTORY]"
    :summary "Create a Geschichte repository and physical projection."}
   {:path ["clone"] :category :start
    :synopsis "clone [-q] [-n] [-o NAME] [-b BRANCH] REPOSITORY [DIRECTORY]"
    :summary "Clone a Git or Geschichte repository into a new projection."}
   {:path ["import-git"] :category :start
    :synopsis "import-git [-q] [-f] [-o NAME] [-b BRANCH] SOURCE [DIRECTORY]"
    :summary "Import refs, commits and content directly from a local .git repository."}

   {:path ["status"] :category :change :synopsis "status [-s|--short] [--porcelain[=VERSION]]"
    :summary "Show changes between the workspace, index and current commit."}
   {:path ["add"] :category :change :synopsis "add [-A|-u] [--] [PATHSPEC...]"
    :summary "Stage workspace content for the next commit."}
   {:path ["commit"] :category :change
    :synopsis "commit [-a] [-m MESSAGE | -F FILE] [--amend] [--allow-empty]"
    :summary "Record staged content as a new commit."}
   {:path ["diff"] :category :change
    :synopsis "diff [--cached|--staged] [--stat|--name-only|--name-status] [REV...] [-- PATH...]"
    :summary "Compare workspace, index, commits or trees."}
   {:path ["restore"] :category :change
    :synopsis "restore [--staged] [--source REV] [--] PATHSPEC..."
    :summary "Restore workspace or staged paths from another tree."}
   {:path ["reset"] :category :change :synopsis "reset [--soft|--mixed|--hard] [REV] [-- PATH...]"
    :summary "Move HEAD or reset selected paths."}
   {:path ["rm"] :category :change :synopsis "rm [-r] [-f] [--cached] [--] PATHSPEC..."
    :summary "Remove paths from the workspace and/or index."}
   {:path ["mv"] :category :change :synopsis "mv [-f] SOURCE DESTINATION"
    :summary "Move or rename a tracked path."}
   {:path ["clean"] :category :change :synopsis "clean [-n|-f] [-d] [-x|-X] [PATH...]"
    :summary "Remove untracked files from the physical projection."}

   {:path ["log"] :category :inspect
    :synopsis "log [-N] [--oneline] [--format FORMAT] [--all] [REV] [-- PATH...]"
    :summary "Show commit history."}
   {:path ["show"] :category :inspect :synopsis "show [--format FORMAT] [--stat] [OBJECT...]"
    :summary "Show commits, trees or blobs."}
   {:path ["grep"] :category :inspect :synopsis "grep [-n] [-i] [-l] PATTERN [REV] [-- PATH...]"
    :summary "Search tracked content."}
   {:path ["describe"] :category :inspect :synopsis "describe [--tags] [--always] [REV]"
    :summary "Name a commit relative to a nearby tag."}
   {:path ["rev-parse"] :category :inspect :synopsis "rev-parse [OPTIONS] [REV...]"
    :summary "Resolve revisions and inspect repository context."}
   {:path ["rev-list"] :category :inspect :synopsis "rev-list [OPTIONS] REV... [-- PATH...]"
    :summary "List commits reachable from revisions."}
   {:path ["cat-file"] :category :inspect :synopsis "cat-file (-t|-s|-p|--batch) OBJECT"
    :summary "Inspect Git-compatible object representations."}
   {:path ["ls-files"] :category :inspect :synopsis "ls-files [-c|-m|-o|-d] [--stage] [-- PATH...]"
    :summary "List paths in the index or workspace."}
   {:path ["ls-tree"] :category :inspect :synopsis "ls-tree [-r] [-d] [-l] [--name-only] TREE [PATH...]"
    :summary "List the contents of a tree."}
   {:path ["check-ignore"] :category :inspect :synopsis "check-ignore [-q|-v] [--stdin] PATH..."
    :summary "Test paths against ignore rules."}
   {:path ["for-each-ref"] :category :inspect :synopsis "for-each-ref [--format FORMAT] [PATTERN...]"
    :summary "Format and list refs."}
   {:path ["show-ref"] :category :inspect :synopsis "show-ref [--heads] [--tags] [--verify] [PATTERN...]"
    :summary "List or verify refs."}
   {:path ["symbolic-ref"] :category :inspect :synopsis "symbolic-ref [-q] [--short] NAME [REF]"
    :summary "Read or update symbolic refs."}

   {:path ["branch"] :category :history :synopsis "branch [-a|-r] [-d|-D] [-m] [NAME [START]]"
    :summary "List, create, move or delete branches."}
   {:path ["tag"] :category :history :synopsis "tag [-l] [-d] [-f] [NAME [REV]]"
    :summary "List, create or delete tags."}
   {:path ["checkout"] :category :history :synopsis "checkout [-b|-B] BRANCH [START] | checkout [REV] -- PATH..."
    :summary "Switch branches or restore paths."}
   {:path ["switch"] :category :history :synopsis "switch [-c|-C] [--detach] BRANCH"
    :summary "Switch the current workspace branch."}
   {:path ["merge"] :category :history :synopsis "merge [--ff-only|--no-ff] [-m MESSAGE] REV"
    :summary "Join histories with a fast-forward or merge commit."}
   {:path ["merge-base"] :category :history :synopsis "merge-base [--is-ancestor] REV REV"
    :summary "Find a best common ancestor."}
   {:path ["stash"] :category :history :synopsis "stash [push|list|show|pop|apply|drop|clear] [OPTIONS]"
    :summary "Store and restore uncommitted changes."}
   {:path ["cherry-pick"] :category :history :synopsis "cherry-pick [--no-commit] REV..."
    :summary "Apply changes introduced by existing commits."}
   {:path ["rebase"] :category :history :synopsis "rebase [--onto NEWBASE] UPSTREAM [BRANCH]"
    :summary "Replay commits onto another base."}
   {:path ["worktree"] :category :history :synopsis "worktree (list|add|remove|prune) [OPTIONS]"
    :summary "Manage isolated physical Geschichte workspaces."}

   {:path ["config"] :category :collaborate :synopsis "config [--get|--add|--unset|--list] [NAME [VALUE]]"
    :summary "Read and write repository configuration."}
   {:path ["remote"] :category :collaborate :synopsis "remote [-v] | remote (add|remove|get-url|set-url) ..."
    :summary "Manage named remote repositories."}
   {:path ["fetch"] :category :collaborate :synopsis "fetch [-q] [--tags|--no-tags] [REMOTE [REFSPEC]]"
    :summary "Download remote history without advancing the workspace."}
   {:path ["pull"] :category :collaborate :synopsis "pull [--ff-only] [-q] [REMOTE]"
    :summary "Fetch and fast-forward the current branch."}
   {:path ["push"] :category :collaborate :synopsis "push [-u] [-f] [--delete] [REMOTE [REFSPEC...]]"
    :summary "Publish refs to a remote repository."}
   {:path ["ls-remote"] :category :collaborate :synopsis "ls-remote [--heads|--tags] REPOSITORY [PATTERN...]"
    :summary "List refs available in a remote repository."}

   {:path ["db"] :category :native :synopsis "db COMMAND [ARGS...]"
    :summary "Access the underlying queryable repository database."
    :children [["db" "query"]]}
   {:path ["db" "query"] :category :native :synopsis "db query DATALOG"
    :summary "Evaluate a Datalog query against the repository database."}
   {:path ["repo"] :category :native :synopsis "repo (status|refs|log)"
    :summary "Inspect Geschichte's structured repository model."
    :children [["repo" "status"] ["repo" "refs"] ["repo" "log"]]}
   {:path ["repo" "status"] :category :native :synopsis "repo status"
    :summary "Print structured three-tree status as EDN."}
   {:path ["repo" "refs"] :category :native :synopsis "repo refs"
    :summary "Print logical Geschichte refs as EDN."}
   {:path ["repo" "log"] :category :native :synopsis "repo log"
    :summary "Print reachable commit metadata as EDN."}
   {:path ["workspace"] :category :native :synopsis "workspace (list|publish|advance)"
    :summary "Inspect and coordinate isolated physical workspaces."
    :children [["workspace" "list"] ["workspace" "publish"]
               ["workspace" "advance"]]}
   {:path ["workspace" "list"] :category :native :synopsis "workspace list"
    :summary "Print registered physical workspace metadata as EDN."}
   {:path ["workspace" "publish"] :category :native :synopsis "workspace publish"
    :summary "Publish this workspace ref to its canonical branch."}
   {:path ["workspace" "advance"] :category :native :synopsis "workspace advance"
    :summary "Advance this workspace from its canonical branch."}])

(def ^:private category-labels
  [[:start "Start or import a repository"]
   [:change "Record and shape changes"]
   [:inspect "Inspect history and content"]
   [:history "Branch and combine history"]
   [:collaborate "Configure and collaborate"]
   [:native "Queryable Geschichte operations"]])

(def ^:private command-aliases
  {["query"] ["db" "query"]
   ["refs"] ["repo" "refs"]})

(defn- command-name [{:keys [path]}]
  (str/join " " path))

(defn- command-index []
  (into {} (map (juxt :path identity)) command-specs))

(defn usage []
  (str "●━━●━┯━●  Geschichte\n"
       "     └━●  queryable version control\n\n"
       "Usage: ges [--repo FILE] [-C DIRECTORY] COMMAND [ARGS...]\n"
       "       ges help [COMMAND]\n\n"
       (apply str
              (for [[category label] category-labels
                    :let [commands (filter #(and (= category (:category %))
                                                 (= 1 (count (:path %))))
                                           command-specs)]
                    :when (seq commands)]
                (str label ":\n"
                     (apply str
                            (for [{:keys [summary] :as command} commands]
                              (format "  %-14s %s\n" (command-name command) summary)))
                     "\n")))
       "Global options:\n"
       "  -C DIRECTORY        run as if started in DIRECTORY; may be repeated\n"
       "  -c NAME=VALUE       set command-scoped Git configuration\n"
       "  -r, --repo FILE     use an explicit repo.edn or Datahike config\n"
       "  -h, --help          show this help\n\n"
       "Run 'ges help COMMAND' or 'ges COMMAND --help' for command help.\n"))

(defn- command-usage [path]
  (let [path (get command-aliases path path)
        spec (get (command-index) path)]
    (when spec
      (str "●━━●━┯━●  Geschichte\n"
           "     └━●  " (command-name spec) "\n\n"
           "Usage: ges " (:synopsis spec) "\n\n"
           (:summary spec) "\n"
           (when-let [children (seq (:children spec))]
             (str "\nCommands:\n"
                  (apply str
                         (for [child children
                               :let [child-spec (get (command-index) child)]]
                           (format "  %-20s %s\n"
                                   (command-name child-spec)
                                   (:summary child-spec))))))
           "\nRun 'ges help' to list all commands.\n"))))

(defn- help-request
  "Return the command path requested by either supported help spelling.
  An empty path denotes top-level help; nil means normal command execution."
  [argv]
  (cond
    (empty? argv) []
    (= "help" (first argv)) (vec (rest argv))
    (some #{"-h" "--help"} argv)
    (vec (take-while #(not (contains? #{"-h" "--help"} %)) argv))
    :else nil))

(defn- help-result [path]
  (if (empty? path)
    {:stdout (usage) :stderr "" :exit 0}
    (if-let [text (command-usage path)]
      {:stdout text :stderr "" :exit 0}
      {:stdout ""
       :stderr (str "ges: no help for '" (str/join " " path) "'\n"
                    "Run 'ges help' to list commands.\n")
       :exit 129})))

(defn- command-argv
  "Remove host-resolved Git globals for command discovery. The transitional
  `ges git` spelling also accepts globals after `git`."
  [argv]
  (let [argv (:args (git-command/parse-global argv))]
    (if (= "git" (first argv))
      (:args (git-command/parse-global (vec (rest argv))))
      argv)))

(defn- host-options
  "Remove Geschichte-only global options without consuming Git options."
  [argv]
  (loop [remaining (vec argv), result [], options {}]
    (if-let [arg (first remaining)]
      (cond
        (contains? #{"-r" "--repo"} arg)
        (if-let [value (second remaining)]
          (recur (subvec remaining 2) result (assoc options :repo value))
          (throw (ex-info (str "option requires an argument: " arg)
                          {:exit 129 :kind :usage})))

        (str/starts-with? arg "--repo=")
        (recur (subvec remaining 1) result
               (assoc options :repo (subs arg (count "--repo="))))

        (and (empty? result) (contains? #{"-h" "--help"} arg))
        (recur (subvec remaining 1) result (assoc options :help true))

        :else (recur (subvec remaining 1) (conj result arg) options))
      {:argv result :options options})))

(defn- apply-directories [cwd directories]
  (reduce (fn [^java.io.File directory child]
            (let [^java.io.File child-file (io/file child)]
              (.getCanonicalFile
               (if (.isAbsolute child-file)
                 child-file
                 (io/file directory child)))))
          (.getCanonicalFile ^java.io.File (io/file cwd)) directories))

(defn- resolve-local-path [cwd path]
  (let [^java.io.File path (io/file path)]
    (.getCanonicalFile
     (if (.isAbsolute path) path (io/file cwd path)))))

(defn- repo-relative
  ([root path] (repo-relative root root path))
  ([root cwd path]
   (let [root (.normalize (.toAbsolutePath (.toPath (io/file root))))
         target (.normalize (.toAbsolutePath (.toPath (io/file cwd (str path)))))]
     (when (.startsWith target root)
       (-> (.toString (.relativize root target))
           (str/replace #"\\" "/"))))))

(defn- connect! [config]
  (when-not (d/database-exists? config)
    (throw (ex-info "Geschichte repository is not initialized"
                    {:exit 128 :kind :repository-not-found})))
  (d/connect config))

(defn- with-connection [config f]
  (let [conn (connect! config)]
    (try (f conn) (finally (d/release conn)))))

(defn- branch-ref [branch]
  (if (or (nil? branch) (str/starts-with? branch "refs/"))
    branch
    (str "refs/heads/" branch)))

(defn- initialize-database! [config branch]
  (when-not (d/database-exists? config) (d/create-database config))
  (let [conn (d/connect config)]
    (try
      (repo/init! conn (cond-> {} branch (assoc :branch (branch-ref branch))))
      conn
      (catch Throwable error
        (d/release conn)
        (throw error)))))

(defn- init-command [cwd explicit args]
  (let [{:keys [path branch quiet?]} (git-command/parse-init args)
        ^java.io.File root (resolve-local-path cwd path)
        _ (when (and (nil? explicit)
                     (not (.isFile (projection/marker-file root)))
                     (git-local/discover root))
            (throw (ex-info
                    "native Git repository detected; use 'ges import-git . --force' to preserve its history"
                    {:exit 128 :kind :native-git-detected
                     :root (.getPath root)})))
        locator (if explicit
                  (assoc (projection/read-marker explicit) :root root)
                  (projection/initialize-projection! root))
        initial-conn (initialize-database! (:config locator) branch)
        locator (try
                  (physical-workspace/ensure-registered! locator)
                  (finally (d/release initial-conn)))
        conn (connect! (:config locator))]
    (try
      (when (:projection? locator) (projection/scan! conn root))
      {:stdout (if quiet? ""
                   (str "Initialized empty Geschichte repository in "
                        (.getPath (projection/marker-file root)) "\n"))
       :stderr "" :exit 0}
      (finally (d/release conn)))))

(defn- materialize-imported-projection! [locator root]
  (let [locator (physical-workspace/ensure-registered! locator)
        projection-conn (connect! (:config locator))]
    (try
      (when (:projection? locator)
        (projection/materialize-changes! projection-conn root {}
                                         (repo/worktree projection-conn)))
      locator
      (finally (d/release projection-conn)))))

(defn- delete-tree! [^java.io.File root]
  (when (.exists root)
    (with-open [paths (Files/walk (.toPath root)
                                  (make-array java.nio.file.FileVisitOption 0))]
      (doseq [^Path path (reverse (sort-by #(.getNameCount ^Path %)
                                           (iterator-seq (.iterator paths))))]
        (Files/deleteIfExists path)))))

(defn- profile-enabled? [config]
  (contains? #{"1" "true" "yes" "on"}
             (some-> (get config "geschichte.profile") str/lower-case)))

(defn- profile-reporter [config]
  (when (profile-enabled? config)
    (fn [{:keys [event phase duration-ms completed total round]}]
      (binding [*out* *err*]
        (case event
          :start (println (str "ges profile: " (name phase) " started"))
          :complete
          (println (format "ges profile: %s completed in %.1f ms"
                           (name phase) (double duration-ms)))
          :progress
          (println (str "ges profile: " (name phase) " " completed "/"
                        total " (round " round ")"))
          nil)
        (flush)))))

(def ^:private ingestion-long-options
  {"geschichte.git.primitive-index-threshold" :primitive-index-threshold
   "geschichte.git.delta-parallelism" :delta-parallelism
   "geschichte.git.delta-frontier-bytes" :delta-frontier-bytes
   "geschichte.git.max-pack-index-memory-bytes" :max-pack-index-memory-bytes
   "geschichte.git.pack-chunk-size" :pack-chunk-size})

(defn- ingestion-options [config]
  (cond->
   (reduce-kv
    (fn [options config-key option-key]
      (if-let [value (get config config-key)]
        (let [parsed (parse-long value)]
          (when-not (and (some? parsed) (not (neg? parsed)))
            (throw (ex-info (str "invalid non-negative integer for " config-key)
                            {:exit 129 :key config-key :value value})))
          (assoc options option-key parsed))
        options))
    {} ingestion-long-options)
    (get config "geschichte.git.delta-spill-directory")
    (assoc :delta-spill-directory
           (get config "geschichte.git.delta-spill-directory"))))

(defn- clone-command [cwd explicit config-overrides args]
  (let [{:keys [url path origin branch quiet? no-checkout?] :as options}
        (git-command/parse-clone args)
        reporter (profile-reporter config-overrides)
        ingestion (ingestion-options config-overrides)
        ^java.io.File root (resolve-local-path cwd path)
        existed? (.exists root)]
    (when (and (.exists root) (seq (.list root)))
      (throw (ex-info (str "destination path '" path
                           "' already exists and is not an empty directory.")
                      {:exit 128 :kind :destination-exists})))
    (let [locator (if explicit
                    (assoc (projection/read-marker explicit) :root root)
                    (projection/initialize-projection! root))]
      (try
        (let [conn (initialize-database! (:config locator) nil)]
          (try
            (if-let [local-source (git-local/source-file cwd url)]
              (git-local/import! conn local-source
                                 (cond-> (assoc ingestion
                                                :remote origin :branch branch
                                                :clone? true
                                                :no-checkout? no-checkout?)
                                   reporter (assoc :phase-fn reporter)))
              ((:clone git-transport/operations)
               {:conn conn :remote origin :url url
                :options (cond-> ingestion
                           branch (assoc :branch branch)
                           no-checkout? (assoc :no-checkout? true)
                           reporter (assoc :phase-fn reporter))}))
            (when-not no-checkout?
              (materialize-imported-projection! locator root))
            {:stdout ""
             :stderr (if quiet? "" (str "Cloning into '" path "'...\n"))
             :exit 0}
            (finally (d/release conn))))
        (catch Throwable error
          ;; A newly-created destination must never look like a usable partial
          ;; clone. Its pack chunks may have been written before atomic pack/ref
          ;; publication, so delete the local database and projection together.
          (when-not explicit
            (try (when (d/database-exists? (:config locator))
                   (d/delete-database (:config locator)))
                 (catch Throwable _))
            (try (delete-tree! root) (catch Throwable _))
            (when existed? (.mkdirs root)))
          (throw error))))))

(defn- parse-import-git [args]
  (loop [args args, options {:origin "origin"}, operands []]
    (if-let [arg (first args)]
      (cond
        (contains? #{"-q" "--quiet"} arg)
        (recur (next args) (assoc options :quiet? true) operands)

        (contains? #{"-f" "--force"} arg)
        (recur (next args) (assoc options :force? true) operands)

        (contains? #{"-b" "--branch" "-o" "--origin"} arg)
        (if-let [value (second args)]
          (recur (nnext args)
                 (assoc options
                        (if (contains? #{"-b" "--branch"} arg)
                          :branch :origin)
                        value)
                 operands)
          (throw (ex-info (str "option requires an argument: " arg)
                          {:exit 129 :kind :usage})))

        (str/starts-with? arg "--branch=")
        (recur (next args)
               (assoc options :branch (subs arg (count "--branch="))) operands)

        (str/starts-with? arg "--origin=")
        (recur (next args)
               (assoc options :origin (subs arg (count "--origin="))) operands)

        (str/starts-with? arg "-")
        (throw (ex-info (str "unknown option `" arg "'")
                        {:exit 129 :kind :usage}))

        :else (recur (next args) options (conj operands arg)))
      (let [[source destination & extra] operands]
        (when (seq extra)
          (throw (ex-info "usage: ges import-git [options] SOURCE [DIRECTORY]"
                          {:exit 129 :kind :usage})))
        (assoc options :source (or source ".") :destination destination)))))

(defn- import-git-command [cwd explicit args]
  (when explicit
    (throw (ex-info "--repo cannot select the destination of import-git"
                    {:exit 129 :kind :usage})))
  (let [{:keys [source destination force? quiet? origin branch]}
        (parse-import-git args)
        source-file (or (git-local/source-file cwd source)
                        (throw (ex-info "import-git requires a local path or file:// URL"
                                        {:exit 129 :kind :usage :source source})))
        repository (git-local/require-repository source-file)
        in-place? (nil? destination)
        ^java.io.File root (if destination
                             (resolve-local-path cwd destination)
                             (or (:work-tree repository)
                                 (throw (ex-info
                                         "a destination is required for a bare repository"
                                         {:exit 129 :kind :usage}))))]
    (when (and in-place? (not force?))
      (throw (ex-info
              "in-place import may overwrite tracked files; rerun with --force or supply a destination"
              {:exit 128 :kind :force-required :root (.getPath ^java.io.File root)})))
    (when (.isFile (projection/marker-file root))
      (throw (ex-info "destination is already a Geschichte repository"
                      {:exit 128 :kind :destination-exists
                       :root (.getPath ^java.io.File root)})))
    (when (and (not in-place?) (.exists ^java.io.File root)
               (seq (.list ^java.io.File root)))
      (throw (ex-info "destination already exists and is not empty"
                      {:exit 128 :kind :destination-exists
                       :root (.getPath ^java.io.File root)})))
    (let [locator (projection/initialize-projection! root)
          conn (initialize-database! (:config locator) nil)]
      (try
        (let [result (git-local/import! conn source-file
                                        {:remote origin :branch branch})]
          (materialize-imported-projection! locator root)
          {:stdout (if quiet? "" (str (pr-str result) "\n"))
           :stderr "" :exit 0})
        (finally (d/release conn))))))

(defn- run-native [conn locator command args]
  (case command
    "db"
    (case (first args)
      "query" (let [form (some-> (second args) edn/read-string)]
                (when-not form
                  (throw (ex-info "db query requires a Datalog form"
                                  {:exit 129 :kind :usage})))
                {:stdout (str (pr-str (d/q form @conn)) "\n")
                 :stderr "" :exit 0})
      (throw (ex-info "usage: ges db query DATALOG"
                      {:exit 129 :kind :usage})))

    "repo"
    (case (first args)
      "status" {:stdout (str (pr-str (repo/status conn)) "\n") :stderr "" :exit 0}
      "refs" {:stdout (str (pr-str (repo/refs conn)) "\n") :stderr "" :exit 0}
      "log" {:stdout (str (pr-str (repo/log conn)) "\n") :stderr "" :exit 0}
      (throw (ex-info "usage: ges repo status|refs|log"
                      {:exit 129 :kind :usage})))

    "workspace"
    (case (first args)
      "list" {:stdout (str (pr-str (physical-workspace/list-records locator))
                           "\n")
              :stderr "" :exit 0}
      "publish" (do
                  (when (:projection? locator)
                    (projection/scan! conn (:root locator)))
                  {:stdout (str (pr-str (physical-workspace/publish! locator conn))
                                "\n")
                   :stderr "" :exit 0})
      "advance" (do
                  (when (:projection? locator)
                    (projection/scan! conn (:root locator)))
                  {:stdout (str (pr-str (physical-workspace/advance! locator conn))
                                "\n")
                   :stderr "" :exit 0})
      (throw (ex-info "usage: ges workspace list|publish|advance"
                      {:exit 129 :kind :usage})))

    ;; Transitional aliases for the original native surface.
    "query" (run-native conn locator "db" (into ["query"] args))
    "refs" (run-native conn locator "repo" (into ["refs"] args))
    (throw (ex-info (str "unknown Geschichte command: " command)
                    {:exit 129 :kind :usage}))))

(defn- execute-git [conn locator cwd config-overrides argv]
  (let [^java.io.File root (:root locator)
        projection? (:projection? locator)
        _ (when projection? (projection/scan! conn root))
        before (repo/worktree conn)
        result (git-command/execute
                {:conn conn :root (.getPath root)
                 :config (atom config-overrides)
                 :remote-ops git-transport/operations
                 :workspace-ops (when projection?
                                  (physical-workspace/operations conn locator cwd))
                 :read-message #(if (= % "-") (slurp *in*)
                                    (slurp (io/file cwd %)))
                 :repo-relative #(repo-relative root cwd %)}
                argv)
        after (repo/worktree conn)]
    (when (and projection? (not= before after))
      (projection/materialize-changes! conn root before after))
    result))

(defn run
  "Run CLI argv without exiting the process. Returns a process-result map."
  [argv]
  (try
    (let [{host-argv :argv {:keys [repo help]} :options} (host-options argv)]
      (if-let [help-path (or (when help [])
                             (help-request (command-argv host-argv)))]
        (help-result help-path)
        (let [{git-argv :args directories :directories config-overrides :config}
              (git-command/parse-global host-argv)
              cwd (apply-directories (System/getProperty "user.dir") directories)
              command (first git-argv)
              args (vec (rest git-argv))
              ;; `ges git ...` remains a transition alias, including globals
              ;; historically placed after the alias.
              [command args cwd config-overrides]
              (if (= command "git")
                (let [{inner :args inner-dirs :directories inner-config :config}
                      (git-command/parse-global args)]
                  [(first inner) (vec (rest inner))
                   (apply-directories cwd inner-dirs)
                   (merge config-overrides inner-config)])
                [command args cwd config-overrides])]
          (cond
            (= command "init") (init-command cwd repo args)
            (= command "clone")
            (clone-command cwd repo config-overrides args)
            (= command "import-git") (import-git-command cwd repo args)
            (and (= command "diff") (some #{"--no-index"} args))
            (git-no-index/run cwd args)
            :else
            (let [locator (-> (projection/locate cwd repo)
                              physical-workspace/ensure-registered!)]
              (with-connection
                (:config locator)
                (fn [conn]
                  (if (contains? #{"db" "repo" "workspace" "query" "refs"}
                                 command)
                    (run-native conn locator command args)
                    (execute-git conn locator cwd config-overrides
                                 (into [command] args))))))))))
    (catch Throwable error
      (let [{:keys [exit] :as data} (ex-data error)
            details (dissoc data :exit)]
        {:stdout ""
         :stderr (str "fatal: " (or (ex-message error) error) "\n"
                      (when (seq details)
                        (str "fatal: details " (pr-str details) "\n")))
         :exit (or exit 128)}))))

(defn -main [& argv]
  (let [{:keys [stdout stderr exit]} (run argv)]
    (print stdout)
    (flush)
    (binding [*out* *err*]
      (print stderr)
      (flush))
    (when-not (zero? exit) (System/exit exit))))
