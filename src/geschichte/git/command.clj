(ns geschichte.git.command
  "Git-compatible command interpretation and rendering over Geschichte.

   Hosts supply repository context and path resolution. This namespace owns
   Git argv semantics, exit codes, and presentation so native and embedded
   command surfaces cannot drift."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [geschichte.bytes :as bytes]
            [geschichte.content :as content]
            [geschichte.diff :as diff]
            [geschichte.fs :as gfs]
            [geschichte.git.compatibility :as compatibility]
            [geschichte.git.binary-patch :as binary-patch]
            [geschichte.ignore :as ignore]
            [geschichte.git.object :as object]
            [geschichte.git.revision :as revision]
            [geschichte.git.similarity :as similarity]
            [geschichte.merge :as merge]
            [geschichte.repo :as repo]
            [geschichte.query :as query])
  (:import [java.nio ByteBuffer]
           [java.security MessageDigest]
           [java.nio.charset CharacterCodingException CodingErrorAction
            StandardCharsets]))

(defn- git-blob-oid
  "Compute Git's canonical SHA-1 blob ID without assembling chunked content."
  [conn {:keys [content size]}]
  (let [^MessageDigest digest (MessageDigest/getInstance "SHA-1")
        ^bytes header (bytes/utf8 (str "blob " size "\u0000"))]
    (.update digest header 0 (alength header))
    (content/consume-by-id!
     conn content
     (fn [chunk]
       (.update digest ^bytes chunk 0 (alength ^bytes chunk))))
    (object/bytes->hex (.digest digest))))

(defn- ok
  ([] (ok ""))
  ([stdout] {:stdout stdout :stderr "" :exit 0}))

(defn- fail
  ([message] (fail message 128))
  ([message exit]
   {:stdout "" :stderr (str "fatal: " message "\n") :exit exit}))

(defn- transparent-message [{:keys [reason options action]}]
  (case reason
    :command-not-implemented "command is not yet implemented by Geschichte"
    :unsupported-options (str "unsupported option"
                              (when (< 1 (count options)) "s") ": "
                              (str/join ", " options))
    :unsupported-action (str "unsupported action"
                             (when action (str ": " action)))
    :detached-worktree-not-implemented
    "detached worktrees are not implemented by Geschichte"
    :shallow-history-not-implemented
    "shallow history is not implemented by Geschichte"
    :fetch-ref-maintenance-not-implemented
    "fetch ref maintenance is not implemented by Geschichte"
    :force-lease-not-implemented
    "force-with-lease is not implemented by Geschichte"
    :rebase-state-not-present
    "no resumable Geschichte rebase is in progress"
    (str "unsupported Git operation: " (name reason))))

(defn- preflight [argv]
  (let [{:keys [status] :as classification}
        (compatibility/classify-shape argv)]
    (when (= :transparent status)
      (fail (transparent-message classification)
            (if (= :command-not-implemented (:reason classification)) 1 129)))))

(defn- index-of [values value]
  (.indexOf ^java.util.List values value))

(defn- validate-options!
  "Reject unsupported options before a command can accidentally perform a
  different mutation. `specs` maps accepted option names to `:flag` or `:value`;
  predicates accept dynamic spellings such as `-5`. Parsing stops at `--`."
  ([command args specs] (validate-options! command args specs []))
  ([command args specs predicates]
   (loop [args (seq args)]
     (when-let [arg (first args)]
       (cond
         (= arg "--") nil

         (= :value (get specs arg))
         (if (second args)
           (recur (nnext args))
           (throw (ex-info (str "option requires an argument: " arg)
                           {:command command :option arg})))

         (= :flag (get specs arg))
         (recur (next args))

         (some (fn [[option kind]]
                 (and (= :value kind)
                      (str/starts-with? arg (str option "="))))
               specs)
         (recur (next args))

         (some #(% arg) predicates)
         (recur (next args))

         (str/starts-with? arg "-")
         (throw (ex-info (str "unknown option `" arg "'")
                         {:command command :option arg}))

         :else (recur (next args)))))))

(defn parse-global
  "Separate host-resolved Git global options from command argv. `-C` may be
  repeated; hosts apply each directory in order before repository discovery.
  `-c name=value` entries are returned in order for command-scoped config."
  [argv]
  (loop [remaining (vec argv), directories [], config []]
    (if-let [arg (first remaining)]
      (cond
        (contains? #{"--no-pager" "--paginate"} arg)
        (recur (subvec remaining 1) directories config)

        (= arg "-C")
        (if-let [directory (second remaining)]
          (recur (subvec remaining 2) (conj directories directory) config)
          (throw (ex-info "option requires an argument: -C" {})))

        (str/starts-with? arg "-C=")
        (recur (subvec remaining 1)
               (conj directories (subs arg (count "-C="))) config)

        (= arg "-c")
        (if-let [entry (second remaining)]
          (if (str/includes? entry "=")
            (recur (subvec remaining 2) directories (conj config entry))
            (throw (ex-info "-c expects name=value" {:value entry})))
          (throw (ex-info "option requires an argument: -c" {})))

        (str/starts-with? arg "-c=")
        (let [entry (subs arg (count "-c="))]
          (if (str/includes? entry "=")
            (recur (subvec remaining 1) directories (conj config entry))
            (throw (ex-info "-c expects name=value" {:value entry}))))

        :else {:args remaining :directories directories
               :config (into {} (map #(str/split % #"=" 2)) config)})
      {:args [] :directories directories
       :config (into {} (map #(str/split % #"=" 2)) config)})))

(defn parse-init
  "Parse Git-compatible `init` arguments. Host adapters perform filesystem
   creation and mounting because those operations are deliberately outside the
   repository command engine."
  [args]
  (loop [args args, opts {}, operands []]
    (if-let [arg (first args)]
      (cond
        (or (= arg "-q") (= arg "--quiet"))
        (recur (next args) (assoc opts :quiet? true) operands)

        (or (= arg "-b") (= arg "--initial-branch"))
        (if-let [branch (second args)]
          (recur (nnext args) (assoc opts :branch branch) operands)
          (throw (ex-info (str "option requires an argument: " arg) {})))

        (str/starts-with? arg "--initial-branch=")
        (recur (next args)
               (assoc opts :branch (subs arg (count "--initial-branch=")))
               operands)

        (= arg "--bare")
        (throw (ex-info "bare repositories are not supported by the mounted worktree model" {}))

        (str/starts-with? arg "-")
        (throw (ex-info (str "unknown option `" arg "'") {}))

        :else (recur (next args) opts (conj operands arg)))
      (do
        (when (> (count operands) 1)
          (throw (ex-info "too many arguments" {})))
        (assoc opts :path (or (first operands) "."))))))

(defn parse-clone
  "Parse the host-lifecycle portion of `git clone`.

  Clone creates both repository storage and a destination mount/projection, so
  hosts call this before the ordinary open-connection command engine."
  [args]
  (loop [args args, opts {:origin "origin"}, operands []]
    (if-let [arg (first args)]
      (cond
        (or (= arg "-q") (= arg "--quiet"))
        (recur (next args) (assoc opts :quiet? true) operands)

        (or (= arg "-n") (= arg "--no-checkout"))
        (recur (next args) (assoc opts :no-checkout? true) operands)

        (contains? #{"-b" "--branch" "-o" "--origin" "--depth"} arg)
        (if-let [value (second args)]
          (recur (nnext args)
                 (assoc opts (cond
                               (contains? #{"-b" "--branch"} arg) :branch
                               (= "--depth" arg) :depth
                               :else :origin)
                        value)
                 operands)
          (throw (ex-info (str "option requires an argument: " arg) {})))

        (str/starts-with? arg "--branch=")
        (recur (next args)
               (assoc opts :branch (subs arg (count "--branch="))) operands)

        (str/starts-with? arg "--origin=")
        (recur (next args)
               (assoc opts :origin (subs arg (count "--origin="))) operands)

        (str/starts-with? arg "--depth=")
        (throw (ex-info "shallow clone (--depth) is not implemented" {}))

        (str/starts-with? arg "-")
        (throw (ex-info (str "unknown option `" arg "'") {}))

        :else (recur (next args) opts (conj operands arg)))
      (let [[url destination & extra] operands
            inferred (some-> url
                             (str/replace #"/+$" "")
                             (str/split #"[/:]") last
                             (str/replace #"\.git$" ""))]
        (when (or (str/blank? url) (seq extra))
          (throw (ex-info "usage: git clone [options] <repository> [<directory>]"
                          {})))
        (when (str/blank? (or destination inferred))
          (throw (ex-info "could not determine clone directory" {:url url})))
        (when (:depth opts)
          (throw (ex-info "shallow clone (--depth) is not implemented" {})))
        (assoc opts :url url :path (or destination inferred))))))

(defn- status-code [kind]
  ({:added "A" :deleted "D" :modified "M"} kind " "))

(defn- short-status [entries]
  (apply str
         (mapcat
          (fn [{:keys [path index worktree]}]
            (cond-> []
              (= :ignored worktree)
              (conj (str "!! " path "\n"))

              (and (not= :ignored worktree)
                   (or index (and worktree (not= :untracked worktree))))
              (conj (str (status-code index) (status-code worktree)
                         " " path "\n"))
              (= :untracked worktree)
              (conj (str "?? " path "\n"))))
          entries)))

(defn- git-status [conn args]
  (validate-options! "status" args
                     {"-s" :flag "--short" :flag "--porcelain" :flag
                      "--porcelain=v1" :flag "-b" :flag "--branch" :flag
                      "-sb" :flag "--ignored" :flag
                      "-u" :flag "--untracked-files" :flag}
                     [#(or (str/starts-with? % "--untracked-files=")
                           (boolean (re-matches #"-u(?:no|normal|all)" %)))])
  (let [rules (ignore/rules conn)
        untracked-mode (or (some (fn [arg]
                                   (cond
                                     (str/starts-with? arg "--untracked-files=")
                                     (subs arg (count "--untracked-files="))
                                     (re-matches #"-u(no|normal|all)" arg)
                                     (second (re-matches #"-u(no|normal|all)" arg))))
                                 args)
                           "normal")
        show-ignored? (boolean (some #{"--ignored"} args))
        entries (->> (repo/status-entries conn)
                     (keep (fn [{:keys [path worktree] :as entry}]
                             (let [ignored? (and (= :untracked worktree)
                                                 (ignore/ignored? rules path))
                                   entry (cond
                                           (and ignored? show-ignored?)
                                           (assoc entry :worktree :ignored)
                                           ignored? (dissoc entry :worktree)
                                           (and (= :untracked worktree)
                                                (= "no" untracked-mode))
                                           (dissoc entry :worktree)
                                           :else entry)]
                               (when (or (:index entry) (:worktree entry))
                                 entry))))
                     vec)
        short? (some #{"-s" "--short" "--porcelain" "--porcelain=v1" "-sb"} args)
        show-branch? (some #{"-b" "--branch" "-sb"} args)
        branch (str/replace (repo/current-ref conn) #"^refs/heads/" "")]
    (if short?
      (ok (str (when show-branch? (str "## " branch "\n"))
               (short-status entries)))
      (ok (str "On branch " branch "\n"
               (if (empty? entries)
                 "nothing to commit, working tree clean\n"
                 (short-status entries)))))))

(defn- git-add [conn repo-relative args]
  (validate-options! "add" args
                     {"-A" :flag "--all" :flag
                      "-u" :flag "--update" :flag
                      "-f" :flag "--force" :flag})
  (let [all? (some #{"-A" "--all"} args)
        update? (some #{"-u" "--update"} args)
        dot? (some #{"."} args)
        force? (boolean (some #{"-f" "--force"} args))
        paths (remove #(or (= % "-A") (= % "--all")
                           (= % "-u") (= % "--update") (= % "--")
                           (= % "-f") (= % "--force")) args)
        rules (ignore/rules conn)]
    (let [tracked (keys (query/stage @conn))
          worktree (repo/files conn)
          candidates (vec (distinct (concat tracked worktree)))
          cwd-relative (or (repo-relative ".") "")
          under? (fn [prefix path]
                   (or (= prefix path)
                       (str/blank? prefix)
                       (str/starts-with? path (str prefix "/"))))
          selected
          (cond
            all? candidates
            update? (filterv #(under? cwd-relative %) tracked)
            dot? (filterv #(under? cwd-relative %) candidates)
            :else
            (let [specs (mapv #(or (repo-relative %)
                                   (throw (ex-info "pathspec is outside repository"
                                                   {:path %})))
                              paths)]
              (when (empty? specs)
                (throw (ex-info "Nothing specified, nothing added" {})))
              (mapcat (fn [spec]
                        (let [matches (filterv #(under? spec %) candidates)]
                          (when (empty? matches)
                            (throw (ex-info
                                    (str "pathspec '" spec "' did not match any files")
                                    {:pathspec spec})))
                          matches))
                      specs)))
          selected (vec (distinct selected))]
      (when (and (not all?) (not update?) (not dot?) (empty? paths))
        (throw (ex-info "Nothing specified, nothing added" {})))
      (let [ignored (when-not force? (filter #(ignore/ignored? rules %) selected))]
        (when (and (seq ignored) (not all?) (not dot?))
          (throw (ex-info
                  (str "The following paths are ignored by one of your .gitignore files:\n"
                       (str/join "\n" ignored)
                       "\nUse -f if you really want to add them.")
                  {:paths ignored})))
        (repo/stage! conn (if force?
                            selected
                            (vec (remove #(ignore/ignored? rules %) selected))))
        (ok)))))

(defn- option-value [args short long]
  (or (some (fn [[a b]] (when (or (= a short) (= a long)) b))
            (partition-all 2 1 args))
      (some #(when (str/starts-with? % (str long "="))
               (subs % (inc (count long)))) args)))

(defn- configured-author [config]
  (let [name (get config "user.name")
        email (get config "user.email")]
    (cond
      (and name email) (str name " <" email ">")
      name name
      :else "unknown")))

(declare short-option-char?)

(defn- git-commit [conn global-config read-message args]
  (validate-options! "commit" args
                     {"-m" :value "--message" :value "--author" :value
                      "-F" :value "--file" :value
                      "-F-" :flag
                      "-a" :flag "--all" :flag "-q" :flag "--quiet" :flag
                      "-am" :value "--no-verify" :flag
                      "--amend" :flag "--no-edit" :flag}
                     [#(boolean (re-matches #"-[aq]*F-?" %))])
  (let [stage-tracked? (or (boolean (some #{"-a" "--all" "-am"} args))
                           (short-option-char? args \a))
        quiet? (or (boolean (some #{"-q" "--quiet"} args))
                   (short-option-char? args \q))
        amend? (boolean (some #{"--amend"} args))
        _ (when stage-tracked?
            (repo/stage! conn (keys (query/stage @conn))))
        message (or (option-value args "-m" "--message")
                    (option-value args "-am" "--message")
                    (when-let [path (or (option-value args "-F" "--file")
                                        (some (fn [[arg value]]
                                                (when (re-matches #"-[aq]*F" arg)
                                                  value))
                                              (partition-all 2 1 args))
                                        (when (some #(re-matches #"-[aq]*F-" %)
                                                    args) "-"))]
                      (if read-message
                        (str/trimr (read-message path))
                        (throw (ex-info
                                "commit -F requires a host message reader"
                                {:path path}))))
                    (when (and amend? (some #{"--no-edit"} args))
                      (:geschichte.commit/message (repo/head-commit conn))))
        author (or (option-value args nil "--author")
                   (configured-author
                    (merge @global-config (repo/configuration conn))))
        commit (repo/commit! conn {:message message :author author
                                   :amend? amend?})]
    (ok (if quiet? ""
            (str "[" (str/replace (:geschichte.ref/name commit) #"^refs/heads/" "")
                 " " (:geschichte.commit/id commit) "] " message "\n")))))

(defn- decode-text [value]
  (try
    (let [decoder (doto (.newDecoder StandardCharsets/UTF_8)
                    (.onMalformedInput CodingErrorAction/REPORT)
                    (.onUnmappableCharacter CodingErrorAction/REPORT))
          text (str (.decode decoder (ByteBuffer/wrap value)))]
      (when-not (str/includes? text "\u0000") text))
    (catch CharacterCodingException _ nil)))

(defn- pathspec-regex
  ([spec] (pathspec-regex spec false))
  ([spec glob?]
   (let [expression
         (loop [characters (seq spec), pieces []]
           (if-let [character (first characters)]
             (if (and glob? (= character \*) (= \* (second characters)))
               (recur (nnext characters) (conj pieces ".*"))
               (recur (next characters)
                      (conj pieces
                            (case character
                              \* (if glob? "[^/]*" ".*")
                              \? (if glob? "[^/]" ".")
                              (java.util.regex.Pattern/quote
                               (str character))))))
             (apply str pieces)))]
     (re-pattern (str "^" expression "$")))))

(defn- parse-pathspec [spec]
  (if (map? spec)
    spec
    (let [spec (str spec)
          [magic pattern]
          (cond
            (str/starts-with? spec ":(")
            (let [end (str/index-of spec ")")]
              (when-not end
                (throw (ex-info (str "invalid pathspec magic: " spec)
                                {:exit 129 :kind :usage})))
              [(set (map keyword (str/split (subs spec 2 end) #",")))
               (subs spec (inc end))])

            (str/starts-with? spec ":/") [#{:top} (subs spec 2)]
            (or (str/starts-with? spec ":!")
                (str/starts-with? spec ":^")) [#{:exclude} (subs spec 2)]
            :else [#{} spec])]
      (when-let [unsupported (seq (remove #{:top :glob :literal :icase :exclude}
                                          magic))]
        (throw (ex-info (str "unsupported pathspec magic: "
                             (str/join ", " (map name unsupported)))
                        {:exit 129 :kind :usage :magic unsupported})))
      {:pattern pattern :top? (contains? magic :top)
       :glob? (contains? magic :glob) :literal? (contains? magic :literal)
       :icase? (contains? magic :icase)
       :exclude? (contains? magic :exclude)})))

(defn- pathspec-match? [spec path]
  (let [{:keys [pattern glob? literal? icase?]} (parse-pathspec spec)
        [pattern path] (if icase?
                         [(str/lower-case pattern) (str/lower-case path)]
                         [pattern path])
        wildcard? (and (not literal?) (re-find #"[*?]" pattern))]
    (if wildcard?
      (or (re-matches (pathspec-regex pattern glob?) path)
          ;; A basename-only pathspec recursively matches at every depth.
          (and (not glob?) (not (str/includes? pattern "/"))
               (re-matches (pathspec-regex pattern false)
                           (last (str/split path #"/")))))
      (or (str/blank? pattern)
          (= pattern path)
          (str/starts-with? path (str pattern "/"))))))

(defn- path-selected? [specs path]
  (let [specs (mapv parse-pathspec specs)
        included (remove :exclude? specs)
        excluded (filter :exclude? specs)]
    (and (or (empty? included) (some #(pathspec-match? % path) included))
         (not-any? #(pathspec-match? % path) excluded))))

(defn- resolve-diff-pathspec [repo-relative spec]
  (let [{:keys [pattern top?] :as parsed} (parse-pathspec spec)
        pattern (if top?
                  pattern
                  (or (repo-relative pattern)
                      (throw (ex-info (str "pathspec is outside the repository: " spec)
                                      {:exit 128 :pathspec spec}))))]
    (assoc parsed :pattern pattern)))

(defn- diff-args [conn repo-relative args]
  (validate-options! "diff" args
                     {"--cached" :flag "--staged" :flag
                      "--quiet" :flag "--exit-code" :flag
                      "--name-only" :flag "--name-status" :flag
                      "--stat" :flag "--shortstat" :flag
                      "--numstat" :flag "--raw" :flag
                      "--binary" :flag "--full-index" :flag
                      "-p" :flag "-u" :flag "--patch" :flag
                      "--patch-with-raw" :flag "--patch-with-stat" :flag
                      "-z" :flag "-R" :flag "--abbrev" :value
                      "--check" :flag
                      "--diff-filter" :value
                      "-w" :flag "--ignore-all-space" :flag
                      "-b" :flag "--ignore-space-change" :flag
                      "--ignore-space-at-eol" :flag
                      "-U" :value "--unified" :value
                      "-M" :flag "--find-renames" :flag
                      "--no-renames" :flag "--no-color" :flag
                      "--color" :flag}
                     [#(boolean (re-matches #"-U\d+" %))
                      #(boolean (re-matches #"-M\d*%?" %))
                      #(boolean (re-matches #"--find-renames=\d+%?" %))
                      #(boolean (re-matches #"--color=(always|auto|never)" %))])
  (let [separator (index-of args "--")
        before (if (neg? separator) args (subvec args 0 separator))
        after (if (neg? separator) [] (subvec args (inc separator)))
        options (set (filter #(str/starts-with? % "-") before))
        operands (vec (remove #(str/starts-with? % "-") before))
        resolved (mapv #(or (when (str/includes? % "..") :range)
                            (revision/resolve conn %)) operands)
        revision-count (count (take-while some? resolved))
        revisions (subvec operands 0 revision-count)
        implicit-paths (subvec operands revision-count)
        context-value (or (option-value before "-U" "--unified")
                          (some #(second (re-matches #"-U(\d+)" %)) before))
        context (if context-value
                  (try
                    (let [value (Long/parseLong context-value)]
                      (when (neg? value)
                        (throw (NumberFormatException.)))
                      value)
                    (catch NumberFormatException _
                      (throw (ex-info (str "invalid context length: " context-value)
                                      {:exit 129 :kind :usage}))))
                  3)
        color (or (some #(when (str/starts-with? % "--color=")
                           (subs % (count "--color="))) before)
                  (when (contains? options "--color") "always")
                  (when (contains? options "--no-color") "never"))
        rename-value (some (fn [arg]
                             (or (second (re-matches #"-M(\d+)%?" arg))
                                 (second (re-matches #"--find-renames=(\d+)%?"
                                                     arg))))
                           before)
        rename-threshold (if rename-value (Long/parseLong rename-value) 50)]
    (when (and color (not (contains? #{"never" "auto" "always"} color)))
      (throw (ex-info
              (str "diff color mode '" color
                   "' is not available in the binding-neutral renderer")
              {:exit 129 :kind :usage :color color})))
    (when (> revision-count 2)
      (throw (ex-info "too many revisions for diff" {:revisions revisions})))
    {:cached? (boolean (some #{"--cached" "--staged"} before))
     :quiet? (boolean (some #{"--quiet"} before))
     :exit-code? (boolean (some #{"--quiet" "--exit-code"} before))
     :name-only? (contains? options "--name-only")
     :name-status? (contains? options "--name-status")
     :numstat? (contains? options "--numstat")
     :raw? (contains? options "--raw")
     :patch-with-raw? (contains? options "--patch-with-raw")
     :binary-patch? (contains? options "--binary")
     :full-index? (contains? options "--full-index")
     :patch? (boolean (some #{"-p" "-u" "--patch" "--patch-with-raw"
                              "--patch-with-stat"} before))
     :nul-terminated? (contains? options "-z")
     :reverse? (contains? options "-R")
     :abbrev (if-let [value (option-value before nil "--abbrev")]
               (try
                 (let [value (Long/parseLong value)]
                   (when-not (<= 1 value 40) (throw (NumberFormatException.)))
                   value)
                 (catch NumberFormatException _
                   (throw (ex-info (str "invalid object name abbreviation: " value)
                                   {:exit 129 :kind :usage}))))
               7)
     :diff-filter (option-value before nil "--diff-filter")
     :whitespace-mode (cond
                        (some #{"-w" "--ignore-all-space"} before) :all
                        (some #{"-b" "--ignore-space-change"} before) :change
                        (some #{"--ignore-space-at-eol"} before) :eol)
     :stat? (or (contains? options "--stat")
                (contains? options "--patch-with-stat"))
     :shortstat? (contains? options "--shortstat")
     :check? (contains? options "--check")
     :context context
     :color color
     :renames? (not (contains? options "--no-renames"))
     :rename-threshold rename-threshold
     :revisions (if (and (= 1 (count revisions))
                         (str/includes? (first revisions) ".."))
                  (revision/diff-pair conn (first revisions))
                  (mapv #(revision/require conn %) revisions))
     :paths (mapv #(resolve-diff-pathspec repo-relative %)
                  (concat implicit-paths after))}))

(defn- abbreviated-blob-oid [conn entry]
  (if entry (subs (git-blob-oid conn entry) 0 7) "0000000"))

(defn- entry-mode [entry]
  (if entry (format "%06o" (:mode entry)) "000000"))

(defn- entry-has-nul? [conn entry]
  (when-let [id (:content entry)]
    (let [remaining (long-array [8000])
          found (boolean-array 1)]
      (content/consume-by-id!
       conn id
       (fn [^bytes chunk]
         (let [limit (int (min (aget remaining 0) (alength chunk)))]
           (loop [index 0]
             (when (and (< index limit) (not (aget found 0)))
               (if (zero? (aget chunk index))
                 (aset found 0 true)
                 (recur (inc index)))))
           (aset remaining 0 (- (aget remaining 0) limit))
           (when (or (aget found 0) (zero? (aget remaining 0)))
             content/stop-consumption))))
      (aget found 0))))

(defn- write-entry! [conn entry ^java.io.OutputStream output]
  (when-let [id (:content entry)]
    (content/consume-by-id!
     conn id #(.write output ^bytes % 0 (alength ^bytes %)))))

(defn- streaming-binary-patch [conn before after]
  (binary-patch/patch-streaming
   (long (or (:size before) 0)) #(write-entry! conn before %)
   (long (or (:size after) 0)) #(write-entry! conn after %)))

(defn- render-file-diff
  ([conn change] (render-file-diff conn 3 change))
  ([conn context {:keys [path old-path before after kind similarity]}]
   (render-file-diff conn context nil
                     {:path path :old-path old-path :before before :after after
                      :kind kind :similarity similarity}))
  ([conn context normalize
    {:keys [path old-path before after kind similarity]}]
   (let [left-entry before
         right-entry after
         nul-binary? (or (entry-has-nul? conn left-entry)
                         (entry-has-nul? conn right-entry))
         left-bytes (when-not nul-binary?
                      (or (repo/read-entry conn left-entry) (bytes/empty-bytes)))
         right-bytes (when-not nul-binary?
                       (or (repo/read-entry conn right-entry) (bytes/empty-bytes)))
         left-text (when left-bytes (decode-text left-bytes))
         right-text (when right-bytes (decode-text right-bytes))
         old-path (or old-path path)
         a-name (if left-entry (str "a/" old-path) "/dev/null")
         b-name (if right-entry (str "b/" path) "/dev/null")
         left-mode (entry-mode left-entry)
         right-mode (entry-mode right-entry)
         left-full-oid (if left-entry (git-blob-oid conn left-entry)
                           (apply str (repeat 40 "0")))
         right-full-oid (if right-entry (git-blob-oid conn right-entry)
                            (apply str (repeat 40 "0")))
         left-oid (subs left-full-oid 0 7)
         right-oid (subs right-full-oid 0 7)
         mode-changed? (and left-entry right-entry
                            (not= (:mode left-entry) (:mode right-entry)))
         content-changed? (not= (:content left-entry) (:content right-entry))
         renamed? (= :renamed kind)
         header (str "diff --git a/" old-path " b/" path "\n"
                     (when-not left-entry (str "new file mode " right-mode "\n"))
                     (when-not right-entry (str "deleted file mode " left-mode "\n"))
                     (when renamed?
                       (str "similarity index " similarity "%\n"
                            "rename from " old-path "\n"
                            "rename to " path "\n"))
                     (when mode-changed?
                       (str "old mode " left-mode "\n"
                            "new mode " right-mode "\n"))
                     (when content-changed?
                       (str "index " left-oid ".." right-oid
                            (when (and left-entry right-entry
                                       (not mode-changed?))
                              (str " " left-mode))
                            "\n")))
         full-header (if content-changed?
                       (str/replace
                        header
                        #"index [0-9a-f]{7}\.\.[0-9a-f]{7}([^\n]*)\n"
                        (str "index " left-full-oid ".." right-full-oid
                             (when (and left-entry right-entry
                                        (not mode-changed?))
                               (str " " left-mode))
                             "\n"))
                       header)]
     (if (and left-text right-text)
       (let [result (diff/diff-text left-text right-text
                                    (when normalize {:normalize normalize}))
             patch-body (diff/unified result {:a-name a-name :b-name b-name
                                              :context context})]
         {:path path :old-path old-path :kind kind :similarity similarity
          :before before :after after
          :header header :full-header full-header
          :left-full-oid left-full-oid :right-full-oid right-full-oid
          :patch-body patch-body
          :text (str header patch-body)
          :added (reduce + 0 (map (fn [{:keys [op b-count]}]
                                    (if (= :insert op) b-count 0))
                                  (:edits result)))
          :deleted (reduce + 0 (map (fn [{:keys [op a-count]}]
                                      (if (= :delete op) a-count 0))
                                    (:edits result)))
          :result result :right-text right-text :binary? false})
       {:path path :old-path old-path :kind kind :similarity similarity
        :before before :after after
        :header header :full-header full-header
        :left-full-oid left-full-oid :right-full-oid right-full-oid
        :patch-body (str "Binary files " a-name " and " b-name " differ\n")
        :text (str header "Binary files " a-name " and " b-name " differ\n")
        :added 0 :deleted 0 :binary? true}))))

(defn- render-stat-summary [rendered]
  (let [added (reduce + 0 (map :added rendered))
        deleted (reduce + 0 (map :deleted rendered))
        files (count rendered)]
    (when (pos? files)
      (str " " files " file" (when (not= files 1) "s") " changed"
           (when (pos? added)
             (str ", " added " insertion" (when (not= added 1) "s") "(+)"))
           (when (pos? deleted)
             (str ", " deleted " deletion" (when (not= deleted 1) "s") "(-)"))
           "\n"))))

(defn- render-stat [rendered]
  (str
   (apply str
          (map (fn [{:keys [path old-path kind added deleted binary? before after]}]
                 (let [display-path (if (= :renamed kind)
                                      (str old-path " => " path) path)]
                   (if binary?
                     (str " " display-path " | Bin " (or (:size before) 0) " -> "
                          (or (:size after) 0) " bytes\n")
                     (str " " display-path " | " (+ added deleted) " "
                          (apply str (repeat added "+"))
                          (apply str (repeat deleted "-")) "\n"))))
               rendered))
   (render-stat-summary rendered)))

(defn- render-numstat [rendered]
  (apply str
         (map (fn [{:keys [path old-path kind added deleted binary?]}]
                (str (if binary? "-" added) "\t"
                     (if binary? "-" deleted) "\t"
                     (if (= :renamed kind) (str old-path " => " path) path)
                     "\n"))
              rendered)))

(defn- colorize-patch [output]
  (apply str
         (for [line (str/split output #"(?<=\n)")
               :let [newline? (str/ends-with? line "\n")
                     body (if newline? (subs line 0 (dec (count line))) line)
                     addition? (and (str/starts-with? body "+")
                                    (not (str/starts-with? body "+++ ")))
                     context? (str/starts-with? body " ")
                     color (cond
                             (or (str/starts-with? body "diff --git ")
                                 (str/starts-with? body "index ")
                                 (str/starts-with? body "new file mode ")
                                 (str/starts-with? body "deleted file mode ")
                                 (str/starts-with? body "old mode ")
                                 (str/starts-with? body "new mode ")
                                 (str/starts-with? body "similarity index ")
                                 (str/starts-with? body "rename from ")
                                 (str/starts-with? body "rename to ")
                                 (str/starts-with? body "--- ")
                                 (str/starts-with? body "+++ ")) "\033[1m"
                             (str/starts-with? body "@@ ") "\033[36m"
                             (str/starts-with? body "-") "\033[31m")]]
           (str (cond
                  addition? (str "\033[32m+\033[m\033[32m" (subs body 1)
                                 "\033[m")
                  color (str color body "\033[m")
                  context? (str body "\033[m")
                  :else body)
                (when newline? "\n")))))

(defn- render-patches
  [conn rendered {:keys [binary-patch? full-index? abbrev reverse?]}]
  (apply str
         (map (fn [{:keys [binary? header full-header patch-body before after]}]
                (let [header (if (or binary-patch? full-index? (not= abbrev 7))
                               full-header header)
                      header (if (and (not full-index?) (not= abbrev 7))
                               (str/replace
                                header
                                #"index ([0-9a-f]{40})\.\.([0-9a-f]{40})"
                                (fn [[_ left right]]
                                  (str "index " (subs left 0 abbrev) ".."
                                       (subs right 0 abbrev))))
                               header)
                      header (if reverse?
                               (-> header
                                   (str/replace
                                    #"diff --git a/([^\n]*?) b/([^\n]*)\n"
                                    "diff --git b/$1 a/$2\n")
                                   (str/replace #"--- a/" "--- b/")
                                   (str/replace #"\+\+\+ b/" "+++ a/"))
                               header)
                      patch-body (if reverse?
                                   (-> patch-body
                                       (str/replace #"--- a/" "--- b/")
                                       (str/replace #"\+\+\+ b/" "+++ a/")
                                       (str/replace #"Binary files a/" "Binary files b/")
                                       (str/replace #" and b/" " and a/"))
                                   patch-body)]
                  (if (and binary-patch? binary?)
                    (str header (streaming-binary-patch conn before after))
                    (str header patch-body))))
              rendered)))

(defn- render-raw [conn rendered worktree-after? abbrev nul-terminated?]
  (apply str
         (map (fn [{:keys [path old-path kind similarity before after]}]
                (let [oid #(if % (subs (git-blob-oid conn %) 0 abbrev)
                               (apply str (repeat abbrev "0")))
                      separator (if nul-terminated? "\u0000" "\t")
                      terminator (if nul-terminated? "\u0000" "\n")]
                  (str ":" (entry-mode before) " " (entry-mode after) " "
                       (oid before) " "
                       (if worktree-after?
                         (apply str (repeat abbrev "0"))
                         (oid after)) " "
                       ({:added "A" :deleted "D" :modified "M"
                         :renamed (format "R%03d" similarity)} kind "M")
                       separator (when (= :renamed kind)
                                   (str old-path separator))
                       path terminator)))
              rendered)))

(defn- plan-exact-renames
  "Pair additions and deletions with the same content identity without loading
  payloads. This is the cheap, deterministic first phase of Git-style rename
  detection; unmatched rows retain their original add/delete meaning."
  [changes]
  (let [deletions (filterv #(= :deleted (:kind %)) changes)
        additions (filterv #(= :added (:kind %)) changes)
        additions-by-content (group-by #(get-in % [:after :content]) additions)
        available (atom (into {} (map (fn [[content rows]]
                                        [content (vec (sort-by :path rows))])
                                      additions-by-content)))
        renames (keep (fn [deleted]
                        (let [content (get-in deleted [:before :content])]
                          (when-let [added (first (get @available content))]
                            (swap! available update content #(vec (rest %)))
                            {:path (:path added)
                             :old-path (:path deleted)
                             :kind :renamed :similarity 100
                             :before (:before deleted) :after (:after added)})))
                      (sort-by :path deletions))
        renamed-old (set (map :old-path renames))
        renamed-new (set (map :path renames))]
    (->> (concat (remove #(or (contains? renamed-old (:path %))
                              (contains? renamed-new (:path %)))
                         changes)
                 renames)
         (sort-by :path)
         vec)))

(def ^:private similarity-max-bytes (* 8 1024 1024))
(def ^:private similarity-max-pairs 1000)

(defn- plan-similar-renames [conn threshold changes]
  (let [deletions (filterv #(= :deleted (:kind %)) changes)
        additions (filterv #(= :added (:kind %)) changes)
        pair-count (* (count deletions) (count additions))]
    (if (or (zero? pair-count) (> pair-count similarity-max-pairs))
      changes
      (let [payloads (atom {})
            payload (fn [entry]
                      (let [id (:content entry)]
                        (or (get @payloads id)
                            (let [value (repo/read-entry conn entry)]
                              (swap! payloads assoc id value)
                              value))))
            candidates
            (for [deleted deletions
                  added additions
                  :when (and (<= (:size (:before deleted)) similarity-max-bytes)
                             (<= (:size (:after added)) similarity-max-bytes))
                  :let [score (similarity/percentage
                               (payload (:before deleted))
                               (payload (:after added)) threshold)]
                  :when (>= score threshold)]
              {:deleted deleted :added added :score score
               :basename? (= (similarity/basename (:path deleted))
                             (similarity/basename (:path added)))})
            candidates (sort-by (juxt (comp - :score)
                                      (comp not :basename?)
                                      (comp :path :deleted)
                                      (comp :path :added))
                                candidates)
            selected (loop [candidates candidates, old #{}, new #{}, result []]
                       (if-let [{:keys [deleted added score]} (first candidates)]
                         (if (or (contains? old (:path deleted))
                                 (contains? new (:path added)))
                           (recur (rest candidates) old new result)
                           (recur (rest candidates)
                                  (conj old (:path deleted))
                                  (conj new (:path added))
                                  (conj result
                                        {:path (:path added)
                                         :old-path (:path deleted)
                                         :kind :renamed :similarity score
                                         :before (:before deleted)
                                         :after (:after added)})))
                         result))
            renamed-old (set (map :old-path selected))
            renamed-new (set (map :path selected))]
        (->> (concat (remove #(or (contains? renamed-old (:path %))
                                  (contains? renamed-new (:path %)))
                             changes)
                     selected)
             (sort-by :path)
             vec)))))

(defn- git-diff [conn repo-relative args]
  (let [{:keys [cached? quiet? exit-code? name-only? name-status? numstat? raw?
                patch-with-raw? binary-patch? full-index? patch?
                nul-terminated? reverse? abbrev
                stat? shortstat? check? diff-filter whitespace-mode context color
                renames?
                rename-threshold revisions paths]}
        (diff-args conn repo-relative args)
        [left right] (cond
                       cached? [(or (first revisions) :head) :index]
                       (= 2 (count revisions)) revisions
                       (= 1 (count revisions)) [(first revisions) :worktree]
                       :else [:index :worktree])
        allowed-kinds (when diff-filter
                        (set (keep {\A :added \D :deleted \M :modified
                                    \R :renamed}
                                   diff-filter)))
        normalize-line (case whitespace-mode
                         :all #(str/replace % #"\s+" "")
                         :change #(-> % str/trim (str/replace #"\s+" " "))
                         :eol #(str/replace % #"[ \t]+$" "")
                         nil)
        whitespace-equivalent?
        (fn [{:keys [before after]}]
          (let [left (some-> (repo/read-entry conn before) decode-text)
                right (some-> (repo/read-entry conn after) decode-text)]
            (and left right
                 (= (mapv normalize-line (str/split-lines left))
                    (mapv normalize-line (str/split-lines right))))))
        changed (->> (repo/changes conn left right)
                     ;; Git's ordinary diff never reports untracked files.
                     ;; A physical Geschichte projection imports them into its
                     ;; authoritative worktree, so exclude worktree-only rows
                     ;; unless a future explicit --no-index mode requests them.
                     (filterv #(not (and (= right :worktree)
                                         (nil? (:before %)))))
                     (#(if renames?
                         (->> % plan-exact-renames
                              (plan-similar-renames conn rename-threshold))
                         %))
                     (filterv #(or (nil? allowed-kinds)
                                   (contains? allowed-kinds (:kind %))))
                     (filterv #(or (nil? whitespace-mode)
                                   (not (whitespace-equivalent? %))))
                     (filterv #(or (path-selected? paths (:path %))
                                   (and (:old-path %)
                                        (path-selected? paths (:old-path %))))))
        changed (if reverse?
                  (mapv (fn [{:keys [path old-path before after kind] :as change}]
                          (cond-> (assoc change :before after :after before
                                         :kind ({:added :deleted
                                                 :deleted :added}
                                                kind kind))
                            (= :renamed kind)
                            (assoc :path old-path :old-path path)))
                        changed)
                  changed)
        rendered (mapv #(render-file-diff conn context normalize-line %) changed)
        whitespace-errors
        (when check?
          (mapcat (fn [{:keys [path result]}]
                    (loop [operations (some-> result diff/operations)
                           line-number 1, errors []]
                      (if-let [[operation line] (first operations)]
                        (case operation
                          :add (recur
                                (next operations) (inc line-number)
                                (cond-> errors
                                  (re-find #"[ \t]+$" line)
                                  (conj (str path ":" line-number
                                             ": trailing whitespace.\n+"
                                             line "\n"))))
                          :keep (recur (next operations) (inc line-number) errors)
                          :del (recur (next operations) line-number errors))
                        errors)))
                  rendered))
        output
        (cond
          quiet? ""
          check? (apply str whitespace-errors)
          name-only? (apply str (map #(str (:path %)
                                           (if nul-terminated? "\u0000" "\n"))
                                     rendered))
          name-status? (apply str
                              (map (fn [{:keys [path old-path kind similarity]}]
                                     (let [separator (if nul-terminated? "\u0000" "\t")
                                           terminator (if nul-terminated? "\u0000" "\n")]
                                       (str ({:added "A" :deleted "D" :modified "M"
                                              :renamed (format "R%03d" similarity)} kind "M")
                                            separator
                                            (when (= :renamed kind)
                                              (str old-path separator))
                                            path terminator)))
                                   changed))
          numstat? (render-numstat rendered)
          patch-with-raw? (str (render-raw conn rendered (= right :worktree)
                                           abbrev nul-terminated?)
                               "\n"
                               (render-patches conn rendered
                                               {:binary-patch? binary-patch?
                                                :full-index? full-index?
                                                :abbrev abbrev
                                                :reverse? reverse?}))
          raw? (render-raw conn rendered (= right :worktree)
                           abbrev nul-terminated?)
          shortstat? (or (render-stat-summary rendered) "")
          (and stat? patch?) (str (render-stat rendered) "\n"
                                  (render-patches conn rendered
                                                  {:binary-patch? binary-patch?
                                                   :full-index? full-index?
                                                   :abbrev abbrev
                                                   :reverse? reverse?}))
          stat? (render-stat rendered)
          :else (render-patches conn rendered
                                {:binary-patch? binary-patch?
                                 :full-index? full-index?
                                 :abbrev abbrev
                                 :reverse? reverse?}))
        output (if (= color "always") (colorize-patch output) output)]
    {:stdout output
     :stderr ""
     :exit (cond
             (and check? (seq whitespace-errors)) 2
             (and exit-code? (seq changed)) 1
             :else 0)}))

(defn- git-log [conn args]
  (validate-options! "log" args
                     {"--oneline" :flag "--stat" :flag "--all" :flag
                      "-n" :value "--max-count" :value
                      "--grep" :value "-S" :value
                      "-i" :flag "--regexp-ignore-case" :flag
                      "--all-match" :flag "--reverse" :flag
                      "-p" :flag "--patch" :flag "--follow" :flag
                      "--name-only" :flag "--name-status" :flag
                      "--diff-filter" :value}
                     [#(boolean (re-matches #"-[0-9]+" %))
                      #(str/starts-with? % "--format=")
                      #(str/starts-with? % "--pretty=")
                      #(and (str/starts-with? % "-S") (> (count %) 2))])
  (let [separator (index-of args "--")
        before (if (neg? separator) args (subvec args 0 separator))
        paths (if (neg? separator) [] (subvec args (inc separator)))
        oneline? (some #{"--oneline"} before)
        stat? (boolean (some #{"--stat"} args))
        patch? (boolean (some #{"-p" "--patch"} args))
        name-only? (boolean (some #{"--name-only"} args))
        name-status? (boolean (some #{"--name-status"} args))
        reverse? (boolean (some #{"--reverse"} args))
        all? (boolean (some #{"--all"} args))
        format-option (some (fn [arg]
                              (cond
                                (str/starts-with? arg "--format=")
                                (subs arg (count "--format="))
                                (str/starts-with? arg "--pretty=format:")
                                (subs arg (count "--pretty=format:"))
                                (str/starts-with? arg "--pretty=")
                                (subs arg (count "--pretty=")))) args)
        grep-patterns (vec (concat
                            (keep (fn [[a b]] (when (= a "--grep") b))
                                  (partition-all 2 1 args))
                            (keep #(when (str/starts-with? % "--grep=")
                                     (subs % (count "--grep="))) args)))
        ignore-case? (boolean (some #{"-i" "--regexp-ignore-case"} args))
        all-match? (boolean (some #{"--all-match"} args))
        pickaxe (or (option-value args "-S" nil)
                    (some #(when (and (str/starts-with? % "-S")
                                      (> (count %) 2))
                             (subs % 2)) args))
        diff-filter (option-value args nil "--diff-filter")
        allowed-kinds (when diff-filter
                        (set (keep {\A :added \D :deleted \M :modified}
                                   diff-filter)))
        limit (some-> (or (option-value args "-n" "--max-count")
                          (some #(second (re-matches #"-([0-9]+)" %)) args))
                      parse-long)
        operands (vec (remove #(str/starts-with? % "-")
                              (loop [remaining before, out []]
                                (if-let [arg (first remaining)]
                                  (if (contains? #{"-n" "--max-count" "--grep"
                                                   "-S" "--diff-filter"} arg)
                                    (recur (nnext remaining) out)
                                    (recur (next remaining) (conj out arg)))
                                  out))))
        _ (when (> (count operands) 1)
            (throw (ex-info "too many revisions for log" {:operands operands})))
        selection (when-let [expression (first operands)]
                    (revision/selection conn expression))
        starts (when all?
                 (->> (vals (repo/refs conn))
                      (keep #(repo/commit-by-id conn %))
                      vec))
        _ (when (and all? selection)
            (throw (ex-info "--all cannot be combined with a revision range"
                            {:operands operands})))
        selected-starts (or starts (:starts selection))
        commits (repo/log conn (cond-> {:limit (if (seq paths)
                                                 Integer/MAX_VALUE
                                                 (or limit 100))}
                                 selected-starts (assoc :starts selected-starts)
                                 selection (assoc :exclude (:exclude selection))))
        commit-changes
        (fn [commit]
          (let [parent-id (get-in commit [:geschichte.commit/parents 0
                                          :geschichte.commit/id])
                parent (when parent-id (repo/commit-by-id conn parent-id))]
            (->> (repo/changes conn (or parent :empty) commit)
                 (filterv #(or (nil? allowed-kinds)
                               (contains? allowed-kinds (:kind %)))))))
        pickaxe-match?
        (fn [change]
          (let [before (some-> (repo/read-entry conn (:before change)) decode-text)
                after (some-> (repo/read-entry conn (:after change)) decode-text)
                count-matches (fn [text]
                                (if text
                                  (count (re-seq (re-pattern
                                                  (java.util.regex.Pattern/quote
                                                   pickaxe)) text))
                                  0))]
            (not= (count-matches before) (count-matches after))))
        message-match?
        (fn [commit]
          (let [message (:geschichte.commit/message commit)
                match (fn [pattern]
                        (re-find (re-pattern
                                  (str (when ignore-case? "(?i)") pattern))
                                 message))]
            (or (empty? grep-patterns)
                ((if all-match? every? some) match grep-patterns))))
        commits (filterv (fn [commit]
                           (and (message-match? commit)
                                (or (nil? pickaxe)
                                    (some pickaxe-match? (commit-changes commit)))))
                         commits)
        commits (if (seq paths)
                  (->> commits
                       (filter
                        (fn [commit]
                          (some #(path-selected? paths (:path %))
                                (commit-changes commit))))
                       (take (or limit 100)))
                  commits)
        commits (if reverse? (vec (reverse commits)) commits)
        format-commit
        (fn [commit]
          (let [id (str (:geschichte.commit/id commit))
                message (:geschichte.commit/message commit)
                author (:geschichte.commit/author commit)
                [_ author-name author-email]
                (or (re-matches #"^(.*?) <(.*?)>$" author)
                    [nil author ""])]
            (if format-option
              (str (-> format-option
                       (str/replace "%H" id)
                       (str/replace "%h" (subs id 0 8))
                       (str/replace "%s" message)
                       (str/replace "%an" author-name)
                       (str/replace "%ae" author-email)) "\n")
              (if oneline?
                (str (subs id 0 8) " " message "\n")
                (str "commit " id "\n"
                     "Author: " author "\n\n    " message "\n\n")))))
        commit-body
        (fn [commit]
          (let [changes (commit-changes commit)]
            (cond
              stat? (render-stat (mapv #(render-file-diff conn %) changes))
              patch? (apply str (map #(-> (render-file-diff conn %) :text) changes))
              name-only? (apply str (map #(str (:path %) "\n") changes))
              name-status? (apply str
                                  (map #(str ({:added "A" :deleted "D" :modified "M"}
                                              (:kind %) "M") "\t" (:path %) "\n")
                                       changes))
              :else "")))]
    (ok
     (apply str
            (map (fn [commit]
                   (str (format-commit commit)
                        (when (or stat? patch? name-only? name-status?)
                          (commit-body commit))))
                 commits)))))

(defn- git-ls-files [conn args]
  (validate-options! "ls-files" args
                     {"-z" :flag "-o" :flag "--others" :flag
                      "-i" :flag "--ignored" :flag
                      "--exclude-standard" :flag "--error-unmatch" :flag
                      "-s" :flag "--stage" :flag})
  (let [nul? (boolean (some #{"-z"} args))
        others? (boolean (some #{"-o" "--others"} args))
        ignored? (boolean (some #{"-i" "--ignored"} args))
        rules (ignore/rules conn)
        tracked (->> (query/stage @conn)
                     (keep (fn [[path entry]]
                             (when (= :present (:state entry)) path))))
        tracked-set (set tracked)
        untracked (remove tracked-set (repo/files conn))
        pathspecs (vec (remove #(or (= % "--") (str/starts-with? % "-")) args))
        selected (cond
                   ignored? (filter #(ignore/ignored? rules %) untracked)
                   others? (ignore/filter-visible rules untracked)
                   :else tracked)
        selected (filter #(path-selected? pathspecs %) selected)
        selected (sort selected)
        unmatched (when (some #{"--error-unmatch"} args)
                    (remove #(some (fn [path] (path-selected? [%] path)) selected)
                            pathspecs))
        separator (if nul? "\u0000" "\n")]
    (if (seq unmatched)
      (throw (ex-info (str "pathspec '" (first unmatched)
                           "' did not match any file(s) known to git") {}))
      (ok (str (str/join separator
                         (if (some #{"-s" "--stage"} args)
                           (map (fn [path]
                                  (let [{:keys [mode] :as entry}
                                        (get (repo/index-tree conn) path)]
                                    (str (format "%06o" mode) " "
                                         (git-blob-oid conn entry)
                                         " 0\t" path)))
                                selected)
                           selected))
               (when (seq selected) separator))))))

(defn- short-option-char? [args character]
  (boolean (some #(and (re-matches #"-[A-Za-z]+" %)
                       (str/includes? (subs % 1) (str character)))
                 args)))

(defn- git-grep [conn args]
  (validate-options! "grep" args
                     {"-n" :flag "--line-number" :flag
                      "-E" :flag "--extended-regexp" :flag
                      "-F" :flag "--fixed-strings" :flag
                      "-i" :flag "--ignore-case" :flag
                      "-l" :flag "--files-with-matches" :flag
                      "-q" :flag "--quiet" :flag
                      "-c" :flag "--count" :flag "-I" :flag
                      "-w" :flag "--word-regexp" :flag "-r" :flag}
                     [#(boolean (re-matches #"-[nEFilqcrIw]+" %))])
  (let [separator (index-of args "--")
        before (if (neg? separator) args (subvec args 0 separator))
        after (if (neg? separator) [] (subvec args (inc separator)))
        operands (vec (remove #(str/starts-with? % "-") before))
        pattern (first operands)
        revision-name (second operands)
        revision (when revision-name (revision/resolve conn revision-name))
        implicit-paths (if revision (subvec operands (min 2 (count operands)))
                           (subvec operands (min 1 (count operands))))
        paths (vec (concat implicit-paths after))
        tree (if revision (repo/tree-at conn revision) (repo/index-tree conn))
        expression (cond-> (if (or (some #{"-F" "--fixed-strings"} args)
                                   (short-option-char? args \F))
                             (java.util.regex.Pattern/quote (or pattern ""))
                             (or pattern ""))
                     (or (some #{"-i" "--ignore-case"} args)
                         (short-option-char? args \i))
                     (->> (str "(?i)"))
                     (or (some #{"-w" "--word-regexp"} args)
                         (short-option-char? args \w))
                     (->> (format "\\b(?:%s)\\b")))
        regex (re-pattern expression)
        line-number? (or (boolean (some #{"-n" "--line-number"} args))
                         (short-option-char? args \n))
        names? (or (boolean (some #{"-l" "--files-with-matches"} args))
                   (short-option-char? args \l))
        count? (or (boolean (some #{"-c" "--count"} args))
                   (short-option-char? args \c))
        quiet? (or (boolean (some #{"-q" "--quiet"} args))
                   (short-option-char? args \q))
        matches
        (mapcat
         (fn [path]
           (when-let [text (some-> (if revision
                                     (repo/read-at conn revision path)
                                     (repo/read conn path))
                                   decode-text)]
             (let [lines (keep-indexed
                          (fn [index line]
                            (when (re-find regex line) [(inc index) line]))
                          (str/split-lines text))]
               (if (and names? (seq lines))
                 [[path nil nil]]
                 (map #(into [path] %) lines)))))
         (filter #(path-selected? paths %) (keys tree)))]
    {:stdout (if quiet? ""
                 (apply str
                        (map (fn [[path line-number line]]
                               (cond
                                 names? (str path "\n")
                                 count? (str path ":" line "\n")
                                 line-number? (str path ":" line-number ":" line "\n")
                                 :else (str path ":" line "\n")))
                             (if count?
                               (map (fn [[path rows]] [path nil (count rows)])
                                    (group-by first matches))
                               matches))))
     :stderr ""
     :exit (if (seq matches) 0 1)}))

(defn- split-object-expression [expression]
  (let [[revision path] (str/split expression #":" 2)]
    [revision path]))

(declare git-show)

(defn- git-cat-file [conn args]
  (validate-options! "cat-file" args
                     {"-e" :flag "-t" :flag "-p" :flag})
  (let [expression (first (remove #(str/starts-with? % "-") args))
        [revision-name path] (split-object-expression expression)
        commit (revision/resolve conn revision-name)
        exists? (and commit (or (nil? path) (contains? (repo/tree-at conn commit) path)))]
    (cond
      (some #{"-e"} args) {:stdout "" :stderr "" :exit (if exists? 0 1)}
      (not exists?) (throw (ex-info (str "Not a valid object name " expression) {}))
      (some #{"-t"} args) (ok (str (if path "blob" "commit") "\n"))
      (some #{"-p"} args) (if path
                            (ok (or (decode-text (repo/read-at conn commit path))
                                    (throw (ex-info "binary blob cannot be printed" {}))))
                            (git-show conn [revision-name]))
      :else (throw (ex-info "usage: git cat-file (-e|-t|-p) <object>" {})))))

(defn- git-describe [conn args]
  (validate-options! "describe" args
                     {"--tags" :flag "--always" :flag "--abbrev" :value})
  (let [expression (or (first (remove #(str/starts-with? % "-") args)) "HEAD")
        commit (revision/require conn expression)
        commits (repo/log conn {:start commit :limit Integer/MAX_VALUE})
        positions (into {} (map-indexed (fn [i c]
                                          [(:geschichte.commit/id c) i]) commits))
        tags (for [[ref id] (repo/refs conn)
                   :when (and (str/starts-with? ref "refs/tags/")
                              (contains? positions id))]
               [(subs ref (count "refs/tags/")) id (get positions id)])
        [tag tag-id distance] (first (sort-by (juxt #(nth % 2) first) tags))
        id (str (:geschichte.commit/id commit))]
    (cond
      (and tag (zero? distance)) (ok (str tag "\n"))
      tag (ok (str tag "-" distance "-g" (subs id 0 8) "\n"))
      (some #{"--always"} args) (ok (str (subs id 0 8) "\n"))
      :else (throw (ex-info "No names found, cannot describe anything." {})))))

(defn- git-symbolic-ref [conn args]
  (validate-options! "symbolic-ref" args {"--short" :flag "-q" :flag})
  (let [name (first (remove #(str/starts-with? % "-") args))]
    (when-not (= name "HEAD")
      (throw (ex-info "only HEAD symbolic-ref is supported" {:name name})))
    (let [ref (repo/current-ref conn)]
      (ok (str (if (some #{"--short"} args)
                 (str/replace ref #"^refs/heads/" "") ref) "\n")))))

(defn- parse-worktree-add [args]
  (loop [remaining args
         opts {:force? false :quiet? false :detach? false}
         operands []]
    (if-let [arg (first remaining)]
      (cond
        (contains? #{"-f" "--force"} arg)
        (recur (next remaining) (assoc opts :force? true) operands)

        (contains? #{"-q" "--quiet"} arg)
        (recur (next remaining) (assoc opts :quiet? true) operands)

        (= "--detach" arg)
        (recur (next remaining) (assoc opts :detach? true) operands)

        (contains? #{"-b" "-B"} arg)
        (if-let [branch (second remaining)]
          (recur (nnext remaining)
                 (assoc opts :new-branch branch
                        :reset-branch? (= "-B" arg)) operands)
          (throw (ex-info (str arg " requires a branch name") {})))

        (str/starts-with? arg "-")
        (throw (ex-info (str "unsupported worktree add option: " arg) {}))

        :else (recur (next remaining) opts (conj operands arg)))
      (let [[path target & extra] operands]
        (when (or (str/blank? path) (seq extra))
          (throw (ex-info "usage: git worktree add [options] <path> [<commit-ish>]"
                          {})))
        (assoc opts :path path :target target)))))

(defn- worktree-output [records porcelain?]
  (apply str
         (map (fn [{:keys [path head branch detached? locked?]}]
                (if porcelain?
                  (str "worktree " path "\n"
                       "HEAD " (or head "0000000000000000000000000000000000000000") "\n"
                       (if detached? "detached\n"
                           (str "branch " branch "\n"))
                       (when locked? "locked\n") "\n")
                  (str path "  " (or head "00000000")
                       (if detached? " (detached HEAD)"
                           (str " [" (str/replace branch #"^refs/heads/" "") "]"))
                       "\n")))
              records)))

(defn- git-worktree [conn root workspace-ops args]
  (let [[operation & operands] args]
    (case operation
      "list"
      (do
        (validate-options! "worktree list" operands {"--porcelain" :flag})
        (let [records (if-let [list! (:list workspace-ops)]
                        (list!)
                        [{:path root
                          :head (some-> (repo/head-commit conn)
                                        :geschichte.commit/id str)
                          :branch (repo/current-ref conn)}])]
          (ok (worktree-output records
                               (boolean (some #{"--porcelain"} operands))))))

      "add"
      (if-let [add! (:add workspace-ops)]
        (do (add! (parse-worktree-add operands)) (ok))
        (throw (ex-info
                "worktree add requires a host workspace adapter; this repository was not mutated"
                {:operation operation :requires :workspace-adapter})))

      "remove"
      (if-let [remove! (:remove workspace-ops)]
        (let [force? (boolean (some #{"-f" "--force"} operands))
              paths (vec (remove #(str/starts-with? % "-") operands))]
          (validate-options! "worktree remove" operands
                             {"-f" :flag "--force" :flag})
          (when-not (= 1 (count paths))
            (throw (ex-info "usage: git worktree remove [-f] <worktree>" {})))
          (remove! {:path (first paths) :force? force?})
          (ok))
        (throw (ex-info
                "worktree remove requires a host workspace adapter; this repository was not mutated"
                {:operation operation :requires :workspace-adapter})))

      "prune"
      (do
        (validate-options! "worktree prune" operands {"-v" :flag "--verbose" :flag})
        (when-let [prune! (:prune workspace-ops)]
          (prune! {:verbose? (boolean (some #{"-v" "--verbose"} operands))}))
        (ok))

      (throw (ex-info
              (str "worktree " (or operation "")
                   " requires a host workspace adapter; this repository was not mutated")
              {:operation operation :requires :workspace-adapter})))))

(defn- format-ref [format-string ref id]
  (let [short (-> ref
                  (str/replace #"^refs/heads/" "")
                  (str/replace #"^refs/remotes/" "")
                  (str/replace #"^refs/tags/" ""))]
    (-> format-string
        (str/replace "%(refname:short)" short)
        (str/replace "%(refname)" ref)
        (str/replace "%(objectname:short)" (subs (str id) 0 (min 8 (count (str id)))))
        (str/replace "%(objectname)" (str id)))))

(defn- git-for-each-ref [conn args]
  (validate-options! "for-each-ref" args
                     {"--format" :value "--sort" :value "--count" :value})
  (let [prefix (first (remove #(str/starts-with? % "-")
                              (loop [remaining args, out []]
                                (if-let [arg (first remaining)]
                                  (if (contains? #{"--format" "--sort" "--count"} arg)
                                    (recur (nnext remaining) out)
                                    (recur (next remaining) (conj out arg)))
                                  out))))
        format-string (or (option-value args nil "--format") "%(objectname) %(refname)")
        sort-key (option-value args nil "--sort")
        descending? (str/starts-with? (or sort-key "") "-")
        limit (some-> (option-value args nil "--count") parse-long)
        refs (concat (repo/refs conn)
                     (map (fn [[ref {:keys [oid]}]] [ref oid])
                          (query/git-refs @conn)))
        refs (filter #(or (nil? prefix) (str/starts-with? (first %) prefix)) refs)
        refs (sort-by first (if descending? #(compare %2 %1) compare) refs)
        refs (if limit (take limit refs) refs)]
    (ok (apply str (map (fn [[ref id]]
                          (str (format-ref format-string ref id) "\n")) refs)))))

(defn- git-show-ref [conn args]
  (validate-options! "show-ref" args
                     {"--heads" :flag "--tags" :flag "--verify" :flag
                      "--quiet" :flag "-q" :flag})
  (let [patterns (remove #(str/starts-with? % "-") args)
        refs (concat (repo/refs conn)
                     (map (fn [[ref {:keys [oid]}]] [ref oid])
                          (query/git-refs @conn)))
        refs (filter (fn [[ref _]]
                       (and (or (not (some #{"--heads"} args))
                                (str/starts-with? ref "refs/heads/"))
                            (or (not (some #{"--tags"} args))
                                (str/starts-with? ref "refs/tags/"))
                            (or (empty? patterns)
                                (some #(or (= % ref) (str/ends-with? ref %)) patterns))))
                     refs)
        quiet? (boolean (some #{"--quiet" "-q"} args))]
    {:stdout (if quiet? ""
                 (apply str (map (fn [[ref id]] (str id " " ref "\n")) refs)))
     :stderr "" :exit (if (seq refs) 0 1)}))

(defn- git-check-ignore [conn repo-relative args]
  (validate-options! "check-ignore" args {"-v" :flag "--verbose" :flag
                                          "-q" :flag "--quiet" :flag})
  (let [verbose? (boolean (some #{"-v" "--verbose"} args))
        quiet? (boolean (some #{"-q" "--quiet"} args))
        paths (->> args
                   (remove #(or (= % "--") (str/starts-with? % "-")))
                   (mapv #(or (repo-relative %)
                              (throw (ex-info "path is outside repository"
                                              {:path %})))))
        rules (ignore/rules conn)
        matches (keep (fn [path]
                        (when (ignore/ignored? rules path)
                          (let [rule (last (filter #(re-matches (:regex %) path)
                                                   rules))]
                            [path rule])))
                      paths)]
    {:stdout (if quiet? ""
                 (apply str
                        (map (fn [[path rule]]
                               (if verbose?
                                 (str (or (:pattern rule) "") "\t" path "\n")
                                 (str path "\n")))
                             matches)))
     :stderr ""
     :exit (if (seq matches) 0 1)}))

(defn- git-ls-tree [conn args]
  (validate-options! "ls-tree" args
                     {"-r" :flag "--recursive" :flag
                      "--name-only" :flag "--name-status" :flag
                      "-z" :flag})
  (let [separator (index-of args "--")
        before (if (neg? separator) args (subvec args 0 separator))
        pathspecs (if (neg? separator) [] (subvec args (inc separator)))
        operands (vec (remove #(str/starts-with? % "-") before))
        expression (or (first operands) "HEAD")
        implicit-paths (subvec operands (min 1 (count operands)))
        commit (revision/require conn expression)
        tree (repo/tree-at conn commit)
        paths (filterv #(path-selected? (vec (concat implicit-paths pathspecs)) %)
                       (keys tree))
        nul? (boolean (some #{"-z"} args))
        names? (boolean (some #{"--name-only" "--name-status"} args))
        terminator (if nul? "\u0000" "\n")]
    (ok (apply str
               (map (fn [path]
                      (if names?
                        (str path terminator)
                        (let [{:keys [mode] :as entry} (get tree path)]
                          (str (format "%06o" mode) " blob "
                               (git-blob-oid conn entry) "\t"
                               path terminator))))
                    paths)))))

(defn- git-rev-list [conn args]
  (validate-options! "rev-list" args
                     {"--count" :flag "--left-right" :flag
                      "--max-count" :value "-n" :value "--all" :flag}
                     [#(boolean (or (re-matches #"-[0-9]+" %)
                                    (re-matches #"-n[0-9]+" %)))])
  (let [count? (boolean (some #{"--count"} args))
        left-right? (boolean (some #{"--left-right"} args))
        operands (vec (remove #(str/starts-with? % "-")
                              (loop [remaining args, out []]
                                (if-let [arg (first remaining)]
                                  (if (contains? #{"--max-count" "-n"} arg)
                                    (recur (nnext remaining) out)
                                    (recur (next remaining) (conj out arg)))
                                  out))))
        expression (or (first operands) "HEAD")]
    (when (> (count operands) 1)
      (throw (ex-info "rev-list currently accepts one revision expression"
                      {:operands operands})))
    (if (and left-right? (str/includes? expression "..."))
      (let [[left-name right-name] (str/split expression #"\.\.\." 2)
            left (revision/require conn left-name)
            right (revision/require conn right-name)
            left-ids (revision/reachable-ids conn left)
            right-ids (revision/reachable-ids conn right)
            left-only (sort (set/difference left-ids right-ids))
            right-only (sort (set/difference right-ids left-ids))]
        (if count?
          (ok (str (count left-only) "\t" (count right-only) "\n"))
          (ok (str (apply str (map #(str "<" % "\n") left-only))
                   (apply str (map #(str ">" % "\n") right-only))))))
      (let [{:keys [starts exclude]}
            (if (some #{"--all"} args)
              {:starts (keep #(repo/commit-by-id conn %) (vals (repo/refs conn)))
               :exclude #{}}
              (revision/selection conn expression))
            limit (some-> (or (option-value args "-n" "--max-count")
                              (some #(second (re-matches #"-n([0-9]+)" %)) args))
                          parse-long)
            commits (repo/log conn {:starts starts :exclude exclude
                                    :limit (or limit Integer/MAX_VALUE)})]
        (if count?
          (ok (str (count commits) "\n"))
          (ok (apply str (map #(str (:geschichte.commit/id %) "\n") commits))))))))

(defn- git-config [conn global-config args]
  (validate-options! "config" args
                     {"--global" :flag "--local" :flag
                      "-l" :flag "--list" :flag "--get" :flag
                      "--unset" :flag})
  (let [global? (boolean (some #{"--global"} args))
        config (if global?
                 @global-config
                 (merge @global-config (repo/configuration conn)))
        args (vec (remove #{"--global" "--local"} args))
        list? (some #{"-l" "--list"} args)
        get? (some #{"--get"} args)
        unset? (some #{"--unset"} args)
        operands (vec (remove #(str/starts-with? % "-") args))
        key (first operands)
        value (second operands)]
    (cond
      list?
      (ok (apply str (map (fn [[key value]] (str key "=" value "\n"))
                          (sort config))))

      unset?
      (do (if global?
            (swap! global-config dissoc key)
            (repo/unset-config! conn key))
          (ok))

      (or get? (and key (nil? value)))
      (if-let [value (get config key)]
        (ok (str value "\n"))
        {:stdout "" :stderr "" :exit 1})

      (and key value)
      (do (if global?
            (swap! global-config assoc key value)
            (repo/set-config! conn key value))
          (ok))

      :else
      (fail "invalid config invocation"))))

(defn- resolve-commit [conn expression]
  (revision/resolve conn (or expression "HEAD")))

(defn- expand-repository-paths [conn repo-relative specs extra-paths]
  (let [known (->> (concat (keys (repo/tree-at conn))
                           (keys (query/stage @conn))
                           (repo/files conn)
                           extra-paths)
                   distinct sort vec)
        specs (mapv #(or (repo-relative %)
                         (throw (ex-info "pathspec is outside repository"
                                         {:path %})))
                    specs)]
    (when (empty? specs)
      (throw (ex-info "you must specify path(s) to restore" {})))
    (vec
     (distinct
      (mapcat (fn [spec]
                (let [matches (filterv #(or (= spec %)
                                            (str/blank? spec)
                                            (str/starts-with? % (str spec "/")))
                                       known)]
                  (when (empty? matches)
                    (throw (ex-info
                            (str "pathspec '" spec "' did not match any files")
                            {:pathspec spec})))
                  matches))
              specs)))))

(defn- git-restore [conn repo-relative args]
  (validate-options! "restore" args
                     {"-S" :flag "--staged" :flag
                      "-W" :flag "--worktree" :flag
                      "-s" :value "--source" :value})
  (let [staged? (boolean (some #{"-S" "--staged"} args))
        explicit-worktree? (boolean (some #{"-W" "--worktree"} args))
        source-name (or (option-value args "-s" "--source")
                        (some #(when (str/starts-with? % "--source=")
                                 (subs % (count "--source="))) args))
        source (when source-name
                 (or (resolve-commit conn source-name)
                     (throw (ex-info (str "could not resolve " source-name) {}))))
        separator (index-of args "--")
        paths (if (neg? separator)
                (remove #(or (str/starts-with? % "-")
                             (= % source-name)) args)
                (subvec args (inc separator)))
        source-tree (when source (repo/tree-at conn source))
        paths (expand-repository-paths conn repo-relative paths (keys source-tree))]
    (repo/restore-paths! conn paths
                         {:source source
                          :staged? staged?
                          :worktree? (or explicit-worktree? (not staged?))})
    (ok)))

(defn- git-reset [conn repo-relative args]
  (validate-options! "reset" args
                     {"--soft" :flag "--mixed" :flag "--hard" :flag
                      "-q" :flag "--quiet" :flag})
  (let [mode (cond
               (some #{"--soft"} args) :soft
               (some #{"--hard"} args) :hard
               :else :mixed)
        separator (index-of args "--")
        before (if (neg? separator) args (subvec args 0 separator))
        operands (vec (remove #(str/starts-with? % "-") before))
        revision-name (or (first operands) "HEAD")
        revision (or (resolve-commit conn revision-name)
                     (throw (ex-info (str "ambiguous argument '" revision-name "'") {})))
        path-args (if (neg? separator)
                    (subvec operands (min 1 (count operands)))
                    (subvec args (inc separator)))]
    (if (seq path-args)
      (let [tree (repo/tree-at conn revision)
            paths (expand-repository-paths conn repo-relative path-args (keys tree))]
        (repo/restore-paths! conn paths {:source revision
                                         :staged? true
                                         :worktree? false})
        (ok))
      (do (repo/reset! conn revision {:mode mode})
          (ok (str "HEAD is now at "
                   (subs (str (:geschichte.commit/id revision)) 0 8) " "
                   (:geschichte.commit/message revision) "\n"))))))

(defn- git-rm [conn repo-relative args]
  (validate-options! "rm" args {"--cached" :flag "-q" :flag
                                "--quiet" :flag "-r" :flag
                                "-f" :flag "--force" :flag
                                "--ignore-unmatch" :flag}
                     [#(boolean (re-matches #"-[qrf]+" %))])
  (let [cached? (boolean (some #{"--cached"} args))
        quiet? (boolean (some #(or (= % "-q") (= % "--quiet")
                                   (boolean (re-find #"q" %))) args))
        paths (remove #(str/starts-with? % "-") args)
        paths (expand-repository-paths conn repo-relative paths [])]
    (repo/restore-paths! conn paths {:source :empty
                                     :staged? true
                                     :worktree? (not cached?)})
    (ok (if quiet? "" (apply str (map #(str "rm '" % "'\n") paths))))))

(defn- git-mv [conn repo-relative args]
  (validate-options! "mv" args {})
  (let [[from to & extra] args]
    (when (or (str/blank? from) (str/blank? to) (seq extra))
      (throw (ex-info "usage: git mv <source> <destination>" {})))
    (let [from (or (repo-relative from)
                   (throw (ex-info "source is outside repository" {:path from})))
          to (or (repo-relative to)
                 (throw (ex-info "destination is outside repository" {:path to})))
          tracked (set (keys (query/stage @conn)))
          moved (filterv #(or (= from %)
                              (str/starts-with? % (str from "/")))
                         tracked)]
      (when (empty? moved)
        (throw (ex-info (str "bad source, source=" from
                             ", destination=" to) {})))
      (gfs/rename! conn from to)
      (let [destinations (mapv #(str to (subs % (count from))) moved)]
        (repo/stage! conn (into moved destinations))
        (ok)))))

(defn- git-clean [conn repo-relative args]
  (validate-options! "clean" args
                     {"-n" :flag "--dry-run" :flag
                      "-f" :flag "--force" :flag "-d" :flag})
  (let [dry-run? (boolean (some #{"-n" "--dry-run"} args))
        force? (boolean (some #{"-f" "--force"} args))
        specs (vec (remove #(str/starts-with? % "-") args))
        cwd-relative (or (repo-relative ".") "")
        specs (if (seq specs)
                (mapv #(or (repo-relative %)
                           (throw (ex-info "pathspec is outside repository"
                                           {:path %}))) specs)
                [cwd-relative])
        tracked (set (keys (query/stage @conn)))
        rules (ignore/rules conn)
        selected (->> (repo/files conn)
                      (remove tracked)
                      (remove #(ignore/ignored? rules %))
                      (filterv #(path-selected? specs %)))]
    (when-not (or dry-run? force?)
      (throw (ex-info "clean.requireForce defaults to true; use -n or -f" {})))
    (when-not dry-run?
      (doseq [path selected] (repo/remove! conn path)))
    (ok (apply str
               (map #(str (if dry-run? "Would remove " "Removing ") % "\n")
                    selected)))))

(defn- git-show [conn args]
  (validate-options! "show" args
                     {"--no-patch" :flag "-s" :flag "--oneline" :flag
                      "--stat" :flag "--name-only" :flag
                      "--diff-filter" :value}
                     [#(or (str/starts-with? % "--format=")
                           (str/starts-with? % "--pretty="))])
  (let [options (filter #(str/starts-with? % "-") args)
        operand (first (remove #(str/starts-with? % "-") args))
        [revision path] (when operand (str/split operand #":" 2))
        commit (resolve-commit conn revision)]
    (when-not commit
      (throw (ex-info (str "bad object " (or revision "HEAD")) {})))
    (if path
      (if-let [value (repo/read-at conn commit path)]
        (ok (or (decode-text value)
                (throw (ex-info "binary object cannot be written to text stdout"
                                {:path path}))))
        (throw (ex-info (str "path '" path "' does not exist in '" revision "'") {})))
      (let [format-option (some #(cond
                                   (str/starts-with? % "--format=")
                                   (subs % (count "--format="))
                                   (str/starts-with? % "--pretty=")
                                   (subs % (count "--pretty="))) options)
            message (:geschichte.commit/message commit)
            header (cond
                     (some? format-option)
                     (str (-> format-option
                              (str/replace "%H" (str (:geschichte.commit/id commit)))
                              (str/replace "%h" (subs (str (:geschichte.commit/id commit)) 0 8))
                              (str/replace "%s" message))
                          (when-not (str/blank? format-option) "\n"))
                     (some #{"--oneline"} options)
                     (str (subs (str (:geschichte.commit/id commit)) 0 8)
                          " " message "\n")
                     :else
                     (str "commit " (:geschichte.commit/id commit) "\n"
                          "Author: " (:geschichte.commit/author commit) "\n\n    "
                          message "\n"))
            no-patch? (boolean (some #{"--no-patch" "-s"} options))
            stat? (boolean (some #{"--stat"} options))
            name-only? (boolean (some #{"--name-only"} options))
            diff-filter (option-value args nil "--diff-filter")
            allowed-kinds (when diff-filter
                            (set (keep {\A :added \D :deleted \M :modified}
                                       diff-filter)))
            parent-id (get-in commit [:geschichte.commit/parents 0
                                      :geschichte.commit/id])
            parent (when parent-id (repo/commit-by-id conn parent-id))
            changes (filterv #(or (nil? allowed-kinds)
                                  (contains? allowed-kinds (:kind %)))
                             (repo/changes conn (or parent :empty) commit))
            rendered (when (or stat? name-only? (not no-patch?))
                       (mapv #(render-file-diff conn %) changes))
            body (cond
                   stat? (render-stat rendered)
                   name-only? (apply str (map #(str (:path %) "\n") changes))
                   no-patch? ""
                   :else (apply str (map :text rendered)))]
        (ok (str header (when (seq body) "\n") body))))))

(defn- git-branch [conn args]
  (validate-options! "branch" args
                     {"--show-current" :flag "-d" :flag
                      "-D" :flag "--delete" :flag
                      "-a" :flag "--all" :flag "-r" :flag "--remotes" :flag
                      "-v" :flag "-vv" :flag "--verbose" :flag
                      "--contains" :value "--sort" :value "--format" :value
                      "--list" :flag "-m" :flag "-M" :flag
                      "-q" :flag "--quiet" :flag "-f" :flag "--force" :flag})
  (cond
    (some #{"--show-current"} args)
    (ok (str (str/replace (repo/current-ref conn) #"^refs/heads/" "") "\n"))

    (some #{"-d" "-D" "--delete"} args)
    (let [name (first (remove #(str/starts-with? % "-") args))
          force? (boolean (some #{"-D"} args))]
      (repo/delete-branch! conn name {:force? force?})
      (ok (if (some #{"-q" "--quiet"} args) ""
              (str "Deleted branch " name ".\n"))))

    (some #{"-m" "-M"} args)
    (let [operands (vec (remove #(str/starts-with? % "-") args))
          [old-name new-name] (if (= 1 (count operands))
                                [(str/replace (repo/current-ref conn)
                                              #"^refs/heads/" "")
                                 (first operands)]
                                operands)]
      (when (or (str/blank? old-name) (str/blank? new-name)
                (> (count operands) 2))
        (throw (ex-info "usage: git branch (-m|-M) [<oldbranch>] <newbranch>"
                        {})))
      (repo/rename-ref! conn old-name new-name
                        {:force? (boolean (some #{"-M"} args))})
      (ok))

    (some #{"-f" "--force"} args)
    (let [[name start & extra] (remove #(str/starts-with? % "-") args)
          commit (revision/require conn (or start "HEAD"))]
      (when (or (str/blank? name) (seq extra))
        (throw (ex-info "usage: git branch -f <branch> [<start-point>]" {})))
      (if (contains? (repo/refs conn) (str "refs/heads/" name))
        (repo/set-ref! conn name commit)
        (repo/create-ref! conn name commit))
      (ok))

    (or (empty? args)
        (some #{"-a" "--all" "-r" "--remotes" "-v" "-vv" "--verbose"
                "--contains" "--sort" "--format" "--list"} args)
        (some #(or (str/starts-with? % "--sort=")
                   (str/starts-with? % "--format=")) args))
    (let [current (repo/current-ref conn)
          remotes? (boolean (some #{"-r" "--remotes"} args))
          all? (boolean (some #{"-a" "--all"} args))
          contains-expression (option-value args nil "--contains")
          contains-id (some->> contains-expression (revision/require conn)
                               :geschichte.commit/id)
          local (for [[ref id] (repo/refs conn)
                      :when (str/starts-with? ref "refs/heads/")]
                  {:ref ref :id id :current? (= ref current)})
          remote (for [[ref {:keys [oid]}] (query/git-refs @conn)
                       :when (str/starts-with? ref "refs/remotes/")]
                   {:ref ref :id oid :current? false})
          entries (cond remotes? remote all? (concat local remote) :else local)
          entries (if contains-id
                    (filter (fn [{:keys [id]}]
                              (when-let [commit (repo/commit-by-id conn id)]
                                (contains? (revision/reachable-ids conn commit)
                                           contains-id)))
                            entries)
                    entries)
          format-option (some #(when (str/starts-with? % "--format=")
                                 (subs % (count "--format="))) args)]
      (ok (apply str
                 (for [{:keys [ref id current?]} (sort-by :ref entries)
                       :let [short (-> ref
                                       (str/replace #"^refs/heads/" "")
                                       (str/replace #"^refs/remotes/" ""))]
                       :when (or (not (some #{"--list"} args))
                                 (let [pattern (first (remove #(str/starts-with? % "-")
                                                              args))]
                                   (or (nil? pattern)
                                       (re-matches (pathspec-regex pattern) short))))]
                   (if format-option
                     (str (-> format-option
                              (str/replace "%(refname:short)" short)
                              (str/replace "%(refname)" ref)
                              (str/replace "%(objectname)" (str id))) "\n")
                     (str (if current? "* " "  ") short
                          (when (some #{"-v" "-vv" "--verbose"} args)
                            (str " " id))
                          "\n"))))))

    :else
    (let [[name start & extra] (remove #(str/starts-with? % "-") args)]
      (if name
        (do
          (when (seq extra)
            (throw (ex-info "too many arguments for branch" {:args args})))
          (repo/create-ref! conn name
                            (when start (revision/require conn start)))
          (ok))
        (throw (ex-info "unsupported branch invocation" {:args args}))))))

(defn- git-tag [conn args]
  (validate-options! "tag" args {"-d" :flag "--delete" :flag
                                 "-l" :flag "--list" :flag
                                 "--sort" :value "--contains" :value
                                 "--points-at" :value})
  (let [delete? (boolean (some #{"-d" "--delete"} args))
        list? (boolean (some #{"-l" "--list" "--sort" "--contains"
                               "--points-at"} args))
        value-options #{"--sort" "--contains" "--points-at"}
        operands (vec
                  (remove #(str/starts-with? % "-")
                          (loop [remaining args, out []]
                            (if-let [arg (first remaining)]
                              (if (contains? value-options arg)
                                (recur (nnext remaining) out)
                                (recur (next remaining) (conj out arg)))
                              out))))]
    (cond
      delete?
      (do
        (when (empty? operands)
          (throw (ex-info "tag name required" {})))
        (doseq [name operands]
          (repo/delete-branch! conn (str "refs/tags/" name) {:force? true}))
        (ok (apply str (map #(str "Deleted tag '" % "'\n") operands))))

      (or list? (empty? operands))
      (let [pattern (first operands)
            contains-id (some-> (option-value args nil "--contains")
                                (revision/require conn)
                                :geschichte.commit/id)
            points-id (some-> (option-value args nil "--points-at")
                              (revision/require conn)
                              :geschichte.commit/id)
            descending? (some #(str/starts-with? % "--sort=-") args)
            tags (for [[ref id] (repo/refs conn)
                       :when (str/starts-with? ref "refs/tags/")
                       :let [name (subs ref (count "refs/tags/"))
                             commit (repo/commit-by-id conn id)]
                       :when (and
                              (or (nil? pattern)
                                  (re-matches (pathspec-regex pattern) name))
                              (or (nil? points-id) (= points-id id))
                              (or (nil? contains-id)
                                  (contains? (revision/reachable-ids conn commit)
                                             contains-id)))]
                   name)
            tags (sort (if descending? #(compare %2 %1) compare) tags)]
        (ok (apply str (map #(str % "\n") tags))))

      (<= 1 (count operands) 2)
      (let [[name expression] operands]
        (repo/create-ref! conn (str "refs/tags/" name)
                          (revision/require conn (or expression "HEAD")))
        (ok))

      :else (throw (ex-info "too many arguments for tag" {:args args})))))

(declare effective-config)

(defn- git-merge [conn global-config args]
  (validate-options! "merge" args
                     {"--ff-only" :flag "--no-commit" :flag
                      "--no-edit" :flag "-m" :value "--message" :value})
  (let [message-option (option-value args "-m" "--message")
        operands (loop [remaining args, out []]
                   (if-let [arg (first remaining)]
                     (cond
                       (contains? #{"-m" "--message"} arg)
                       (recur (nnext remaining) out)
                       (str/starts-with? arg "-") (recur (next remaining) out)
                       :else (recur (next remaining) (conj out arg)))
                     out))
        _ (when (not= 1 (count operands))
            (throw (ex-info "merge requires exactly one revision" {:args args})))
        expression (first operands)
        ours (or (repo/head-commit conn)
                 (throw (ex-info "cannot merge into an unborn branch" {})))
        theirs (revision/require conn expression)
        plan (merge/plan conn (:geschichte.commit/id ours)
                         (:geschichte.commit/id theirs))]
    (case (:kind plan)
      :up-to-date (ok "Already up to date.\n")

      :fast-forward
      (do
        (when-not (:clean? (repo/status conn))
          (throw (ex-info "merge would overwrite local changes" {})))
        (repo/reset! conn theirs {:mode :hard})
        (ok (str "Updating "
                 (subs (str (:geschichte.commit/id ours)) 0 8) ".."
                 (subs (str (:geschichte.commit/id theirs)) 0 8) "\n"
                 "Fast-forward\n")))

      :merge
      (cond
        (some #{"--ff-only"} args)
        (throw (ex-info "Not possible to fast-forward, aborting." {}))

        (not (:clean? plan))
        {:stdout (apply str
                        (map #(str "CONFLICT (content): Merge conflict in " % "\n")
                             (keys (:conflicts plan))))
         :stderr "Automatic merge failed; fix conflicts and then commit the result.\n"
         :exit 1}

        :else
        (do
          (repo/prepare-merge! conn plan)
          (if (some #{"--no-commit"} args)
            (ok "Automatic merge went well; stopped before committing as requested\n")
            (let [message (or message-option
                              (str "Merge '" expression "'"))
                  author (configured-author
                          (effective-config conn global-config))
                  commit (repo/commit! conn {:message message :author author})]
              (ok (str "[" (str/replace (:geschichte.ref/name commit)
                                        #"^refs/heads/" "") " "
                       (:geschichte.commit/id commit) "] " message "\n")))))))))

(defn- git-merge-base [conn args]
  (validate-options! "merge-base" args {"--is-ancestor" :flag})
  (let [ancestor? (boolean (some #{"--is-ancestor"} args))
        operands (vec (remove #(str/starts-with? % "-") args))]
    (when (not= 2 (count operands))
      (throw (ex-info "merge-base requires exactly two revisions" {:args args})))
    (let [[left right] (mapv #(revision/require conn %) operands)
          left-id (:geschichte.commit/id left)
          right-id (:geschichte.commit/id right)
          base-id (merge/merge-base conn left-id right-id)]
      (if ancestor?
        {:stdout "" :stderr "" :exit (if (= left-id base-id) 0 1)}
        (if base-id
          (ok (str base-id "\n"))
          {:stdout "" :stderr "" :exit 1})))))

(def stash-prefix "refs/geschichte/stash/")

(defn- stash-records [conn]
  (->> (repo/refs conn)
       (keep (fn [[ref id]]
               (when-let [[_ key kind]
                          (re-matches #"^refs/geschichte/stash/([^/]+)/(work|index)$"
                                      ref)]
                 {:key key :kind kind :ref ref
                  :commit (repo/commit-by-id conn id)})))
       (group-by :key)
       (keep (fn [[key entries]]
               (let [by-kind (into {} (map (juxt :kind identity)) entries)
                     work (get by-kind "work")]
                 (when work
                   {:key key
                    :work (:commit work)
                    :index (:commit (get by-kind "index"))
                    :paths (some-> (get (repo/configuration conn)
                                        (str "geschichte.stash." key ".paths"))
                                   (str/split #"\n") set)
                    :refs (mapv :ref entries)}))))
       (sort-by #(some-> % :work :geschichte.commit/time .getTime) >)
       vec))

(defn- select-stash [conn operand]
  (let [records (stash-records conn)
        index (if-let [[_ n] (and operand
                                  (re-matches #"stash@\{([0-9]+)\}" operand))]
                (parse-long n)
                0)]
    (or (get records index)
        (throw (ex-info "No stash entries found." {:stash operand})))))

(defn- apply-stash! [conn {:keys [work index paths]}]
  (let [paths (or paths
                  (set/union (set (keys (repo/index-tree conn)))
                             (set (keys (repo/tree-at conn work)))
                             (set (keys (repo/tree-at conn index)))))]
    (repo/restore-paths! conn paths {:source work
                                     :staged? false :worktree? true})
    (repo/restore-paths! conn paths {:source index
                                     :staged? true :worktree? false})))

(defn- git-stash [conn global-config repo-relative args]
  (let [operation (if (contains? #{"push" "pop" "apply" "list" "show"
                                   "drop" "clear"} (first args))
                    (first args) "push")
        args (if (= operation (first args)) (vec (rest args)) (vec args))]
    (case operation
      "list"
      (do
        (validate-options! "stash list" args {})
        (ok (apply str
                   (map-indexed
                    (fn [index {:keys [work]}]
                      (str "stash@{" index "}: "
                           (:geschichte.commit/message work) "\n"))
                    (stash-records conn)))))

      "clear"
      (do
        (validate-options! "stash clear" args {})
        (let [records (stash-records conn)]
          (doseq [{:keys [refs]} records, ref refs]
            (repo/delete-branch! conn ref {:force? true}))
          (doseq [{:keys [key]} records]
            (repo/unset-config! conn (str "geschichte.stash." key ".paths"))))
        (ok))

      ("pop" "apply" "drop" "show")
      (do
        (validate-options! (str "stash " operation) args
                           {"-q" :flag "--quiet" :flag
                            "-p" :flag "--patch" :flag "--stat" :flag})
        (let [operand (first (remove #(str/starts-with? % "-") args))
              {:keys [refs work key paths] :as stash} (select-stash conn operand)]
          (case operation
            "show" (git-show conn
                             (cond-> [(str (:geschichte.commit/id work))]
                               (not (some #{"-p" "--patch"} args))
                               (conj "--stat")))
            "drop" (do (doseq [ref refs]
                         (repo/delete-branch! conn ref {:force? true}))
                       (repo/unset-config! conn
                                           (str "geschichte.stash." key ".paths"))
                       (ok))
            (do
              (let [status (repo/status conn)
                    changed (set (concat (:staged status) (:unstaged status)
                                         (:untracked status)))]
                (when-not (if paths
                            (empty? (set/intersection changed paths))
                            (:clean? status))
                  (throw (ex-info "local changes would be overwritten by stash"
                                  {}))))
              (apply-stash! conn stash)
              (when (= operation "pop")
                (doseq [ref refs]
                  (repo/delete-branch! conn ref {:force? true}))
                (repo/unset-config! conn
                                    (str "geschichte.stash." key ".paths")))
              (ok)))))

      "push"
      (do
        (validate-options! "stash push" args
                           {"-q" :flag "--quiet" :flag
                            "-u" :flag "--include-untracked" :flag
                            "-m" :value "--message" :value})
        (let [separator (index-of args "--")
              path-args (when-not (neg? separator)
                          (subvec args (inc separator)))
              selected-paths (when (seq path-args)
                               (set (expand-repository-paths
                                     conn repo-relative path-args [])))
              head (or (repo/head-commit conn)
                       (throw (ex-info "You do not have the initial commit yet" {})))
              status (repo/status conn)
              include-untracked? (boolean
                                  (some #{"-u" "--include-untracked"} args))
              changed (set (concat (:staged status) (:unstaged status)
                                   (when include-untracked? (:untracked status))))
              stashable? (seq (if selected-paths
                                (set/intersection changed selected-paths)
                                changed))]
          (if-not stashable?
            {:stdout "No local changes to save\n" :stderr "" :exit 0}
            (let [key (str (random-uuid))
                  index-ref (str stash-prefix key "/index")
                  work-ref (str stash-prefix key "/work")
                  message (or (option-value args "-m" "--message")
                              (str "WIP on "
                                   (str/replace (repo/current-ref conn)
                                                #"^refs/heads/" "")))
                  index-changed? (seq (repo/changes conn :head :index))
                  index-commit (if index-changed?
                                 (repo/commit! conn
                                               {:message (str "index: " message)
                                                :author (configured-author
                                                         (effective-config
                                                          conn global-config))})
                                 head)]
              (repo/create-ref! conn index-ref index-commit)
              (when selected-paths
                (repo/set-config! conn (str "geschichte.stash." key ".paths")
                                  (str/join "\n" (sort selected-paths))))
              (when index-changed? (repo/reset! conn head {:mode :mixed}))
              (if include-untracked?
                (repo/stage-all! conn)
                (repo/stage! conn (keys (query/stage @conn))))
              (let [work-commit (repo/commit! conn
                                              {:message message
                                               :author (configured-author
                                                        (effective-config
                                                         conn global-config))})]
                (repo/create-ref! conn work-ref work-commit)
                (repo/reset! conn head {:mode :hard})
                (when selected-paths
                  (let [all-paths (set/union
                                   (set (keys (repo/tree-at conn work-commit)))
                                   (set (keys (repo/tree-at conn index-commit)))
                                   (set (keys (repo/tree-at conn head))))
                        unselected (set/difference all-paths selected-paths)]
                    (repo/restore-paths! conn unselected
                                         {:source work-commit :staged? false
                                          :worktree? true})
                    (repo/restore-paths! conn unselected
                                         {:source index-commit :staged? true
                                          :worktree? false})))
                (ok (if (some #{"-q" "--quiet"} args) ""
                        (str "Saved working directory and index state "
                             message "\n")))))))))))

(defn- changed-tree [conn commit]
  (let [parent-id (get-in commit [:geschichte.commit/parents 0
                                  :geschichte.commit/id])
        parent (when parent-id (repo/commit-by-id conn parent-id))]
    {:parent parent
     :changes (repo/changes conn (or parent :empty) commit)}))

(defn- apply-tree-changes [tree changes]
  (reduce (fn [tree {:keys [path after]}]
            (if after (assoc tree path after) (dissoc tree path)))
          tree changes))

(defn- changes-applicable? [base-tree {:keys [parent changes]} conn]
  (let [parent-tree (if parent (repo/tree-at conn parent) {})]
    (every? (fn [{:keys [path]}]
              (= (get base-tree path) (get parent-tree path)))
            changes)))

(defn- apply-commit! [conn global-config commit]
  (let [{:keys [changes] :as patch} (changed-tree conn commit)
        head-tree (repo/tree conn :head)]
    (when-not (changes-applicable? head-tree patch conn)
      (throw (ex-info "cherry-pick conflicts with the current tree"
                      {:commit (:geschichte.commit/id commit)})))
    (let [paths (mapv :path changes)]
      (repo/restore-paths! conn paths {:source commit
                                       :staged? true :worktree? true})
      (repo/commit! conn {:message (:geschichte.commit/message commit)
                          :author (or (:geschichte.commit/author commit)
                                      (configured-author
                                       (effective-config conn global-config)))}))))

(defn- git-cherry-pick [conn global-config args]
  (validate-options! "cherry-pick" args
                     {"-x" :flag "--no-commit" :flag "-n" :flag})
  (let [expressions (vec (remove #(str/starts-with? % "-") args))]
    (when (empty? expressions)
      (throw (ex-info "cherry-pick requires at least one commit" {})))
    (when-not (:clean? (repo/status conn))
      (throw (ex-info "your local changes would be overwritten by cherry-pick"
                      {})))
    (let [commits (mapv #(revision/require conn %) expressions)
          final (reduce (fn [_ commit]
                          (apply-commit! conn global-config commit))
                        nil commits)]
      (ok (str "[" (str/replace (repo/current-ref conn) #"^refs/heads/" "")
               " " (:geschichte.commit/id final) "] "
               (:geschichte.commit/message final) "\n")))))

(defn- git-rebase [conn global-config args]
  (validate-options! "rebase" args
                     {"--onto" :value "--continue" :flag "--skip" :flag
                      "--abort" :flag "--quit" :flag
                      "-q" :flag "--quiet" :flag})
  (when (some #{"--continue" "--skip" "--abort" "--quit"} args)
    (throw (ex-info "no rebase in progress" {})))
  (when-not (:clean? (repo/status conn))
    (throw (ex-info "cannot rebase: You have unstaged changes." {})))
  (let [onto-name (option-value args nil "--onto")
        operands (vec
                  (remove #(str/starts-with? % "-")
                          (loop [remaining args, out []]
                            (if-let [arg (first remaining)]
                              (if (= arg "--onto")
                                (recur (nnext remaining) out)
                                (recur (next remaining) (conj out arg)))
                              out))))
        [upstream-name branch-name & extra] operands]
    (when (or (nil? upstream-name) (seq extra))
      (throw (ex-info "usage: git rebase [--onto <newbase>] <upstream> [<branch>]"
                      {:args args})))
    (when branch-name (repo/checkout! conn branch-name))
    (let [head (repo/head-commit conn)
          upstream (revision/require conn upstream-name)
          onto (revision/require conn (or onto-name upstream-name))
          selection (revision/selection
                     conn (str upstream-name ".."
                               (:geschichte.commit/id head)))
          commits (vec (reverse (repo/log conn {:starts (:starts selection)
                                                :exclude (:exclude selection)
                                                :limit Integer/MAX_VALUE})))
          initial-tree (repo/tree-at conn onto)
          applicable?
          (loop [tree initial-tree, commits commits]
            (if-let [commit (first commits)]
              (let [patch (changed-tree conn commit)]
                (if (changes-applicable? tree patch conn)
                  (recur (apply-tree-changes tree (:changes patch))
                         (subvec commits 1))
                  false))
              true))]
      (when-not applicable?
        (throw (ex-info "rebase would produce conflicts; repository left unchanged"
                        {})))
      (repo/reset! conn onto {:mode :hard})
      (doseq [commit commits] (apply-commit! conn global-config commit))
      (ok (str "Successfully rebased and updated "
               (repo/current-ref conn) ".\n")))))

(defn- git-checkout [conn repo-relative args]
  (validate-options! "checkout" args
                     {"-f" :flag "--force" :flag
                      "-q" :flag "--quiet" :flag
                      "-b" :flag "-B" :flag "-c" :flag "-C" :flag})
  (let [separator (index-of args "--")]
    (if (not (neg? separator))
      (let [revision-name (when (pos? separator)
                            (first (remove #(str/starts-with? % "-")
                                           (subvec args 0 separator))))
            source (when revision-name
                     (or (resolve-commit conn revision-name)
                         (throw (ex-info (str "invalid reference: " revision-name) {}))))
            paths (expand-repository-paths
                   conn repo-relative (subvec args (inc separator))
                   (keys (when source (repo/tree-at conn source))))]
        (repo/restore-paths! conn paths
                             {:source source
                              :staged? (boolean source)
                              :worktree? true})
        (ok))
      (let [force? (boolean (some #{"-f" "--force"} args))
            quiet? (boolean (some #{"-q" "--quiet"} args))
            create? (boolean (some #{"-b" "-B" "-c" "-C"} args))
            operands (vec (remove #(str/starts-with? % "-") args))
            [name start & extra] operands]
        (when (or (nil? name) (str/starts-with? name "-"))
          (throw (ex-info "you must specify a branch" {})))
        (when (or (and (not create?) start) (seq extra))
          (throw (ex-info "too many arguments for checkout" {:args args})))
        (when create?
          (repo/create-ref! conn name
                            (when start (revision/require conn start))))
        (repo/checkout! conn name {:force? force?})
        (ok (if quiet? "" (str "Switched to branch '" name "'\n")))))))

(defn- git-rev-parse [conn root repo-relative args]
  (validate-options! "rev-parse" args
                     {"--show-toplevel" :flag
                      "--is-inside-work-tree" :flag
                      "--abbrev-ref" :value "--short" :flag
                      "--symbolic-full-name" :flag
                      "--verify" :flag "--quiet" :flag "-q" :flag
                      "--git-dir" :flag "--show-prefix" :flag
                      "--show-cdup" :flag}
                     [#(str/starts-with? % "--short=")])
  (cond
    (some #{"--show-toplevel"} args) (ok (str root "\n"))
    (some #{"--is-inside-work-tree"} args) (ok "true\n")
    (some #{"--git-dir"} args) (ok (str root "/.geschichte\n"))
    (some #{"--show-prefix"} args)
    (let [prefix (or (repo-relative ".") "")]
      (ok (if (str/blank? prefix) "" (str prefix "/\n"))))
    (some #{"--show-cdup"} args)
    (let [prefix (or (repo-relative ".") "")
          depth (count (remove str/blank? (str/split prefix #"/")))]
      (ok (apply str (repeat depth "../"))))
    (and (some #{"--abbrev-ref"} args) (some #{"HEAD"} args))
    (ok (str (str/replace (repo/current-ref conn) #"^refs/heads/" "") "\n"))
    :else
    (let [expressions (vec (remove #(str/starts-with? % "-") args))
          short-option (some #(when (str/starts-with? % "--short=")
                                (subs % (count "--short="))) args)
          short? (boolean (some #(or (= % "--short")
                                     (str/starts-with? % "--short=")) args))
          length (if short-option (parse-long short-option) 8)]
      (if (seq expressions)
        (ok (apply str
                   (map (fn [expression]
                          (let [id (str (:geschichte.commit/id
                                         (revision/require conn expression)))]
                            (str (if short? (subs id 0 (min length (count id))) id)
                                 "\n")))
                        expressions)))
        (fail "unsupported rev-parse invocation")))))

(defn- effective-config [conn global-config]
  (merge @global-config (repo/configuration conn)))

(defn- remote-names [config]
  (->> (keys config)
       (keep #(second (re-matches #"remote\.([^.]+)\.url" %)))
       distinct sort vec))

(defn- git-remote [conn global-config args]
  (let [config (effective-config conn global-config)
        [operation & operands] args]
    (cond
      (nil? operation)
      (ok (apply str (map #(str % "\n") (remote-names config))))

      (contains? #{"-v" "--verbose"} operation)
      (ok (apply str
                 (mapcat (fn [name]
                           (let [url (get config (str "remote." name ".url"))]
                             [(str name "\t" url " (fetch)\n")
                              (str name "\t" url " (push)\n")]))
                         (remote-names config))))

      (= operation "add")
      (let [[name url & extra] operands]
        (when (or (str/blank? name) (str/blank? url) (seq extra))
          (throw (ex-info "usage: git remote add <name> <url>" {})))
        (repo/set-config! conn (str "remote." name ".url") url)
        (ok))

      (contains? #{"remove" "rm"} operation)
      (let [[name & extra] operands]
        (when (or (str/blank? name) (seq extra))
          (throw (ex-info "usage: git remote remove <name>" {})))
        (when-not (contains? config (str "remote." name ".url"))
          (throw (ex-info (str "No such remote: '" name "'") {})))
        (repo/unset-config! conn (str "remote." name ".url"))
        (ok))

      (= operation "get-url")
      (let [[name & extra] operands
            url (get config (str "remote." name ".url"))]
        (when (or (str/blank? name) (seq extra))
          (throw (ex-info "usage: git remote get-url <name>" {})))
        (if url (ok (str url "\n"))
            (throw (ex-info (str "No such remote: '" name "'") {}))))

      (= operation "set-url")
      (let [[name url & extra] operands]
        (when (or (str/blank? name) (str/blank? url) (seq extra))
          (throw (ex-info "usage: git remote set-url <name> <url>" {})))
        (when-not (contains? config (str "remote." name ".url"))
          (throw (ex-info (str "No such remote: '" name "'") {})))
        (repo/set-config! conn (str "remote." name ".url") url)
        (ok))

      :else (throw (ex-info (str "unsupported remote subcommand: " operation)
                            {})))))

(defn- remote-context [conn global-config args]
  (let [remote (or (first (remove #(str/starts-with? % "-") args)) "origin")
        url (get (effective-config conn global-config)
                 (str "remote." remote ".url"))]
    (when-not url
      (throw (ex-info (str "'" remote "' does not appear to be a git repository")
                      {:remote remote})))
    {:remote remote :url url}))

(defn- require-remote-op [remote-ops operation]
  (or (get remote-ops operation)
      (throw (ex-info
              (str "remote " (name operation)
                   " is unavailable in this host; a permitted transport adapter is required")
              {:operation operation}))))

(defn- git-fetch [conn global-config remote-ops args]
  (validate-options! "fetch" args {"-q" :flag "--quiet" :flag
                                   "--all" :flag "--prune" :flag
                                   "--tags" :flag "--no-tags" :flag})
  (when (some #{"--all"} args)
    (throw (ex-info "fetch --all is not implemented; name one remote" {})))
  (when (some #{"--prune"} args)
    (throw (ex-info "fetch --prune is not implemented" {})))
  (let [operands (vec (remove #(str/starts-with? % "-") args))]
    (when (> (count operands) 2)
      (throw (ex-info "fetch refspecs are not implemented yet" {:args args})))
    (let [{:keys [remote url]} (remote-context conn global-config operands)
          result ((require-remote-op remote-ops :fetch)
                  {:conn conn :remote remote :url url
                   :options {:refspec (second operands)
                             :prune? (boolean (some #{"--prune"} args))
                             :tags (cond (some #{"--tags"} args) :all
                                         (some #{"--no-tags"} args) :none)}})]
      (ok (if (some #{"-q" "--quiet"} args) ""
              (str "From " url "\n"
                   (or (:persisted result) 0) " objects imported\n"))))))

(defn- git-pull [conn global-config remote-ops args]
  (validate-options! "pull" args {"--ff-only" :flag
                                  "-q" :flag "--quiet" :flag})
  (let [operands (vec (remove #(str/starts-with? % "-") args))]
    (when (> (count operands) 1)
      (throw (ex-info "pull refspecs are not implemented yet" {:args args})))
    (let [{:keys [remote url]} (remote-context conn global-config operands)
          result ((require-remote-op remote-ops :pull)
                  {:conn conn :remote remote :url url
                   :options {:policy :ff-only}})]
      (ok (if (some #{"-q" "--quiet"} args) ""
              (str (name (:pull/status result)) "\n"))))))

(defn- git-push [conn global-config remote-ops args]
  (validate-options! "push" args {"-u" :flag "--set-upstream" :flag
                                  "-q" :flag "--quiet" :flag
                                  "-f" :flag "--force" :flag
                                  "--force-with-lease" :flag
                                  "--delete" :flag}
                     [#(boolean (re-matches #"-[qfu]+" %))])
  (when (some #{"--force-with-lease"} args)
    (throw (ex-info "push --force-with-lease is not implemented; refusing to weaken it to --force"
                    {})))
  (let [set-upstream? (or (boolean (some #{"-u" "--set-upstream"} args))
                          (short-option-char? args \u))
        operands (vec (remove #(str/starts-with? % "-") args))
        remote (or (first operands) "origin")
        refspec (second operands)
        delete? (boolean (some #{"--delete"} args))
        _ (when (and (not delete?) (> (count operands) 2))
            (throw (ex-info "too many arguments for push" {:args args})))
        _ (when (and delete? (nil? refspec))
            (throw (ex-info "--delete requires at least one branch" {})))
        {:keys [url]} (remote-context conn global-config [remote])
        current-ref (repo/current-ref conn)
        [source destination] (if refspec
                               (let [[source destination]
                                     (str/split refspec #":" 2)]
                                 [source (or destination source)])
                               [current-ref current-ref])
        commit (when-not delete? (revision/require conn source))
        destination (if (str/starts-with? destination "refs/")
                      destination (str "refs/heads/" destination))
        destinations (if delete?
                       (mapv #(if (str/starts-with? % "refs/") %
                                  (str "refs/heads/" %))
                             (rest operands))
                       [destination])
        results (mapv (fn [destination]
                        ((require-remote-op remote-ops :push)
                         {:conn conn :remote remote :url url
                          :commit-id (some-> commit :geschichte.commit/id)
                          :options {:ref destination
                                    :force? (or (boolean
                                                 (some #{"-f" "--force"} args))
                                                (short-option-char? args \f))}}))
                      destinations)]
    (doseq [result results
            :let [report (:report result)
                  rejected (some (fn [[ref status]]
                                   (when (= :rejected (:status status))
                                     [ref (:reason status)]))
                                 (:refs report))]]
      (when (and report (not= "ok" (:unpack report)))
        (throw (ex-info (str "remote unpack failed: " (:unpack report))
                        {:report report})))
      (when rejected
        (throw (ex-info (str "remote rejected " (first rejected) ": "
                             (second rejected)) {:report report}))))
    (when set-upstream?
      (let [branch (str/replace current-ref #"^refs/heads/" "")]
        (repo/set-config! conn (str "branch." branch ".remote") remote)
        (repo/set-config! conn (str "branch." branch ".merge") destination)))
    (ok (if (or (some #{"-q" "--quiet"} args)
                (short-option-char? args \q)) ""
            (str "To " url "\n"
                 (apply str (map #(str % "\n") destinations)))))))

(defn- git-ls-remote [conn global-config remote-ops args]
  (validate-options! "ls-remote" args
                     {"--heads" :flag "-h" :flag "--tags" :flag
                      "--refs" :flag "--symref" :flag "--exit-code" :flag})
  (let [operands (vec (remove #(str/starts-with? % "-") args))
        [repository & patterns] operands
        _ (when (str/blank? repository)
            (throw (ex-info "usage: git ls-remote [options] <repository> [<refs>...]"
                            {})))
        config (if conn (effective-config conn global-config) @global-config)
        url (or (get config (str "remote." repository ".url")) repository)
        prefixes (cond
                   (some #{"--heads" "-h"} args) ["refs/heads/"]
                   (some #{"--tags"} args) ["refs/tags/"]
                   :else ["HEAD" "refs/heads/" "refs/tags/"])
        refs ((require-remote-op remote-ops :ls-remote)
              {:url url :options {:prefixes prefixes}})
        refs (filter (fn [{:keys [ref]}]
                       (or (empty? patterns)
                           (some #(or (= % ref)
                                      (re-matches (pathspec-regex %) ref)
                                      (re-matches (pathspec-regex %)
                                                  (last (str/split ref #"/"))))
                                 patterns))) refs)
        output (apply str
                      (mapcat (fn [{:keys [oid ref attributes]}]
                                (concat
                                 (when (and (some #{"--symref"} args)
                                            (:symref-target attributes))
                                   [(str "ref: " (:symref-target attributes)
                                         "\t" ref "\n")])
                                 (when oid [(str oid "\t" ref "\n")])))
                              refs))]
    {:stdout output :stderr ""
     :exit (if (and (some #{"--exit-code"} args) (empty? refs)) 2 0)}))

(defn execute
  "Execute Git-compatible argv against a host-supplied repository context.

   Context keys:
   - `:conn` is the Geschichte Datahike connection.
   - `:root` is the displayed worktree root.
   - `:config` is an atom containing Git-compatible configuration.
   - `:repo-relative` maps a host path argument to a repository path or nil.

   The returned map always contains `:stdout`, `:stderr`, and `:exit`."
  [{:keys [conn root config repo-relative remote-ops workspace-ops read-message]} argv]
  (try
    (let [argv (vec (remove #{"--no-pager" "--paginate"} argv))
          unsupported (preflight argv)
          command (first argv)
          args (subvec argv (min 1 (count argv)))
          repo-relative (or repo-relative identity)]
      (or unsupported
          (case command
            "config" (git-config conn config args)
            "status" (git-status conn args)
            "add" (git-add conn repo-relative args)
            "commit" (git-commit conn config read-message args)
            "diff" (git-diff conn repo-relative args)
            "log" (git-log conn args)
            "show" (git-show conn args)
            "grep" (git-grep conn args)
            "cat-file" (git-cat-file conn args)
            "describe" (git-describe conn args)
            "symbolic-ref" (git-symbolic-ref conn args)
            "worktree" (git-worktree conn root workspace-ops args)
            "for-each-ref" (git-for-each-ref conn args)
            "show-ref" (git-show-ref conn args)
            "ls-files" (git-ls-files conn args)
            "ls-tree" (git-ls-tree conn args)
            "check-ignore" (git-check-ignore conn repo-relative args)
            "rev-list" (git-rev-list conn args)
            "branch" (git-branch conn args)
            "tag" (git-tag conn args)
            "merge" (git-merge conn config args)
            "merge-base" (git-merge-base conn args)
            "stash" (git-stash conn config repo-relative args)
            "cherry-pick" (git-cherry-pick conn config args)
            "rebase" (git-rebase conn config args)
            "checkout" (git-checkout conn repo-relative args)
            "switch" (git-checkout conn repo-relative args)
            "restore" (git-restore conn repo-relative args)
            "reset" (git-reset conn repo-relative args)
            "rm" (git-rm conn repo-relative args)
            "mv" (git-mv conn repo-relative args)
            "clean" (git-clean conn repo-relative args)
            "rev-parse" (git-rev-parse conn root repo-relative args)
            "remote" (git-remote conn config args)
            "fetch" (git-fetch conn config remote-ops args)
            "pull" (git-pull conn config remote-ops args)
            "push" (git-push conn config remote-ops args)
            "ls-remote" (git-ls-remote conn config remote-ops args)
            (fail (str "'" command "' is not a Geschichte command") 1))))
    (catch Throwable error
      (fail (or (ex-message error) (str error))
            (or (:exit (ex-data error))
                (when (= :usage (:kind (ex-data error))) 129)
                128)))))
