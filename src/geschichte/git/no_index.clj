(ns geschichte.git.no-index
  "Repository-independent `git diff --no-index` host adapter."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [geschichte.diff :as diff])
  (:import [java.io File]
           [java.nio ByteBuffer]
           [java.nio.charset CharacterCodingException CodingErrorAction
            StandardCharsets]
           [java.security MessageDigest]))

(set! *warn-on-reflection* true)

(defn- fail [message]
  (throw (ex-info message {:exit 129 :kind :usage})))

(defn- parse-args [args]
  (loop [args args, options {:context 3}, operands []]
    (if-let [arg (first args)]
      (cond
        (= arg "--") (recur (next args) options (into operands (rest args)))
        (= arg "--no-index") (recur (next args) options operands)
        (= arg "--quiet")
        (recur (next args) (assoc options :quiet? true) operands)
        (= arg "--exit-code") (recur (next args) options operands)
        (contains? #{"-U" "--unified"} arg)
        (if-let [value (second args)]
          (recur (nnext args) (assoc options :context (parse-long value)) operands)
          (fail (str "option requires an argument: " arg)))
        (re-matches #"-U\d+" arg)
        (recur (next args) (assoc options :context (parse-long (subs arg 2))) operands)
        (str/starts-with? arg "--unified=")
        (recur (next args)
               (assoc options :context
                      (parse-long (subs arg (count "--unified=")))) operands)
        (str/starts-with? arg "-")
        (fail (str "unsupported no-index option: " arg))
        :else (recur (next args) options (conj operands arg)))
      (do
        (when-not (= 2 (count operands))
          (fail "usage: ges diff --no-index [options] PATH PATH"))
        (assoc options :paths operands)))))

(defn- resolve-file [cwd path]
  (let [^File file (io/file path)]
    (.getCanonicalFile (if (.isAbsolute file) file (io/file cwd path)))))

(defn- display-path [path]
  (-> path (str/replace #"^\./" "") (str/replace #"^/+" "")))

(defn- decode-text [^bytes value]
  (try
    (let [decoder (doto (.newDecoder StandardCharsets/UTF_8)
                    (.onMalformedInput CodingErrorAction/REPORT)
                    (.onUnmappableCharacter CodingErrorAction/REPORT))
          text (str (.decode decoder (ByteBuffer/wrap value)))]
      (when-not (str/includes? text "\u0000") text))
    (catch CharacterCodingException _ nil)))

(defn- blob-oid [^bytes value]
  (let [^MessageDigest digest (MessageDigest/getInstance "SHA-1")
        ^bytes header (.getBytes (str "blob " (alength value) "\u0000") "UTF-8")]
    (.update digest header 0 (alength header))
    (.update digest value 0 (alength value))
    (apply str (map #(format "%02x" (bit-and 0xff %)) (.digest digest)))))

(defn- file-mode [^File file]
  (when file (if (.canExecute file) "100755" "100644")))

(defn- file-tree [^File root]
  (if (.isFile root)
    {"" root}
    (into (sorted-map)
          (for [^File file (file-seq root) :when (.isFile file)]
            [(-> (.toPath root) (.relativize (.toPath file)) str
                 (str/replace #"\\" "/"))
             file]))))

(defn- render-pair [left right left-label right-label context]
  (let [left-bytes (if left
                     (java.nio.file.Files/readAllBytes (.toPath ^File left))
                     (byte-array 0))
        right-bytes (if right
                      (java.nio.file.Files/readAllBytes (.toPath ^File right))
                      (byte-array 0))
        left-mode (file-mode left)
        right-mode (file-mode right)
        content-changed? (not (java.util.Arrays/equals left-bytes right-bytes))
        mode-changed? (and left right (not= left-mode right-mode))]
    (when (or content-changed? mode-changed?)
      (let [left-text (decode-text left-bytes)
            right-text (decode-text right-bytes)
            left-oid (if left (subs (blob-oid left-bytes) 0 7) "0000000")
            right-oid (if right (subs (blob-oid right-bytes) 0 7) "0000000")
            header-left-label (if left left-label right-label)
            header-right-label (if right right-label left-label)
            header (str "diff --git a/" header-left-label
                        " b/" header-right-label "\n"
                        (when-not left (str "new file mode " right-mode "\n"))
                        (when-not right (str "deleted file mode " left-mode "\n"))
                        (when mode-changed?
                          (str "old mode " left-mode "\n"
                               "new mode " right-mode "\n"))
                        (when content-changed?
                          (str "index " left-oid ".." right-oid
                               (when (and left right (not mode-changed?))
                                 (str " " left-mode))
                               "\n")))
            a-name (if left (str "a/" left-label) "/dev/null")
            b-name (if right (str "b/" right-label) "/dev/null")]
        (if-not content-changed?
          header
          (if (and left-text right-text)
            (str header
                 (diff/unified (diff/diff-text left-text right-text)
                               {:a-name a-name :b-name b-name :context context}))
            (str header "Binary files " a-name " and " b-name " differ\n")))))))

(defn run [cwd args]
  (let [{:keys [paths context quiet?]} (parse-args args)
        [left-name right-name] paths
        ^File left (resolve-file cwd left-name)
        ^File right (resolve-file cwd right-name)]
    (when (or (not (.exists left)) (not (.exists right)))
      (fail "no-index paths must exist"))
    (when-not (= (.isDirectory left) (.isDirectory right))
      (fail "no-index file-to-directory comparison is not implemented"))
    (let [left-tree (file-tree left)
          right-tree (file-tree right)
          paths (sort (set (concat (keys left-tree) (keys right-tree))))
          directory? (.isDirectory left)
          output
          (apply str
                 (keep (fn [path]
                         (let [left-label (display-path
                                           (if directory?
                                             (str left-name "/" path) left-name))
                               right-label (display-path
                                            (if directory?
                                              (str right-name "/" path) right-name))]
                           (render-pair (get left-tree path) (get right-tree path)
                                        left-label right-label context)))
                       paths))
          changed? (not (str/blank? output))]
      {:stdout (if (or quiet? (not changed?)) "" output)
       :stderr "" :exit (if changed? 1 0)})))
