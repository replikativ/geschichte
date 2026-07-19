(ns geschichte.ignore
  "Portable Gitignore interpretation for Geschichte worktrees."
  (:refer-clojure :exclude [await])
  (:require [clojure.string :as str]
            [geschichte.bytes :as bytes]
            [geschichte.repo :as repo]
            [is.simm.partial-cps.async :refer [await]])
  #?(:cljs (:require-macros [geschichte.macros :refer [platform-async]]))
  #?(:clj (:require [geschichte.macros :refer [platform-async]])))

(defn- parent [path]
  (let [i (.lastIndexOf ^String path "/")]
    (if (neg? i) "" (subs path 0 i))))

(defn- unescaped-trailing-spaces [line]
  (loop [line line]
    (cond
      (not (str/ends-with? line " ")) line
      (str/ends-with? line "\\ ") (str (subs line 0 (- (count line) 2)) " ")
      :else (recur (subs line 0 (dec (count line)))))))

(defn- regex-quote [character]
  (if (contains? #{\. \+ \( \) \^ \$ \| \{ \} \[ \] \\} character)
    (str "\\" character)
    (str character)))

(defn- glob-regex [glob]
  (loop [characters (seq glob), result ""]
    (if-let [character (first characters)]
      (cond
        (= character \*)
        (if (= \* (second characters))
          (if (= \/ (nth characters 2 nil))
            (recur (drop 3 characters) (str result "(?:.*/)?"))
            (recur (drop 2 characters) (str result ".*")))
          (recur (next characters) (str result "[^/]*")))

        (= character \?)
        (recur (next characters) (str result "[^/]"))

        (= character \\)
        (if-let [quoted (second characters)]
          (recur (nnext characters) (str result (regex-quote quoted)))
          (recur (next characters) (str result "\\\\")))

        :else
        (recur (next characters) (str result (regex-quote character))))
      result)))

(defn- parse-line [base order raw-line]
  (let [line (-> raw-line (str/replace #"\r$" "") unescaped-trailing-spaces)]
    (when-not (or (str/blank? line)
                  (and (str/starts-with? line "#")
                       (not (str/starts-with? line "\\#"))))
      (let [escaped-prefix? (or (str/starts-with? line "\\#")
                                (str/starts-with? line "\\!"))
            negated? (and (not escaped-prefix?) (str/starts-with? line "!"))
            pattern (cond-> line
                      negated? (subs 1)
                      escaped-prefix? (subs 1))
            directory? (str/ends-with? pattern "/")
            pattern (str/replace pattern #"/+$" "")
            anchored? (str/starts-with? pattern "/")
            pattern (str/replace pattern #"^/+" "")
            slash? (str/includes? pattern "/")
            prefix (if (str/blank? base) "" (str (glob-regex base) "/"))
            body (glob-regex pattern)
            expression (if (or anchored? slash?)
                         (str "^" prefix body "(?:/.*)?$")
                         (str "^" prefix "(?:.*/)?" body "(?:/.*)?$"))]
        {:base base
         :order order
         :pattern raw-line
         :negated? negated?
         :directory? directory?
         :regex (re-pattern expression)}))))

(defn rules
  "Load ordered rules from every worktree `.gitignore`. Returns directly on
   JVM and a partial-cps computation on ClojureScript."
  [conn]
  (platform-async
   (loop [paths (->> (repo/files conn)
                     (filter #(or (= % ".gitignore")
                                  (str/ends-with? % "/.gitignore")))
                     (sort-by (juxt #(count (str/split % #"/")) identity))
                     seq)
          result []]
     (if-let [path (first paths)]
       (let [base (parent path)
             content (bytes/decode-utf8 (await (repo/read conn path)))
             parsed (keep-indexed #(parse-line base %1 %2)
                                  (str/split content #"\n" -1))]
         (recur (next paths) (into result parsed)))
       result))))

(defn ignored?
  "True when the final matching rule excludes repository-relative `path`."
  [rules path]
  (reduce (fn [ignored {:keys [regex negated?]}]
            (if (re-matches regex path) (not negated?) ignored))
          false rules))

(defn filter-visible [rules paths]
  (remove #(ignored? rules %) paths))
