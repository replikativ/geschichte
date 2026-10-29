(ns geschichte.merge.delta
  "Strict bounded decoding of Geschichte's generated line-delta format.
  This is not a general-purpose EDN parser."
  (:require [clojure.string :as str]
            [geschichte.budget :as budget]
            [geschichte.diff :as diff]
            [geschichte.text-budget :as text-budget]
            #?(:clj [clojure.edn :as edn] :cljs [cljs.reader :as edn])))

(defn- invalid! [message]
  (throw (ex-info message {:type ::invalid-delta})))

(defn preflight!
  "Inspect every character before the EDN reader. Only generated maps, vectors,
  strings and bounded atomic tokens are admitted; dispatch forms cannot run."
  [operation text]
  (let [started? (volatile! false)]
    (loop [i 0 stack [] string? false escaped? false token 0]
      (if (= i (count text))
        (when (or string? (seq stack)) (invalid! "Unterminated line delta"))
        (let [c (nth text i)]
          (budget/charge! operation :text-work 1)
          (when (and (not string?) (empty? stack) (not= c \,) (not (str/blank? (str c))))
            (when (or @started? (not= c \{)) (invalid! "Expected one generated delta map"))
            (vreset! started? true))
          (cond
            string?
            (cond escaped? (recur (inc i) stack true false 0)
                  (= c \\) (recur (inc i) stack true true 0)
                  (= c \") (recur (inc i) stack false false 0)
                  :else (recur (inc i) stack true false 0))

            (= c \")
            (do (budget/charge! operation :delta-forms 1)
                (recur (inc i) stack true false 0))

            (#{\[ \{} c)
            (do (budget/bound! operation :delta-edn-depth (inc (count stack)))
                (budget/charge! operation :delta-forms 1)
                (recur (inc i) (conj stack (if (= c \[) \] \})) false false 0))

            (#{\] \}} c)
            (do (when-not (= c (peek stack)) (invalid! "Mismatched line delta delimiters"))
                (recur (inc i) (pop stack) false false 0))

            (or (= c \,) (str/blank? (str c)))
            (recur (inc i) stack false false 0)

            (#{\# \; \( \) \\} c)
            (invalid! "Unsupported form in generated line delta")

            :else
            (do (when (zero? token) (budget/charge! operation :delta-forms 1))
                (budget/bound! operation :delta-token-chars (inc token))
                (recur (inc i) stack false false (inc token)))))))))

(defn- natural? [n]
  (and (integer? n) (<= 0 n #?(:clj Long/MAX_VALUE :cljs js/Number.MAX_SAFE_INTEGER))))

(defn- closed-keys? [value expected]
  (and (map? value) (= expected (set (keys value)))))

(defn decode
  "Read a checked generated delta and apply it to text. All expanded line/byte
  accounting happens before retaining output or allocating its joined string."
  [operation base stored]
  (preflight! operation stored)
  (let [delta (edn/read-string stored)]
    (when-not (and (closed-keys? delta #{:edits :final-newline?})
                   (boolean? (:final-newline? delta)) (vector? (:edits delta)))
      (invalid! "Invalid generated line delta root"))
    (text-budget/lines! operation base)
    (let [base-lines (:lines (diff/text-lines base))
          result
          (loop [edits (seq (:edits delta)) a 0 b 0 out (transient []) size 0]
            (if-let [{:keys [op a-start a-count b-start b-count lines] :as edit} (first edits)]
              (do
                (budget/charge! operation :text-work 1)
                (when-not (and (closed-keys? edit (cond-> #{:op :a-start :a-count :b-start :b-count}
                                                    (= op :insert) (conj :lines)))
                               (every? natural? [a-start a-count b-start b-count])
                               (= a a-start) (= b b-start)
                               (<= a-count (- (count base-lines) a)))
                  (invalid! "Invalid generated line delta coordinates"))
                (let [selected
                      (case op
                        :equal (do (when-not (= a-count b-count) (invalid! "Unequal equal range"))
                                   (subvec base-lines a (+ a a-count)))
                        :delete (do (when-not (zero? b-count) (invalid! "Nonempty deletion target")) [])
                        :insert (do (when-not (and (zero? a-count) (vector? lines)
                                                   (= (count lines) b-count)
                                                   (every? string? lines))
                                      (invalid! "Invalid inserted lines")) lines)
                        (invalid! "Invalid generated line delta operation"))
                      [out size]
                      (reduce (fn [[out size] line]
                                (budget/charge! operation :text-lines 1)
                                (let [n (text-budget/utf8-size operation line)
                                      added (+ n (if (zero? (count out)) 0 1))]
                                  ;; Embedded LF would change line coordinates.
                                  (when (str/includes? line "\n") (invalid! "Inserted line contains LF"))
                                  (budget/bound! operation :object-bytes (+ size added))
                                  [(conj! out line) (+ size added)]))
                              [out size] selected)]
                  (recur (next edits) (+ a a-count) (+ b b-count) out size)))
              (do (when-not (= a (count base-lines)) (invalid! "Incomplete generated line delta"))
                  (let [size (+ size (if (:final-newline? delta) 1 0))]
                    (budget/bound! operation :object-bytes size)
                    (budget/charge! operation :content-decoded-bytes size)
                    (persistent! out)))))]
      (str (str/join "\n" result) (when (:final-newline? delta) "\n")))))
