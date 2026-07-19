(ns geschichte.diff
  "Structured, filesystem-independent text diffing.

   This namespace deliberately knows nothing about muschel's shell, hosts, or
   filesystem protocol.  It is the experimental extraction boundary for a
   reusable CLJ/CLJS diff library.

   The public representation is a sequence of coalesced edits with half-open
   coordinates in both inputs.  Rendering is a separate concern, so comparison
   normalisation (ignore-case/whitespace, later gitattributes-style drivers)
   never destroys the original text shown to a user.")

(def ^:private default-max-trace-cells
  ;; Myers' ordinary case is excellent, but retaining every frontier for
  ;; backtracking becomes quadratic for unrelated inputs.  Keep the experiment
  ;; predictably bounded until the linear-space bisect kernel lands.
  8000000)

(defn text-lines
  "Split `text` into Git-shaped lines without losing whether the final line has
   a newline.  Newline bytes are excluded from `:lines`; CR in CRLF is retained
   so callers can choose whether it participates in comparison.

   Returns {:lines [...] :final-newline? boolean}."
  [text]
  (let [text (or text "")
        n (count text)]
    #?(:clj
       ;; String.split performs the same scan and substring materialisation in
       ;; optimized JDK code.  Keeping the trailing empty field lets us retain
       ;; Git's final-newline distinction; it is metadata rather than a line.
       (if (zero? n)
         {:lines [] :final-newline? false}
         (let [final-newline? (= \newline (.charAt ^String text (dec n)))
               fields (vec (.split ^String text "\\n" -1))]
           {:lines (if final-newline? (pop fields) fields)
            :final-newline? final-newline?}))
       :cljs
       (loop [start 0
              lines (transient [])]
         (let [end (.indexOf text "\n" start)]
           (if (neg? end)
             {:lines (persistent!
                      (if (< start n)
                        (conj! lines (subs text start n))
                        lines))
              :final-newline? (and (pos? n) (= start n))}
             (recur (inc end) (conj! lines (subs text start end)))))))))

(defn- int-buffer [n]
  #?(:clj (int-array n)
     :cljs (js/Int32Array. n)))

(defn- buffer-copy [buf]
  #?(:clj (aclone ^ints buf)
     :cljs (.slice buf)))

(defn- bget [buf i]
  #?(:clj (aget ^ints buf i)
     :cljs (aget buf i)))

(defn- bset! [buf i value]
  #?(:clj (aset-int ^ints buf i (int value))
     :cljs (aset buf i value))
  buf)

(defn- prepend-prefix-suffix [middle prefix suffix]
  (cond-> []
    (pos? prefix)
    (conj {:op :equal
           :a-start 0 :a-count prefix
           :b-start 0 :b-count prefix})

    true (into middle)

    (pos? suffix)
    (conj {:op :equal
           :a-start nil :a-count suffix
           :b-start nil :b-count suffix})))

(defn- coalesce-steps
  "Turn source-order unit steps into coordinate ranges."
  [steps a-offset b-offset]
  (loop [remaining steps
         a-pos a-offset
         b-pos b-offset
         edits []]
    (if-let [op (first remaining)]
      (let [a-step (if (#{:equal :delete} op) 1 0)
            b-step (if (#{:equal :insert} op) 1 0)
            prior (peek edits)]
        (if (= op (:op prior))
          (recur (next remaining)
                 (+ a-pos a-step)
                 (+ b-pos b-step)
                 (conj (pop edits)
                       (-> prior
                           (update :a-count + a-step)
                           (update :b-count + b-step))))
          (recur (next remaining)
                 (+ a-pos a-step)
                 (+ b-pos b-step)
                 (conj edits {:op op
                              :a-start a-pos :a-count a-step
                              :b-start b-pos :b-count b-step}))))
      edits)))

(defn- fallback-steps [n m]
  ;; A bounded, valid (though not necessarily minimal) edit script. Git's
  ;; default differ likewise permits heuristics rather than promising minimality.
  (into (vec (repeat n :delete)) (repeat m :insert)))

(defn- myers-steps
  "Myers frontier search over `a` and `b`, returning unit steps in source order.
   Retains frontiers for backtracking and falls back to replace-all once the
   configured trace budget would be exceeded."
  [a b max-trace-cells]
  (let [a-data #?(:clj (object-array a) :cljs a)
        b-data #?(:clj (object-array b) :cljs b)
        n (count a)
        m (count b)
        max-d (+ n m)
        ;; One sentinel diagonal on either side avoids special storage cases at
        ;; d=0 and at the outermost frontier.
        offset (inc max-d)
        width (+ 3 (* 2 max-d))
        v (int-buffer width)]
    (loop [d 0
           trace []]
      (if (or (> (* (inc d) width) max-trace-cells)
              (> d max-d))
        (fallback-steps n m)
        (let [done
              (loop [k (- d)]
                (when (<= k d)
                  (let [idx (+ offset k)
                        down? (or (= k (- d))
                                  (and (not= k d)
                                       (< (bget v (dec idx))
                                          (bget v (inc idx)))))
                        ;; Choose the furthest-reaching predecessor. The
                        ;; comparison direction is intentionally written in
                        ;; terms of left/right frontier values to match Myers.
                        x0 (if down?
                             (bget v (inc idx))
                             (inc (bget v (dec idx))))
                        y0 (- x0 k)
                        [x y] (loop [x x0 y y0]
                                (if (and (< x n) (< y m)
                                         (= #?(:clj (aget ^objects a-data x)
                                               :cljs (nth a-data x))
                                            #?(:clj (aget ^objects b-data y)
                                               :cljs (nth b-data y))))
                                  (recur (inc x) (inc y))
                                  [x y]))]
                    (bset! v idx x)
                    (if (and (>= x n) (>= y m))
                      true
                      (recur (+ k 2))))))]
          (if-not done
            (recur (inc d) (conj trace (buffer-copy v)))
            (let [trace' (conj trace (buffer-copy v))]
              (loop [depth d
                     x n
                     y m
                     reversed (transient [])]
                (if (zero? depth)
                  (let [reversed
                        (loop [x x y y acc reversed]
                          (if (and (pos? x) (pos? y))
                            (recur (dec x) (dec y) (conj! acc :equal))
                            acc))]
                    (vec (reverse (persistent! reversed))))
                  (let [previous (nth trace' (dec depth))
                        k (- x y)
                        idx (+ offset k)
                        prev-k (if (or (= k (- depth))
                                       (and (not= k depth)
                                            (< (bget previous (dec idx))
                                               (bget previous (inc idx)))))
                                 (inc k)
                                 (dec k))
                        prev-x (bget previous (+ offset prev-k))
                        prev-y (- prev-x prev-k)
                        [x' _ reversed']
                        (loop [sx x sy y acc reversed]
                          (if (and (> sx prev-x) (> sy prev-y))
                            (recur (dec sx) (dec sy) (conj! acc :equal))
                            [sx sy acc]))
                        insertion? (= x' prev-x)]
                    (recur (dec depth)
                           (long prev-x)
                           (long prev-y)
                           (conj! reversed'
                                  (if insertion? :insert :delete)))))))))))))

(defn diff-lines
  "Diff vectors `a-lines` and `b-lines`.

   Options:
   - `:normalize` comparison-only function applied to every line.
   - `:max-trace-cells` memory/work guard for the current Myers backtracker.

   Returns coalesced half-open edit ranges. Equal ranges can contain different
   original strings when `:normalize` considers them equal."
  ([a-lines b-lines] (diff-lines a-lines b-lines nil))
  ([a-lines b-lines {:keys [normalize max-trace-cells a-keys b-keys]
                     :or {normalize identity
                          max-trace-cells default-max-trace-cells}}]
   (let [a-lines (vec a-lines)
         b-lines (vec b-lines)
         a (or a-keys (mapv normalize a-lines))
         b (or b-keys (mapv normalize b-lines))
         n (count a)
         m (count b)
         prefix (loop [i 0]
                  (if (and (< i n) (< i m) (= (nth a i) (nth b i)))
                    (recur (inc i))
                    i))
         suffix (loop [i 0]
                  (if (and (< i (- n prefix))
                           (< i (- m prefix))
                           (= (nth a (- n i 1)) (nth b (- m i 1))))
                    (recur (inc i))
                    i))
         a-end (- n suffix)
         b-end (- m suffix)
         middle-a (subvec a prefix a-end)
         middle-b (subvec b prefix b-end)
         steps (myers-steps middle-a middle-b max-trace-cells)
         middle (coalesce-steps steps prefix prefix)
         edits (prepend-prefix-suffix middle prefix suffix)
         edits (if (pos? suffix)
                 (assoc-in edits [(dec (count edits)) :a-start] a-end)
                 edits)
         edits (if (pos? suffix)
                 (assoc-in edits [(dec (count edits)) :b-start] b-end)
                 edits)]
     {:algorithm :myers
      :a-lines a-lines
      :b-lines b-lines
      :edits edits})))

(defn diff-text
  "Structured text diff preserving original lines and final-newline metadata."
  ([a-text b-text] (diff-text a-text b-text nil))
  ([a-text b-text opts]
   (let [a (text-lines a-text)
         b (text-lines b-text)
         normalize (or (:normalize opts) identity)
         comparison-keys
         (fn [{:keys [lines final-newline?]}]
           (let [keys (mapv normalize lines)]
             (if (seq keys)
               ;; A final newline is part of the file's bytes. Decorate only
               ;; the final comparison key so `"a"` and `"a\n"` become a
               ;; one-line replacement while their rendered text stays `a`.
               (assoc keys (dec (count keys))
                      [::final-line (peek keys) final-newline?])
               keys)))
         result (diff-lines (:lines a) (:lines b)
                            (assoc (or opts {})
                                   :a-keys (comparison-keys a)
                                   :b-keys (comparison-keys b)))]
     (assoc result
            :a-final-newline? (:final-newline? a)
            :b-final-newline? (:final-newline? b)))))

(defn operations
  "Expand a structured diff into source-order `[:keep|:del|:add line]` pairs.
   This compatibility view is convenient for unified rendering and the existing
   Muschel builtin; consumers doing analysis should prefer `:edits`."
  [{:keys [a-lines b-lines edits]}]
  (persistent!
   (reduce
    (fn [out {:keys [op a-start a-count b-start b-count]}]
      (case op
        :equal (reduce (fn [acc i] (conj! acc [:keep (nth a-lines i)]))
                       out (range a-start (+ a-start a-count)))
        :delete (reduce (fn [acc i] (conj! acc [:del (nth a-lines i)]))
                        out (range a-start (+ a-start a-count)))
        :insert (reduce (fn [acc i] (conj! acc [:add (nth b-lines i)]))
                        out (range b-start (+ b-start b-count)))))
    (transient [])
    edits)))

(defn- group-hunks
  [ops context]
  (let [changed? (fn [[op _]] (not= op :keep))
        consumes-a? (fn [[op _]] (or (= op :keep) (= op :del)))
        consumes-b? (fn [[op _]] (or (= op :keep) (= op :add)))
        n (count ops)
        prefix-counts
        (fn [predicate]
          (let [counts (int-buffer (inc n))]
            (loop [index 0 total 0]
              (if (= index n)
                counts
                (let [total' (+ total (if (predicate (nth ops index)) 1 0))]
                  (bset! counts (inc index) total')
                  (recur (inc index) total'))))))
        a-prefix (prefix-counts consumes-a?)
        b-prefix (prefix-counts consumes-b?)
        regions
        (loop [i 0 acc (transient [])]
          (cond
            (>= i n) (persistent! acc)
            (changed? (nth ops i))
            (let [start (loop [s i k 0]
                          (cond
                            (zero? s) 0
                            (changed? (nth ops (dec s))) (recur (dec s) 0)
                            (>= k context) s
                            :else (recur (dec s) (inc k))))
                  end (loop [e (inc i) k 0]
                        (cond
                          (>= e n) e
                          (changed? (nth ops e)) (recur (inc e) 0)
                          (>= k context) e
                          :else (recur (inc e) (inc k))))]
              (recur (long end) (conj! acc [start end])))
            :else (recur (inc i) acc)))
        merged (reduce (fn [acc [start end]]
                         (if (and (seq acc) (<= start (second (peek acc))))
                           (conj (pop acc) [(first (peek acc))
                                            (max end (second (peek acc)))])
                           (conj acc [start end])))
                       [] regions)]
    (mapv (fn [[start end]]
            (let [lines (subvec ops start end)
                  a-offset (bget a-prefix start)
                  b-offset (bget b-prefix start)
                  a-count (- (bget a-prefix end) a-offset)
                  b-count (- (bget b-prefix end) b-offset)]
              {:a-offset a-offset
               :b-offset b-offset
               :a-start (if (zero? a-count) a-offset (inc a-offset))
               :a-count a-count
               :b-start (if (zero? b-count) b-offset (inc b-offset))
               :b-count b-count
               :lines lines}))
          merged)))

(defn unified
  "Render a structured diff as a unified patch.

   Options: `:a-name`, `:b-name`, and `:context` (default 3). The renderer is
   portable and preserves missing-final-newline markers."
  ([result] (unified result nil))
  ([{:keys [a-lines b-lines a-final-newline? b-final-newline?] :as result}
    {:keys [a-name b-name context]
     :or {a-name "a" b-name "b" context 3}}]
   (let [ops (operations result)
         hunks (group-hunks ops context)
         a-total (count a-lines)
         b-total (count b-lines)]
     (if (empty? hunks)
       ""
       (apply str
              (str "--- " a-name "\n+++ " b-name "\n")
              (for [{:keys [a-offset b-offset a-start a-count
                            b-start b-count lines]} hunks]
                (let [body
                      (loop [remaining lines
                             a-index a-offset
                             b-index b-offset
                             pieces (transient [])]
                        (if-let [[op line] (first remaining)]
                          (let [missing-a? (and (#{:keep :del} op)
                                                (= a-index (dec a-total))
                                                (not a-final-newline?))
                                missing-b? (and (#{:keep :add} op)
                                                (= b-index (dec b-total))
                                                (not b-final-newline?))
                                marker? (or missing-a? missing-b?)
                                prefix (case op :keep " " :del "-" :add "+")]
                            (recur (next remaining)
                                   (+ a-index (if (#{:keep :del} op) 1 0))
                                   (+ b-index (if (#{:keep :add} op) 1 0))
                                   (cond-> (conj! pieces (str prefix line "\n"))
                                     marker? (conj! "\\ No newline at end of file\n"))))
                          (persistent! pieces)))]
                  (let [range-text (fn [start count]
                                     (if (= 1 count)
                                       (str start)
                                       (str start "," count)))]
                    (apply str
                           (str "@@ -" (range-text a-start a-count)
                                " +" (range-text b-start b-count) " @@\n")
                           body)))))))))
