(ns geschichte.merge.text
  "Three-way LINE merge — the content half of a merge.

   `merge.core/plan-trees` resolves a path by comparing tree ENTRIES: same on
   both sides, unchanged on one side, or a conflict. Those three cases are also
   git's, and git then does one more thing — in the conflict case it merges the
   two blobs LINE by line and only reports a conflict if that fails. This is
   that step.

   Without it, every path both sides touched conflicts, whether or not the
   changes overlap. That is correct but coarse enough to matter: two people (or
   two agents) working different parts of one file collide on every merge, and
   each collision costs whoever has to arbitrate it.

   Pure and total: lines in, lines or conflict hunks out. No connection, no IO,
   no store. That is what keeps it here in the portable planner rather than in
   `repo` — and it is also what makes it testable, which matters because every
   failure mode in a merge is quiet. An off-by-one does not throw; it drops a
   line.

   The algorithm is diff3. Diff base→ours and base→theirs, express each as
   replacements over BASE ranges, then sweep both in base order: a range only
   one side touched is taken from that side, a range both touched is a conflict
   unless they made the same change, everything else is base.

   It does NOT resolve conflicts. Overlapping edits come back as the three
   texts, for a caller to decide. Auto-resolving them is how a merge tool loses
   work."
  (:require [clojure.string :as str]
            [geschichte.diff :as gdiff]
            [geschichte.budget :as budget]
            [geschichte.text-budget :as text-budget]))

(defn- equal-lines? [operation ours theirs]
  (when operation
    (doseq [line (concat ours theirs)]
      (budget/charge! operation :text-work (count line))))
  (= ours theirs))

(defn- append-lines [operation out lines]
  (budget/charge! operation :text-work (count lines))
  (budget/charge! operation :text-lines (count lines))
  (into out lines))

(defn- replacements
  "A diff, as replacements over BASE ranges: `[{:start :end :lines} …]`, half
   open, sorted, non-overlapping.

   Adjacent delete+insert become ONE replacement rather than two edits. Kept
   separate they read as \"deleted these lines\" and \"added those\", and two
   sides that both rewrote one line would then look like four independent
   changes, two of which happen to abut — which merges cleanly and produces
   nonsense."
  [{:keys [a-lines b-lines edits]} operation]
  (let [raw (keep (fn [{:keys [op a-start a-count b-start b-count]}]
                    (budget/charge! operation :text-work 1)
                    (case op
                      :equal nil
                      :delete {:start a-start :end (+ a-start a-count) :lines []}
                      :insert {:start a-start :end a-start
                               :lines (subvec (vec b-lines) b-start (+ b-start b-count))}))
                  edits)]
    (->> raw
         (sort (fn [a b] (budget/charge! operation :text-work 1)
                 (compare [(:start a) (:end a)] [(:start b) (:end b)])))
         (reduce (fn [acc {:keys [start end lines] :as r}]
                   (budget/charge! operation :text-work 1)
                   (if-let [prev (peek acc)]
                     (if (<= start (:end prev))
                       ;; touching or overlapping — one replacement
                       (conj (pop acc)
                             {:start (:start prev)
                              :end (max (:end prev) end)
                              :lines (append-lines operation (:lines prev) lines)})
                       (conj acc r))
                     (conj acc r)))
                 [])
         vec)))

(defn- overlaps?
  "Do two base ranges touch? Adjacency counts: an insert at line N and a
   replacement ending at N describe the same seam, and treating them as
   independent interleaves the two sides' text."
  [a b]
  (and (<= (:start a) (:end b)) (<= (:start b) (:end a))))

(defn- overlap-cluster [ours theirs operation]
  (let [start (min (:start (first ours)) (:start (first theirs)))]
    (loop [end (max (:end (first ours)) (:end (first theirs)))
           o ours t theirs os [] ts []]
      (budget/charge! operation :text-work 1)
      (cond
        (and (seq o) (<= (:start (first o)) end))
        (recur (max end (:end (first o))) (rest o) t (conj os (first o)) ts)

        (and (seq t) (<= (:start (first t)) end))
        (recur (max end (:end (first t))) o (rest t) os (conj ts (first t)))

        :else {:start start :end end :ours os :theirs ts :o o :t t}))))

(defn- render-cluster [base start end replacements operation]
  (loop [pos start edits (seq replacements) out []]
    (budget/charge! operation :text-work 1)
    (if-let [{:keys [start end lines]} (first edits)]
      (recur end (next edits) (append-lines operation (append-lines operation out (subvec base pos start)) lines))
      (append-lines operation out (subvec base pos end)))))

(defn merge-lines
  "Three-way merge of line vectors. Returns

     {:clean? true  :lines [...]}
     {:clean? false :lines [...] :conflicts [{:base :ours :theirs} …]}

   The line vector is returned either way — on a conflict it carries the
   ours-side text at the conflicting regions, so a caller that wants to show
   something still can, and `:conflicts` says exactly what was elided."
  ([base ours theirs] (merge-lines base ours theirs nil))
  ([base ours theirs {:keys [merge-budget]}]
   (let [operation merge-budget
         _ (budget/charge! operation :text-work (+ (count base) (count ours) (count theirs)))
         base (vec base) ours (vec ours) theirs (vec theirs)
         o (replacements (gdiff/diff-lines base ours {:merge-budget operation}) operation)
         t (replacements (gdiff/diff-lines base theirs {:merge-budget operation}) operation)]
     (loop [pos 0, o o, t t, out [], conflicts []]
       (budget/charge! operation :text-work 1)
       (let [oh (first o) th (first t)]
         (cond
          ;; nothing left to apply — the rest of base survives
           (and (nil? oh) (nil? th))
           (cond-> {:clean? (empty? conflicts)
                    :lines (append-lines operation out (subvec base (min pos (count base))))}
             (seq conflicts) (assoc :conflicts conflicts))

          ;; both sides touch the same region
           (and oh th (overlaps? oh th))
           (let [{:keys [start end ours theirs o t]} (overlap-cluster o t operation)
                 ours (render-cluster base start end ours operation)
                 theirs (render-cluster base start end theirs operation)
                 out (append-lines operation (append-lines operation out (subvec base pos start)) ours)]
             (recur end o t out
                    (if (equal-lines? operation ours theirs)
                      conflicts
                      (conj conflicts {:base (subvec base start end)
                                       :ours ours :theirs theirs}))))

          ;; only ours, or ours comes first
           (and oh (or (nil? th) (< (:start oh) (:start th))))
           (recur (:end oh) (rest o) t
                  (append-lines operation (append-lines operation out (subvec base pos (:start oh))) (:lines oh))
                  conflicts)

           :else
           (recur (:end th) o (rest t)
                  (append-lines operation (append-lines operation out (subvec base pos (:start th))) (:lines th))
                  conflicts)))))))

(defn merge-text
  "`merge-lines` over strings. Returns the same map with `:text` instead of
   `:lines`.

   Splits and rejoins on `\\n` rather than using `text-lines`' final-newline
   metadata: a merge must reproduce the file's bytes, and the round trip
   `split → join` is exact for that as long as both halves agree."
  ([base ours theirs] (merge-text base ours theirs nil))
  ([base ours theirs {:keys [merge-budget] :as opts}]
   (let [split #(do (text-budget/lines! merge-budget %)
                    (if (empty? %) [] (str/split (or % "") #"\n" -1)))
         r (merge-lines (split base) (split ours) (split theirs) opts)
         output-size (when merge-budget
                       (reduce (fn [n line] (+ n (text-budget/utf8-size merge-budget line)))
                               (max 0 (dec (count (:lines r)))) (:lines r)))
         _ (budget/bound! merge-budget :object-bytes output-size)
         _ (budget/charge! merge-budget :merge-output-bytes output-size)]
     (-> r
         (assoc :text (str/join "\n" (:lines r)))
         (dissoc :lines)))))
