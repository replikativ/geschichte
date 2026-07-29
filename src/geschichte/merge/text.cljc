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
            [geschichte.diff :as gdiff]))

(defn- replacements
  "A diff, as replacements over BASE ranges: `[{:start :end :lines} …]`, half
   open, sorted, non-overlapping.

   Adjacent delete+insert become ONE replacement rather than two edits. Kept
   separate they read as \"deleted these lines\" and \"added those\", and two
   sides that both rewrote one line would then look like four independent
   changes, two of which happen to abut — which merges cleanly and produces
   nonsense."
  [{:keys [a-lines b-lines edits]}]
  (let [raw (keep (fn [{:keys [op a-start a-count b-start b-count]}]
                    (case op
                      :equal nil
                      :delete {:start a-start :end (+ a-start a-count) :lines []}
                      :insert {:start a-start :end a-start
                               :lines (subvec (vec b-lines) b-start (+ b-start b-count))}))
                  edits)]
    (->> raw
         (sort-by (juxt :start :end))
         (reduce (fn [acc {:keys [start end lines] :as r}]
                   (if-let [prev (peek acc)]
                     (if (<= start (:end prev))
                       ;; touching or overlapping — one replacement
                       (conj (pop acc)
                             {:start (:start prev)
                              :end (max (:end prev) end)
                              :lines (into (:lines prev) lines)})
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

(defn merge-lines
  "Three-way merge of line vectors. Returns

     {:clean? true  :lines [...]}
     {:clean? false :lines [...] :conflicts [{:base :ours :theirs} …]}

   The line vector is returned either way — on a conflict it carries the
   ours-side text at the conflicting regions, so a caller that wants to show
   something still can, and `:conflicts` says exactly what was elided."
  [base ours theirs]
  (let [base (vec base) ours (vec ours) theirs (vec theirs)
        o (replacements (gdiff/diff-lines base ours))
        t (replacements (gdiff/diff-lines base theirs))]
    (loop [pos 0, o o, t t, out [], conflicts []]
      (let [oh (first o) th (first t)]
        (cond
          ;; nothing left to apply — the rest of base survives
          (and (nil? oh) (nil? th))
          (cond-> {:clean? (empty? conflicts)
                   :lines (into out (subvec base (min pos (count base))))}
            (seq conflicts) (assoc :conflicts conflicts))

          ;; both sides touch the same region
          (and oh th (overlaps? oh th))
          (let [start (min (:start oh) (:start th))
                end (max (:end oh) (:end th))]
            (if (= (:lines oh) (:lines th))
              ;; the SAME edit made twice is not a conflict — two agents told to
              ;; do one thing both doing it is the expected case, not a clash
              (recur end (rest o) (rest t)
                     (into (into out (subvec base pos start)) (:lines oh))
                     conflicts)
              (recur end (rest o) (rest t)
                     (into (into out (subvec base pos start)) (:lines oh))
                     (conj conflicts
                           {:base (subvec base (min start (count base))
                                          (min end (count base)))
                            :ours (:lines oh)
                            :theirs (:lines th)}))))

          ;; only ours, or ours comes first
          (and oh (or (nil? th) (< (:start oh) (:start th))))
          (recur (:end oh) (rest o) t
                 (into (into out (subvec base pos (:start oh))) (:lines oh))
                 conflicts)

          :else
          (recur (:end th) o (rest t)
                 (into (into out (subvec base pos (:start th))) (:lines th))
                 conflicts))))))

(defn merge-text
  "`merge-lines` over strings. Returns the same map with `:text` instead of
   `:lines`.

   Splits and rejoins on `\\n` rather than using `text-lines`' final-newline
   metadata: a merge must reproduce the file's bytes, and the round trip
   `split → join` is exact for that as long as both halves agree."
  [base ours theirs]
  (let [split #(if (str/blank? %) [] (str/split (or % "") #"\n" -1))
        r (merge-lines (split base) (split ours) (split theirs))]
    (-> r
        (assoc :text (str/join "\n" (:lines r)))
        (dissoc :lines))))
