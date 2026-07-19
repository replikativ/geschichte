(ns geschichte.bench-diff
  (:require [geschichte.diff :as diff])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- measure [iterations f]
  (dotimes [_ 3] (f))
  (let [samples
        (vec (for [_ (range iterations)]
               (let [start (System/nanoTime)]
                 (f)
                 (/ (- (System/nanoTime) start) 1e6))))]
    {:median-ms (nth (sort samples) (quot iterations 2))
     :min-ms (apply min samples)
     :samples-ms samples}))

(defn -main [& _]
  (let [a (apply str (for [i (range 5000)]
                       (str "line " i
                            " shared payload alpha beta gamma delta\n")))
        b (apply str (for [i (range 5000)]
                       (str "line " i " shared payload "
                            (if (zero? (mod i 100)) "changed" "alpha beta")
                            " gamma delta\n")))
        a-lines (:lines (diff/text-lines a))
        b-lines (:lines (diff/text-lines b))
        structured (diff/diff-text a b)
        line-result (measure 9 #(diff/diff-lines a-lines b-lines))
        clj-result (measure 9 #(-> (diff/diff-text a b)
                                   (diff/unified {:context 3}) count))
        render-result (measure 9 #(count (diff/unified structured {:context 3})))
        dir (Files/createTempDirectory
             "geschichte-diff-bench" (make-array FileAttribute 0))
        a-path (.resolve dir "a.txt")
        b-path (.resolve dir "b.txt")]
    (Files/writeString a-path a StandardCharsets/UTF_8
                       (make-array java.nio.file.OpenOption 0))
    (Files/writeString b-path b StandardCharsets/UTF_8
                       (make-array java.nio.file.OpenOption 0))
    (let [git-result
          (measure
           9
           #(let [process
                  (.start (ProcessBuilder.
                           ["git" "diff" "--no-index" "--no-color" "--unified=3"
                            (str a-path) (str b-path)]))]
              (.readAllBytes (.getInputStream process))
              (.readAllBytes (.getErrorStream process))
              (when-not (= 1 (.waitFor process))
                (throw (ex-info "Native git diff failed" {})))))]
      (prn {:bytes [(count a) (count b)]
            :lines 5000 :changes 50
            :geschichte-lines line-result
            :geschichte-render render-result
            :geschichte-end-to-end clj-result
            :native-git git-result
            :median-ratio
            (/ (:median-ms clj-result) (:median-ms git-result))}))))
