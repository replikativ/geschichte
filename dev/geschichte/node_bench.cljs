(ns geschichte.node-bench
  (:require [geschichte.diff :as diff]
            [geschichte.git.object :as object]
            [geschichte.git.pkt-line :as pkt]))

(defn- measure [iterations f]
  (dotimes [_ 3] (f))
  (let [samples
        (vec (for [_ (range iterations)]
               (let [start (.now js/performance)]
                 (f)
                 (- (.now js/performance) start))))]
    {:median-ms (nth (sort samples) (quot iterations 2))
     :min-ms (apply min samples)}))

(defn -main [& _]
  (let [a (apply str (for [i (range 5000)]
                       (str "line " i
                            " shared payload alpha beta gamma delta\n")))
        b (apply str (for [i (range 5000)]
                       (str "line " i " shared payload "
                            (if (zero? (mod i 100)) "changed" "alpha beta")
                            " gamma delta\n")))
        payload (object/utf8 a)
        frames (vec (concat (repeat 1000 "capability=value\n") [:flush]))]
    (prn {:runtime :node-cljs
          :bytes [(count a) (count b)]
          :diff-text-unified
          (measure 9 #(-> (diff/diff-text a b)
                          (diff/unified {:context 3}) count))
          :sha1-239kb (measure 25 #(object/object-id :blob payload))
          :pkt-line-1000
          (measure 25 #(-> frames pkt/encode pkt/decode count))})))
