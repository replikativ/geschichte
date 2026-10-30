(ns geschichte.text-budget
  "Bounded text accounting without allocating an encoded copy."
  (:require [geschichte.budget :as budget]))

(defn utf8-size
  "Count encoded bytes, checking every UTF-16 code unit. Unpaired surrogates
  count as three bytes (a conservative bound for the JVM replacement codec)."
  [operation text]
  (loop [i 0 size 0]
    (if (= i (count text))
      size
      (let [code #?(:clj (int (.charAt ^String text i)) :cljs (.charCodeAt text i))
            pair? (and (<= 0xD800 code 0xDBFF) (< (inc i) (count text))
                       (let [next #?(:clj (int (.charAt ^String text (inc i)))
                                     :cljs (.charCodeAt text (inc i)))]
                         (<= 0xDC00 next 0xDFFF)))
            step (if pair? 2 1)
            width (cond (< code 0x80) 1 (< code 0x800) 2 pair? 4 :else 3)]
        (budget/charge! operation :text-work step)
        (recur (+ i step) (+ size width))))))

(defn lines!
  "Preflight line-vector allocation, preserving split's trailing empty field."
  [operation text]
  (when operation
    (let [text (or text "")]
      (loop [i 0 lines (if (empty? text) 0 1)]
        (if (= i (count text))
          (budget/charge! operation :text-lines lines)
          (do (budget/charge! operation :text-work 1)
              (recur (inc i) (if (= \newline (nth text i)) (inc lines) lines))))))))
