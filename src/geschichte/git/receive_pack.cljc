(ns geschichte.git.receive-pack
  "Portable client-side receive-pack framing."
  (:require [clojure.string :as str]
            [geschichte.bytes :as bytes]
            [geschichte.git.object :as object]
            [geschichte.git.pkt-line :as pkt]))

(def zero-oid (apply str (repeat 40 "0")))

(defn parse-advertisement [buffer]
  (let [lines (->> (pkt/decode buffer)
                   (take-while bytes/byte-buffer?)
                   (mapv pkt/text))]
    (reduce-kv
     (fn [result index line]
       (let [line (str/replace line #"\n$" "")
             [ref-part capability-part]
             (if (zero? index) (str/split line #"\u0000" 2) [line nil])
             [oid ref] (str/split ref-part #" " 2)]
         (cond-> (assoc-in result [:refs ref] oid)
           capability-part
           (assoc :capabilities (set (str/split capability-part #" "))))))
     {:refs (sorted-map) :capabilities #{}}
     lines)))

(defn request [{:keys [updates capabilities pack]}]
  (when (empty? updates)
    (throw (ex-info "receive-pack requires at least one ref update" {})))
  (let [commands
        (map-indexed
         (fn [index {:keys [old new ref]}]
           (str old " " new " " ref
                (when (zero? index)
                  (str "\u0000" (str/join " " capabilities)))
                "\n"))
         updates)]
    (object/concat-bytes (pkt/encode (concat commands [:flush]))
                         (or pack (bytes/empty-bytes)))))

(defn parse-report [buffer]
  (let [lines (->> (pkt/decode buffer)
                   (filter bytes/byte-buffer?)
                   (map #(str/replace (pkt/text %) #"\n$" "")))]
    (reduce
     (fn [result line]
       (cond
         (str/starts-with? line "unpack ")
         (assoc result :unpack (subs line (count "unpack ")))

         (str/starts-with? line "ok ")
         (assoc-in result [:refs (subs line (count "ok "))] {:status :ok})

         (str/starts-with? line "ng ")
         (let [[ref reason] (str/split (subs line (count "ng ")) #" " 2)]
           (assoc-in result [:refs ref] {:status :rejected :reason reason}))

         :else (update result :other conj line)))
     {:refs (sorted-map) :other []}
     lines)))
