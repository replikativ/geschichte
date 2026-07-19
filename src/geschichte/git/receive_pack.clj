(ns geschichte.git.receive-pack
  "Client-side receive-pack framing used by Git push. Push is not a protocol-v2
  command; it retains the receive-pack command/capability exchange."
  (:require [clojure.string :as str]
            [geschichte.git.object :as object]
            [geschichte.git.pkt-line :as pkt]))

(def zero-oid (apply str (repeat 40 "0")))

(defn parse-advertisement
  "Parse a receive-pack ref/capability advertisement."
  [bytes]
  (let [lines (->> (pkt/decode bytes) (take-while bytes?) (mapv pkt/text))]
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

(defn request
  "Encode ref update commands followed by a pack. Each update has :old, :new,
  and :ref. Capabilities are attached only to the first command."
  [{:keys [updates capabilities pack]}]
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
                         (or pack (byte-array 0)))))

(defn parse-report
  "Parse a report-status/report-status-v2 response without sideband wrapping."
  [bytes]
  (let [lines (->> (pkt/decode bytes)
                   (filter bytes?)
                   (map #(str/replace (pkt/text %) #"\n$" "")))]
    (reduce
     (fn [result line]
       (cond
         (str/starts-with? line "unpack ")
         (assoc result :unpack (subs line (count "unpack ")))

         (str/starts-with? line "ok ")
         (assoc-in result [:refs (subs line (count "ok "))]
                   {:status :ok})

         (str/starts-with? line "ng ")
         (let [[ref reason] (str/split (subs line (count "ng ")) #" " 2)]
           (assoc-in result [:refs ref] {:status :rejected :reason reason}))

         ;; report-status-v2 can add option lines after a ref status. Retain
         ;; unknown lines so higher layers do not silently discard diagnostics.
         :else (update result :other conj line)))
     {:refs (sorted-map) :other []}
     lines)))
