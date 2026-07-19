(ns geschichte.git.protocol-v2
  "Portable protocol-v2 capability parsing and request construction."
  (:require [clojure.string :as str]
            [geschichte.bytes :as bytes]
            [geschichte.git.object :as object]
            [geschichte.git.pkt-line :as pkt]))

(defn parse-advertisement [buffer]
  (let [frames (pkt/decode buffer)
        lines (mapv pkt/text (take-while bytes/byte-buffer? frames))]
    (when-not (= "version 2\n" (first lines))
      (throw (ex-info "Remote did not advertise Git protocol v2"
                      {:first-line (first lines)})))
    (into (sorted-map)
          (map (fn [line]
                 (let [[name value] (str/split (str/replace line #"\n$" "")
                                               #"=" 2)]
                   [name value])))
          (rest lines))))

(defn request [{:keys [command capabilities arguments]}]
  (pkt/encode
   (concat [(str "command=" command "\n")]
           (map #(str % "\n") capabilities)
           [:delimiter]
           (map #(str % "\n") arguments)
           [:flush])))

(defn ls-refs-request
  ([] (ls-refs-request nil))
  ([{:keys [prefixes peel? symrefs? unborn? capabilities]
     :or {peel? true symrefs? true}}]
   (request {:command "ls-refs"
             :capabilities capabilities
             :arguments
             (concat (when peel? ["peel"])
                     (when symrefs? ["symrefs"])
                     (when unborn? ["unborn"])
                     (map #(str "ref-prefix " %) prefixes))})))

(defn parse-ls-refs [buffer]
  (->> (pkt/decode buffer)
       (take-while bytes/byte-buffer?)
       (mapv
        (fn [frame]
          (let [[first-field ref-name & attributes]
                (str/split (str/replace (pkt/text frame) #"\n$" "") #" ")
                unborn? (= first-field "unborn")]
            {:oid (when-not unborn? first-field)
             :ref ref-name
             :unborn? unborn?
             :attributes
             (into {}
                   (map (fn [attribute]
                          (let [[name value] (str/split attribute #":" 2)]
                            [(keyword name) value])))
                   attributes)})))))

(defn fetch-request [{:keys [wants haves deepen options done? capabilities]
                      :or {done? true}}]
  (request {:command "fetch"
            :capabilities capabilities
            :arguments
            (concat (map #(str "want " %) wants)
                    (map #(str "have " %) haves)
                    (when deepen [(str "deepen " deepen)])
                    options
                    (when done? ["done"]))}))

(defn parse-fetch-response [buffer]
  (let [frames (pkt/decode buffer)
        pack-index
        (first (keep-indexed
                (fn [index frame]
                  (when (and (bytes/byte-buffer? frame)
                             (= "packfile\n" (pkt/text frame)))
                    index))
                frames))]
    (when-not pack-index
      (let [error (some (fn [frame]
                          (when (and (bytes/byte-buffer? frame)
                                     (str/starts-with? (pkt/text frame) "ERR "))
                            (pkt/text frame)))
                        frames)]
        (throw (ex-info (or error "Git fetch response has no packfile section")
                        {:frames (mapv #(if (bytes/byte-buffer? %) (pkt/text %) %)
                                       frames)}))))
    (let [{:keys [pack progress]}
          (reduce
           (fn [result frame]
             (if-not (bytes/byte-buffer? frame)
               result
               (let [length (bytes/length frame)]
                 (when (zero? length)
                   (throw (ex-info "Empty sideband packet" {})))
                 (let [band #?(:clj (bit-and 0xff (aget ^bytes frame 0))
                               :cljs (aget frame 0))
                       payload (bytes/slice frame 1 length)]
                   (case band
                     1 (update result :pack conj payload)
                     2 (update result :progress conj payload)
                     3 (throw (ex-info (bytes/decode-utf8 payload) {:band :error}))
                     (throw (ex-info "Unknown Git sideband channel"
                                     {:band band})))))))
           {:pack [] :progress []}
           (subvec (vec frames) (inc pack-index)))]
      {:pack (apply object/concat-bytes pack)
       :progress (apply str (map bytes/decode-utf8 progress))})))
