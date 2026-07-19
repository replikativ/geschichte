(ns geschichte.git.protocol-v2
  "Protocol-v2 capability parsing and request construction. Transport (process,
  SSH, or smart HTTP) is deliberately a separate concern."
  (:require [clojure.string :as str]
            [geschichte.git.object :as object]
            [geschichte.git.pkt-line :as pkt])
  (:import [java.util Arrays]))

(defn parse-advertisement
  "Parse a protocol-v2 capability advertisement into name -> optional value."
  [bytes]
  (let [frames (pkt/decode bytes)
        lines (mapv pkt/text (take-while bytes? frames))]
    (when-not (= "version 2\n" (first lines))
      (throw (ex-info "Remote did not advertise Git protocol v2"
                      {:first-line (first lines)})))
    (into (sorted-map)
          (map (fn [line]
                 (let [[name value] (str/split (str/replace line #"\n$" "")
                                               #"=" 2)]
                   [name value])))
          (rest lines))))

(defn request
  "Encode one protocol-v2 command request. Capabilities are strings after the
  command packet; arguments are strings after the delimiter packet."
  [{:keys [command capabilities arguments]}]
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

(defn parse-ls-refs
  "Parse an `ls-refs` response. Attributes such as `peeled` and
  `symref-target` are returned as keyword keys."
  [bytes]
  (->> (pkt/decode bytes)
       (take-while bytes?)
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

(defn fetch-request
  "Encode the negotiation portion of a protocol-v2 fetch request."
  [{:keys [wants haves deepen options done? capabilities]
    :or {done? true}}]
  (request {:command "fetch"
            :capabilities capabilities
            :arguments
            (concat (map #(str "want " %) wants)
                    (map #(str "have " %) haves)
                    (when deepen [(str "deepen " deepen)])
                    options
                    (when done? ["done"]))}))

(defn parse-fetch-response
  "Extract the pack stream and progress messages from a protocol-v2 fetch
  response using Git sideband channels 1/2/3."
  [bytes]
  (let [frames (pkt/decode bytes)
        pack-index
        (first (keep-indexed
                (fn [index frame]
                  (when (and (bytes? frame)
                             (= "packfile\n" (pkt/text frame)))
                    index))
                frames))]
    (when-not pack-index
      (let [error (some (fn [frame]
                          (when (and (bytes? frame)
                                     (str/starts-with? (pkt/text frame) "ERR "))
                            (pkt/text frame)))
                        frames)]
        (throw (ex-info (or error "Git fetch response has no packfile section")
                        {:frames (mapv #(if (bytes? %) (pkt/text %) %) frames)}))))
    (let [{:keys [pack progress]}
          (reduce
           (fn [result frame]
             (if-not (bytes? frame)
               result
               (let [^bytes frame frame
                     length (alength frame)]
                 (when (zero? length)
                   (throw (ex-info "Empty sideband packet" {})))
                 (let [band (bit-and 0xff (aget frame 0))
                       payload (Arrays/copyOfRange frame 1 length)]
                   (case band
                     1 (update result :pack conj payload)
                     2 (update result :progress conj payload)
                     3 (throw (ex-info (String. ^bytes payload "UTF-8")
                                       {:band :error}))
                     (throw (ex-info "Unknown Git sideband channel"
                                     {:band band})))))))
           {:pack [] :progress []}
           (subvec (vec frames) (inc pack-index)))]
      {:pack (apply object/concat-bytes pack)
       :progress (apply str (map #(String. ^bytes % "UTF-8") progress))})))

(defn consume-fetch-response!
  "Incrementally consume a protocol-v2 fetch response from `input`.

  Sideband-1 payload bytes are passed directly to `write-pack!` as
  `(write-pack! frame offset length)` without assembling packet or pack
  collections. Progress is bounded by `:max-progress-bytes` (default 1 MiB)
  and may also be observed incrementally through `:progress-fn`."
  ([input write-pack!] (consume-fetch-response! input write-pack! nil))
  ([input write-pack! {:keys [max-progress-bytes progress-fn]
                       :or {max-progress-bytes (* 1024 1024)}}]
   (let [progress (StringBuilder.)]
     (loop [packfile? false]
       (let [frame (pkt/read-frame! input)]
         (cond
           (nil? frame)
           (if packfile?
             {:progress (str progress)}
             (throw (ex-info "Git fetch response has no packfile section" {})))

           (keyword? frame)
           (if packfile?
             {:progress (str progress)}
             (recur false))

           (not packfile?)
           (if (= "packfile\n" (pkt/text frame))
             (recur true)
             (if (str/starts-with? (pkt/text frame) "ERR ")
               (throw (ex-info (pkt/text frame) {:protocol :git-v2}))
               (recur false)))

           :else
           (let [^bytes frame frame
                 length (alength frame)]
             (when (zero? length)
               (throw (ex-info "Empty sideband packet" {})))
             (let [band (bit-and 0xff (aget frame 0))
                   payload-length (dec length)]
               (case band
                 1 (do (when (pos? payload-length)
                         (write-pack! frame 1 payload-length))
                       (recur true))
                 2 (let [message (String. frame 1 payload-length "UTF-8")]
                     (when progress-fn (progress-fn message))
                     (when (< (.length progress) max-progress-bytes)
                       (let [remaining (- max-progress-bytes (.length progress))]
                         (.append ^StringBuilder progress
                                  (.substring ^String message 0
                                              (min remaining
                                                   (.length ^String message))))))
                     (recur true))
                 3 (throw (ex-info (String. frame 1 payload-length "UTF-8")
                                   {:band :error}))
                 (throw (ex-info "Unknown Git sideband channel"
                                 {:band band})))))))))))
