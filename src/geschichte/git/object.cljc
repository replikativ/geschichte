(ns geschichte.git.object
  "Byte-exact loose Git object encoding. This is the stable boundary between
  Geschichte's native model and Git SHA-1/SHA-256 object graphs."
  (:require [clojure.string :as str]
            [geschichte.bytes :as bytes])
  #?(:clj (:import [java.security MessageDigest])))

(def utf8 bytes/utf8)
(def concat-bytes bytes/concat-bytes)

(defn hex->bytes [hex]
  (when (odd? (count hex))
    (throw (ex-info "Hex string must have an even length" {:hex hex})))
  (bytes/from-values
   (map (fn [[a b]]
          #?(:clj (Integer/parseInt (str a b) 16)
             :cljs (js/parseInt (str a b) 16)))
        (partition 2 hex))))

(def ^:private hex-digits "0123456789abcdef")

(defn bytes->hex [buffer]
  (apply str
         (mapcat (fn [i]
                   (let [value #?(:clj (bit-and 0xff (aget ^bytes buffer i))
                                  :cljs (aget buffer i))]
                     [(nth hex-digits (bit-shift-right value 4))
                      (nth hex-digits (bit-and value 15))]))
                 (range (bytes/length buffer)))))

(defn frame
  "Return Git's canonical `<type> <size>\\0<payload>` byte sequence."
  [type payload]
  (concat-bytes (utf8 (str (name type) " " (bytes/length payload) "\u0000"))
                payload))

#?(:cljs
   (defn- node-digest [algorithm buffer]
     (when (exists? js/require)
       (let [crypto (js/require "crypto")
             hash (.createHash crypto (case algorithm :sha1 "sha1" :sha256 "sha256"))]
         (.update hash buffer)
         (js/Uint8Array. (.digest hash))))))

(defn object-id
  "Hash a framed object. Algorithm is :sha1 (Git's default) or :sha256.

  CLJS currently uses Node's native crypto implementation. A browser hash
  capability will be added with the async storage slice."
  ([type payload] (object-id :sha1 type payload))
  ([algorithm type payload]
   (when-not (#{:sha1 :sha256} algorithm)
     (throw (ex-info "Unsupported Git object format" {:algorithm algorithm})))
   (let [framed (frame type payload)
         digest #?(:clj (.digest (MessageDigest/getInstance
                                  (case algorithm :sha1 "SHA-1" :sha256 "SHA-256"))
                                 framed)
                   :cljs (or (node-digest algorithm framed)
                             (throw (ex-info "Synchronous Git hashing requires Node.js"
                                             {:algorithm algorithm}))))]
     (bytes->hex digest))))

(defn blob [payload]
  {:type :blob :payload payload :oid (object-id :blob payload)})

(defn- tree-sort-key [{:keys [name mode]}]
  (str name (when (or (= mode "40000") (= mode 16384)) "/")))

(defn tree-payload
  "Encode tree entries. Each entry has :mode, :name and a hex :oid."
  [entries]
  (apply concat-bytes
         (for [{:keys [mode name oid]} (sort-by tree-sort-key entries)]
           (do
             (when (or (str/includes? name "/") (str/includes? name "\u0000"))
               (throw (ex-info "Git tree entry names are single path segments"
                               {:name name})))
             (concat-bytes (utf8 (str mode " " name "\u0000"))
                           (hex->bytes oid))))))

(defn commit-payload
  "Encode a canonical Git commit body."
  [{:keys [tree parents author committer message encoding]
    :or {parents []}}]
  (when-not (and tree author committer (some? message))
    (throw (ex-info "Git commit requires tree, author, committer, and message"
                    {:tree tree :author author :committer committer})))
  (utf8
   (str "tree " tree "\n"
        (apply str (map #(str "parent " % "\n") parents))
        "author " author "\n"
        "committer " committer "\n"
        (when encoding (str "encoding " encoding "\n"))
        "\n" message)))

(defn tag-payload [{:keys [object type tag tagger message]}]
  (utf8 (str "object " object "\n"
             "type " (name type) "\n"
             "tag " tag "\n"
             (when tagger (str "tagger " tagger "\n"))
             "\n" (or message ""))))

(defn- header-message [payload]
  (let [text (bytes/decode-utf8 payload)
        split (.indexOf text "\n\n")
        header-text (if (neg? split) text (subs text 0 split))
        message (if (neg? split) "" (subs text (+ split 2)))
        headers
        (reduce (fn [headers line]
                  (if (str/starts-with? line " ")
                    (let [[name value] (peek headers)]
                      (conj (pop headers) [name (str value "\n" line)]))
                    (let [space (.indexOf line " ")]
                      (when (neg? space)
                        (throw (ex-info "Malformed Git object header" {:line line})))
                      (conj headers [(subs line 0 space) (subs line (inc space))]))))
                []
                (if (str/blank? header-text) [] (str/split-lines header-text)))]
    {:headers headers :message message}))

(defn parse-commit [payload]
  (let [{:keys [headers] :as parsed} (header-message payload)
        values (fn [name] (mapv second (filter #(= name (first %)) headers)))]
    (assoc parsed
           :tree (first (values "tree"))
           :parents (values "parent")
           :author (first (values "author"))
           :committer (first (values "committer"))
           :encoding (first (values "encoding")))))

(defn parse-tag [payload]
  (let [{:keys [headers] :as parsed} (header-message payload)
        value (fn [name] (some #(when (= name (first %)) (second %)) headers))]
    (assoc parsed :object (value "object") :type (some-> (value "type") keyword)
           :tag (value "tag") :tagger (value "tagger"))))

(defn parse-tree
  "Parse a binary Git tree payload. Object format is :sha1 or :sha256."
  ([payload] (parse-tree :sha1 payload))
  ([algorithm payload]
   (let [oid-length (case algorithm :sha1 20 :sha256 32
                          (throw (ex-info "Unsupported Git object format"
                                          {:algorithm algorithm})))
         limit (bytes/length payload)]
     (loop [position 0 entries []]
       (if (= position limit)
         entries
         (let [space (loop [index position]
                       (when (>= index limit)
                         (throw (ex-info "Truncated Git tree mode" {})))
                       (if (= 32 #?(:clj (bit-and 0xff (aget ^bytes payload index))
                                    :cljs (aget payload index)))
                         index
                         (recur (inc index))))
               nul (loop [index (inc space)]
                     (when (>= index limit)
                       (throw (ex-info "Truncated Git tree name" {})))
                     (if (zero? #?(:clj (aget ^bytes payload index)
                                   :cljs (aget payload index)))
                       index
                       (recur (inc index))))
               oid-start (inc nul)
               oid-end (+ oid-start oid-length)]
           (when (> oid-end limit)
             (throw (ex-info "Truncated Git tree object ID" {})))
           (let [name-bytes (bytes/slice payload (inc space) nul)]
             (recur oid-end
                    (conj entries
                          {:mode (bytes/decode-ascii payload position space)
                           :name (bytes/decode-utf8 name-bytes)
                           :name-bytes name-bytes
                           :oid (bytes->hex
                                 (bytes/slice payload oid-start oid-end))})))))))))
