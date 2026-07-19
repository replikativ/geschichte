(ns geschichte.git.object
  "Byte-exact loose Git object encoding. This is the stable boundary between
  Geschichte's native model and Git SHA-1/SHA-256 object graphs."
  (:require [clojure.string :as str])
  (:import [java.io ByteArrayOutputStream]
           [java.security MessageDigest]
           [java.util Arrays]))

(defn utf8 ^bytes [s]
  (.getBytes ^String (str s) "UTF-8"))

(defn concat-bytes ^bytes [& chunks]
  (let [out (ByteArrayOutputStream.)]
    (doseq [^bytes chunk chunks]
      (.write out chunk 0 (alength chunk)))
    (.toByteArray out)))

(defn hex->bytes ^bytes [hex]
  (when (odd? (count hex))
    (throw (ex-info "Hex string must have an even length" {:hex hex})))
  (byte-array
   (map (fn [[a b]]
          (unchecked-byte (Integer/parseInt (str a b) 16)))
        (partition 2 hex))))

(defn bytes->hex [^bytes bytes]
  (apply str (map #(format "%02x" (bit-and 0xff %)) bytes)))

(defn frame
  "Return Git's canonical `<type> <size>\\0<payload>` byte sequence."
  ^bytes [type ^bytes payload]
  (concat-bytes (utf8 (str (name type) " " (alength payload) "\u0000")) payload))

(defn object-id
  "Hash a framed object. Algorithm is :sha1 (Git's default) or :sha256."
  ([type payload] (object-id :sha1 type payload))
  ([algorithm type ^bytes payload]
   (let [digest (MessageDigest/getInstance
                 (case algorithm :sha1 "SHA-1" :sha256 "SHA-256"
                       (throw (ex-info "Unsupported Git object format"
                                       {:algorithm algorithm}))))]
     (bytes->hex (.digest digest (frame type payload))))))

(defn blob [^bytes payload]
  {:type :blob :payload payload :oid (object-id :blob payload)})

(defn- tree-sort-key [{:keys [name mode]}]
  ;; Git compares tree names as though directories have a trailing slash.
  (str name (when (or (= mode "40000") (= mode 16384)) "/")))

(defn tree-payload
  "Encode tree entries. Each entry has :mode, :name and a hex :oid."
  ^bytes [entries]
  (apply concat-bytes
         (for [{:keys [mode name oid]} (sort-by tree-sort-key entries)]
           (do
             (when (or (str/includes? name "/") (str/includes? name "\u0000"))
               (throw (ex-info "Git tree entry names are single path segments"
                               {:name name})))
             (concat-bytes (utf8 (str mode " " name "\u0000"))
                           (hex->bytes oid))))))

(defn commit-payload
  "Encode a canonical Git commit body. Author/committer strings include the
  identity, epoch seconds, and numeric timezone exactly as Git expects."
  ^bytes [{:keys [tree parents author committer message encoding]
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

(defn tag-payload
  ^bytes [{:keys [object type tag tagger message]}]
  (utf8 (str "object " object "\n"
             "type " (name type) "\n"
             "tag " tag "\n"
             (when tagger (str "tagger " tagger "\n"))
             "\n" (or message ""))))

(defn- header-message [^bytes payload]
  (let [text (String. payload "UTF-8")
        split (.indexOf text "\n\n")
        header-text (if (neg? split) text (subs text 0 split))
        message (if (neg? split) "" (subs text (+ split 2)))
        headers
        (reduce (fn [headers line]
                  (if (str/starts-with? line " ")
                    (let [[name value] (peek headers)]
                      (conj (pop headers) [name (str value "\n" line)]))
                    (let [space (.indexOf ^String line " ")]
                      (when (neg? space)
                        (throw (ex-info "Malformed Git object header"
                                        {:line line})))
                      (conj headers [(subs line 0 space) (subs line (inc space))]))))
                []
                (if (str/blank? header-text) [] (str/split-lines header-text)))]
    {:headers headers :message message}))

(defn parse-commit
  "Parse a Git commit payload while retaining unknown and repeated headers."
  [payload]
  (let [{:keys [headers] :as parsed} (header-message payload)
        values (fn [name] (mapv second (filter #(= name (first %)) headers)))]
    (assoc parsed
           :tree (first (values "tree"))
           :parents (values "parent")
           :author (first (values "author"))
           :committer (first (values "committer"))
           :encoding (first (values "encoding")))))

(defn parse-tag
  "Parse an annotated Git tag payload."
  [payload]
  (let [{:keys [headers] :as parsed} (header-message payload)
        value (fn [name] (some #(when (= name (first %)) (second %)) headers))]
    (assoc parsed :object (value "object") :type (some-> (value "type") keyword)
           :tag (value "tag") :tagger (value "tagger"))))

(defn parse-tree
  "Parse a binary Git tree payload. Object format is :sha1 or :sha256."
  ([payload] (parse-tree :sha1 payload))
  ([algorithm ^bytes payload]
   (let [oid-length (case algorithm :sha1 20 :sha256 32
                          (throw (ex-info "Unsupported Git object format"
                                          {:algorithm algorithm})))
         limit (alength payload)]
     (loop [position 0 entries []]
       (if (= position limit)
         entries
         (let [space (loop [index position]
                       (when (>= index limit)
                         (throw (ex-info "Truncated Git tree mode" {})))
                       (if (= 32 (bit-and 0xff (aget payload index)))
                         index
                         (recur (inc index))))
               nul (loop [index (inc space)]
                     (when (>= index limit)
                       (throw (ex-info "Truncated Git tree name" {})))
                     (if (zero? (aget payload index)) index (recur (inc index))))
               oid-start (inc nul)
               oid-end (+ oid-start oid-length)]
           (when (> oid-end limit)
             (throw (ex-info "Truncated Git tree object ID" {})))
           (let [name-bytes (Arrays/copyOfRange payload (int (inc space))
                                                (int nul))]
             (recur (long oid-end)
                    (conj entries
                          {:mode (String. payload (int position)
                                          (int (- space position))
                                          "US-ASCII")
                           :name (String. ^bytes name-bytes "UTF-8")
                           :name-bytes name-bytes
                           :oid (bytes->hex
                                 (Arrays/copyOfRange payload (int oid-start)
                                                     (int oid-end)))})))))))))
