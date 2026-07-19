(ns geschichte.git.pkt-line
  "Strict byte-preserving implementation of Git's pkt-line framing. Array
  codecs are portable; stateful InputStream reads are JVM-only."
  (:refer-clojure :exclude [flush])
  (:require [geschichte.bytes :as bytes]
            [geschichte.git.object :as object])
  #?(:clj (:import [java.io EOFException InputStream])))

(def max-packet-length 65520)
(def max-payload-length (- max-packet-length 4))

(defn- hex-length [length]
  (let [encoded #?(:clj (Integer/toHexString (int length))
                   :cljs (.toString length 16))]
    (str (apply str (repeat (- 4 (count encoded)) "0")) encoded)))

(defn data
  "Encode one data packet. Payload may be bytes or a UTF-8 string."
  [payload]
  (let [payload (if (string? payload) (object/utf8 payload) payload)
        length (+ 4 (bytes/length payload))]
    (when (> length max-packet-length)
      (throw (ex-info "Git pkt-line payload is too large"
                      {:payload-length (bytes/length payload)
                       :maximum max-payload-length})))
    (object/concat-bytes (object/utf8 (hex-length length)) payload)))

(def flush (object/utf8 "0000"))
(def delimiter (object/utf8 "0001"))
(def response-end (object/utf8 "0002"))

(defn encode
  "Encode frames. A frame is a string/byte buffer or a control keyword."
  [frames]
  (apply object/concat-bytes
         (map (fn [frame]
                (case frame
                  :flush flush
                  :delimiter delimiter
                  :response-end response-end
                  (data frame)))
              frames)))

(defn- parse-length [header position]
  (let [length #?(:clj (try (Integer/parseInt header 16)
                            (catch NumberFormatException error
                              (throw (ex-info "Invalid pkt-line length"
                                              {:position position :header header}
                                              error))))
                  :cljs (js/parseInt header 16))]
    #?(:cljs
       (when (js/isNaN length)
         (throw (ex-info "Invalid pkt-line length"
                         {:position position :header header}))))
    length))

(defn decode
  "Decode all frames, preserving data payload bytes."
  [buffer]
  (let [limit (bytes/length buffer)]
    (loop [position 0 frames []]
      (if (= position limit)
        frames
        (let [_ (when (> (+ position 4) limit)
                  (throw (ex-info "Truncated pkt-line header" {:position position})))
              header (bytes/decode-ascii buffer position (+ position 4))
              length (parse-length header position)]
          (case length
            0 (recur (+ position 4) (conj frames :flush))
            1 (recur (+ position 4) (conj frames :delimiter))
            2 (recur (+ position 4) (conj frames :response-end))
            3 (throw (ex-info "Reserved pkt-line length" {:position position}))
            (do
              (when (< length 4)
                (throw (ex-info "Invalid pkt-line length"
                                {:position position :length length})))
              (when (> (+ position length) limit)
                (throw (ex-info "Truncated pkt-line payload"
                                {:position position :length length
                                 :available (- limit position)})))
              (recur (+ position length)
                     (conj frames
                           (bytes/slice buffer (+ position 4)
                                        (+ position length)))))))))))

(defn text
  "Decode a data-frame byte buffer as UTF-8."
  [frame]
  (when (bytes/byte-buffer? frame) (bytes/decode-utf8 frame)))

#?(:clj
   (defn- read-exactly [^InputStream input length]
     (let [buffer (.readNBytes input length)]
       (when-not (= length (alength buffer))
         (throw (EOFException. (str "Truncated pkt-line: wanted " length
                                    " bytes, got " (alength buffer)))))
       buffer)))

#?(:clj
   (defn read-frame!
     "Read one frame from an InputStream. Returns nil on clean EOF. JVM-only."
     [^InputStream input]
     (let [first-byte (.read input)]
       (when-not (= -1 first-byte)
         (let [header-rest (read-exactly input 3)
               header (bytes/from-values [first-byte
                                          (aget header-rest 0)
                                          (aget header-rest 1)
                                          (aget header-rest 2)])
               length (parse-length (bytes/decode-ascii header) 0)]
           (case length
             0 :flush
             1 :delimiter
             2 :response-end
             3 (throw (ex-info "Reserved pkt-line length" {}))
             (do
               (when (< length 4)
                 (throw (ex-info "Invalid pkt-line length" {:length length})))
               (read-exactly input (- length 4)))))))))

#?(:clj
   (defn read-through-flush!
     "Read frames through and including the next flush packet. JVM-only."
     [input]
     (loop [frames []]
       (let [frame (read-frame! input)]
         (cond
           (nil? frame) (throw (EOFException. "EOF before pkt-line flush"))
           (= :flush frame) (conj frames frame)
           :else (recur (conj frames frame)))))))
