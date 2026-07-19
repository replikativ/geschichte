(ns geschichte.git.pkt-line
  "Strict byte-preserving implementation of Git's pkt-line framing."
  (:refer-clojure :exclude [flush])
  (:require [geschichte.git.object :as object])
  (:import [java.io EOFException InputStream]
           [java.util Arrays]))

(def max-packet-length 65520)
(def max-payload-length (- max-packet-length 4))

(defn data
  "Encode one data packet. Payload may be bytes or a UTF-8 string."
  ^bytes [payload]
  (let [payload (if (string? payload) (object/utf8 payload) payload)
        length (+ 4 (alength ^bytes payload))]
    (when (> length max-packet-length)
      (throw (ex-info "Git pkt-line payload is too large"
                      {:payload-length (alength ^bytes payload)
                       :maximum max-payload-length})))
    (object/concat-bytes (object/utf8 (format "%04x" length)) payload)))

(def flush (object/utf8 "0000"))
(def delimiter (object/utf8 "0001"))
(def response-end (object/utf8 "0002"))

(defn encode
  "Encode frames. A frame is a string/byte-array or :flush/:delimiter/:response-end."
  ^bytes [frames]
  (apply object/concat-bytes
         (map (fn [frame]
                (case frame
                  :flush flush
                  :delimiter delimiter
                  :response-end response-end
                  (data frame)))
              frames)))

(defn decode
  "Decode all frames from a byte array, preserving data payload bytes."
  [^bytes bytes]
  (let [limit (alength bytes)]
    (loop [position 0 frames []]
      (if (= position limit)
        frames
        (let [_ (when (> (+ position 4) limit)
                  (throw (ex-info "Truncated pkt-line header"
                                  {:position position})))
              header (String. bytes (int position) 4 "US-ASCII")
              length (int (try (Integer/parseInt header 16)
                               (catch NumberFormatException error
                                 (throw (ex-info "Invalid pkt-line length"
                                                 {:position position :header header}
                                                 error)))))]
          (case length
            0 (recur (+ position 4) (conj frames :flush))
            1 (recur (+ position 4) (conj frames :delimiter))
            2 (recur (+ position 4) (conj frames :response-end))
            3 (throw (ex-info "Reserved pkt-line length" {:position position}))
            (let [_ (when (< length 4)
                      (throw (ex-info "Invalid pkt-line length"
                                      {:position position :length length})))
                  _ (when (> (+ position length) limit)
                      (throw (ex-info "Truncated pkt-line payload"
                                      {:position position :length length
                                       :available (- limit position)})))
                  payload (Arrays/copyOfRange bytes (int (+ position 4))
                                              (int (+ position length)))]
              (recur (long (+ position length)) (conj frames payload)))))))))

(defn text
  "Decode a data-frame byte array as UTF-8."
  [frame]
  (when (bytes? frame) (String. ^bytes frame "UTF-8")))

(defn- read-exactly [^InputStream input length]
  (let [bytes (.readNBytes input length)]
    (when-not (= length (alength bytes))
      (throw (EOFException. (str "Truncated pkt-line: wanted " length
                                 " bytes, got " (alength bytes)))))
    bytes))

(defn read-frame!
  "Read one frame from an InputStream. Returns nil on clean EOF."
  [^InputStream input]
  (let [first-byte (.read input)]
    (when-not (= -1 first-byte)
      (let [^bytes header-rest (read-exactly input 3)
            header (byte-array [(unchecked-byte first-byte)
                                (aget header-rest 0)
                                (aget header-rest 1)
                                (aget header-rest 2)])
            header-text (String. header "US-ASCII")
            length (int (try (Integer/parseInt header-text 16)
                             (catch NumberFormatException error
                               (throw (ex-info "Invalid pkt-line length"
                                               {:header header-text} error)))))]
        (case length
          0 :flush
          1 :delimiter
          2 :response-end
          3 (throw (ex-info "Reserved pkt-line length" {}))
          (do
            (when (< length 4)
              (throw (ex-info "Invalid pkt-line length" {:length length})))
            (read-exactly input (- length 4))))))))

(defn read-through-flush!
  "Read frames through and including the next flush packet."
  [input]
  (loop [frames []]
    (let [frame (read-frame! input)]
      (cond
        (nil? frame) (throw (EOFException. "EOF before pkt-line flush"))
        (= :flush frame) (conj frames frame)
        :else (recur (conj frames frame))))))
