(ns geschichte.bytes
  "Small platform-specialized byte boundary.

  Public codecs share this API, while hot loops still use primitive byte[] or
  Uint8Array directly through reader conditionals. No persistent-vector
  conversion occurs on the CLJS path."
  #?(:clj (:import [java.io ByteArrayOutputStream]
                   [java.nio.charset StandardCharsets]
                   [java.util Arrays])))

(defn byte-buffer?
  [value]
  #?(:clj (bytes? value)
     :cljs (instance? js/Uint8Array value)))

(defn length [bytes]
  #?(:clj (alength ^bytes bytes)
     :cljs (.-length bytes)))

(defn empty-bytes []
  #?(:clj (byte-array 0)
     :cljs (js/Uint8Array. 0)))

(defn from-values
  "Construct bytes from integer values, truncating each to the low eight bits."
  [values]
  #?(:clj (byte-array (map unchecked-byte values))
     :cljs (js/Uint8Array. (clj->js (vec values)))))

(defn utf8 [value]
  #?(:clj (.getBytes ^String (str value) StandardCharsets/UTF_8)
     :cljs (.encode (js/TextEncoder.) (str value))))

(defn decode-utf8 [bytes]
  #?(:clj (String. ^bytes bytes StandardCharsets/UTF_8)
     :cljs (.decode (js/TextDecoder. "utf-8") bytes)))

(defn decode-ascii
  ([bytes] (decode-ascii bytes 0 (length bytes)))
  ([bytes start end]
   #?(:clj (String. ^bytes bytes (int start) (int (- end start))
                    StandardCharsets/US_ASCII)
      :cljs (.decode (js/TextDecoder. "ascii") (.subarray bytes start end)))))

(defn slice [bytes start end]
  #?(:clj (Arrays/copyOfRange ^bytes bytes (int start) (int end))
     :cljs (.slice bytes start end)))

(defn concat-bytes [& chunks]
  #?(:clj
     (let [out (ByteArrayOutputStream.)]
       (doseq [^bytes chunk chunks]
         (.write out chunk 0 (alength chunk)))
       (.toByteArray out))
     :cljs
     (let [out (js/Uint8Array. (reduce + (map length chunks)))]
       (loop [offset 0 remaining chunks]
         (if-let [chunk (first remaining)]
           (do (.set out chunk offset)
               (recur (+ offset (length chunk)) (next remaining)))
           out)))))

(defn same-bytes?
  "Content equality without allocating an intermediate collection."
  [a b]
  (and (= (length a) (length b))
       (loop [i 0]
         (or (= i (length a))
             (and (= #?(:clj (bit-and 0xff (aget ^bytes a i))
                        :cljs (aget a i))
                     #?(:clj (bit-and 0xff (aget ^bytes b i))
                        :cljs (aget b i)))
                  (recur (inc i)))))))
