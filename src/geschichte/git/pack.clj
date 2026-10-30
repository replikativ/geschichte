(ns geschichte.git.pack
  "Git pack v2 writer. The first writer is deliberately undeltified: larger but
  fully valid, streamable, and independent of future delta-selection policy."
  (:require [geschichte.git.object :as object]
            [geschichte.git.budget :as budget]
            [geschichte.budget :as operation])
  (:import [java.io ByteArrayOutputStream OutputStream]
           [java.nio ByteBuffer ByteOrder]
           [java.security MessageDigest DigestOutputStream]
           [java.util.zip Deflater DeflaterOutputStream]))

(def ^:private type-code
  {:commit 1 :tree 2 :blob 3 :tag 4})

(defn- int32 ^bytes [value]
  (-> (ByteBuffer/allocate 4)
      (.order ByteOrder/BIG_ENDIAN)
      (.putInt (int value))
      (.array)))

(defn- entry-header
  ^bytes [type size]
  (let [code (or (type-code type)
                 (throw (ex-info "Unsupported undeltified pack object type"
                                 {:type type})))]
    (loop [remaining (unsigned-bit-shift-right (long size) 4)
           out [(bit-or (bit-shift-left code 4)
                        (bit-and size 0x0f))]]
      (if (zero? remaining)
        (byte-array (map unchecked-byte out))
        (recur (unsigned-bit-shift-right remaining 7)
               (conj (update out (dec (count out)) #(bit-or % 0x80))
                     (bit-and remaining 0x7f)))))))

(defn- deflate ^bytes [^bytes payload]
  (let [out (ByteArrayOutputStream.)
        deflater (Deflater. Deflater/DEFAULT_COMPRESSION)]
    (with-open [stream (DeflaterOutputStream. out deflater)]
      (.write stream payload))
    (.toByteArray out)))

(defn encode
  "Encode `{oid {:type keyword :payload bytes}}` as a Git pack v2 byte array.
  OIDs are used only for deterministic ordering; every entry is stored whole."
  ^bytes [objects]
  (let [ordered (sort-by key objects)
        body
        (apply object/concat-bytes
               (object/utf8 "PACK")
               (int32 2)
               (int32 (count ordered))
               (mapcat (fn [[_ {:keys [type payload]}]]
                         [(entry-header type (alength ^bytes payload))
                          (deflate payload)])
                       ordered))
        trailer (.digest (MessageDigest/getInstance "SHA-1") body)]
    (object/concat-bytes body trailer)))

(defn encode-bounded
  "Encode a finite undeltified pack with one shared operation. Charge output
   BEFORE buffer growth; compress input in small cooperative chunks. No complete
   compressed-object intermediates or full concatenation of them are retained.
   The caller owns input payloads; the returned pack is transient native bytes."
  ^bytes [objects op]
  (operation/require-limits! op [:objects :object-bytes :pack-bytes])
  (when-not (map? objects) (throw (ex-info "Pack objects must be a finite map" {})))
  (budget/charge! op :objects (count objects))
  (let [out (ByteArrayOutputStream.) digest (MessageDigest/getInstance "SHA-1")
        charged (proxy [OutputStream] []
                  (write
                    ([value]
                     (if (number? value)
                       (do (budget/charge! op :pack-bytes 1) (.write out (int value)))
                       (let [^bytes value value]
                         (budget/charge! op :pack-bytes (alength value))
                         (.write out value 0 (alength value)))))
                    ([value offset length]
                     (budget/charge! op :pack-bytes length)
                     (.write out ^bytes value (int offset) (int length)))))
        stream (DigestOutputStream. charged digest)]
    (.write stream (object/utf8 "PACK"))
    (.write stream (int32 2))
    (.write stream (int32 (count objects)))
    (doseq [[oid {:keys [type payload]}] (sort-by key objects)]
      (budget/check! op)
      (when-not (and (string? oid) (re-matches #"[0-9a-f]{40}" oid) (bytes? payload))
        (throw (ex-info "Invalid pack object" {})))
      (budget/bound! op :object-bytes (alength ^bytes payload))
      (.write stream (entry-header type (alength ^bytes payload)))
      (let [deflater (Deflater. Deflater/DEFAULT_COMPRESSION)
            compressed (DeflaterOutputStream. stream deflater 8192)]
        (try
          (loop [offset 0]
            (when (< offset (alength ^bytes payload))
              (budget/check! op)
              (let [n (min 8192 (- (alength ^bytes payload) offset))]
                (.write compressed ^bytes payload (int offset) (int n))
                (recur (+ offset n)))))
          (.finish compressed)
          (finally (.end deflater)))))
    (.write ^OutputStream charged (.digest digest))
    (budget/check! op)
    (.toByteArray out)))
