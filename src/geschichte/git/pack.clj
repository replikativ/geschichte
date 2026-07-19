(ns geschichte.git.pack
  "Git pack v2 writer. The first writer is deliberately undeltified: larger but
  fully valid, streamable, and independent of future delta-selection policy."
  (:require [geschichte.git.object :as object])
  (:import [java.io ByteArrayOutputStream]
           [java.nio ByteBuffer ByteOrder]
           [java.security MessageDigest]
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
