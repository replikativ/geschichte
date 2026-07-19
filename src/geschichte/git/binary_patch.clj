(ns geschichte.git.binary-patch
  "Git-compatible binary patch bodies.

  Literal bodies are intentionally used as the compatibility baseline. Git
  may choose a smaller delta body, but both encodings have identical apply
  semantics and native `git apply` accepts either form."
  (:import [java.io OutputStream]
           [java.util.zip Deflater DeflaterOutputStream]))

(set! *warn-on-reflection* true)

(def ^:private alphabet
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!#$%&()*+-;<=>?@^_`{|}~")

(defn- encode-group [^bytes data offset length]
  (let [value (loop [index 0, value 0]
                (if (= index 4)
                  value
                  (recur (inc index)
                         (+ (* value 256)
                            (if (< index length)
                              (bit-and 0xff (aget data (+ offset index)))
                              0)))))]
    (loop [index 4, value value, result (char-array 5)]
      (if (neg? index)
        (String. result)
        (let [digit (mod value 85)]
          (aset result index (.charAt ^String alphabet (int digit)))
          (recur (dec index) (quot value 85) result))))))

(defn- encode-line [^bytes data offset length]
  (str (char (if (<= length 26)
               (+ (int \A) (dec length))
               (+ (int \a) (- length 27))))
       (apply str
              (for [group-offset (range 0 length 4)]
                (encode-group data (+ offset group-offset)
                              (min 4 (- length group-offset)))))
       "\n"))

(defn- base85-output-stream [^Appendable output]
  (let [buffer (byte-array 52)
        count (int-array 1)
        emit! (fn []
                (let [length (aget count 0)]
                  (when (pos? length)
                    (.append output ^CharSequence (encode-line buffer 0 length))
                    (aset-int count 0 0))))]
    (proxy [OutputStream] []
      (write
        ([value]
         (let [offset (aget count 0)]
           (aset buffer offset (unchecked-byte value))
           (aset-int count 0 (int (inc offset)))
           (when (= 52 (aget count 0)) (emit!))))
        ([^bytes data offset length]
         (loop [offset offset, remaining length]
           (when (pos? remaining)
             (let [buffer-offset (aget count 0)
                   copied (min remaining (- 52 buffer-offset))]
               (System/arraycopy data offset buffer buffer-offset copied)
               (aset-int count 0 (int (+ buffer-offset copied)))
               (when (= 52 (aget count 0)) (emit!))
               (recur (+ offset copied) (- remaining copied)))))))
      (close [] (emit!)))))

(defn- write-literal-body!
  [^Appendable output size write-payload!]
  (.append output (str "literal " size "\n"))
  (let [^OutputStream encoded (base85-output-stream output)
        ^Deflater compressor (Deflater. Deflater/DEFAULT_COMPRESSION)]
    (with-open [stream (DeflaterOutputStream. encoded compressor)]
      (write-payload! stream)))
  (.append output "\n")
  output)

(defn write-patch!
  "Write a forward/reverse literal patch to an Appendable without retaining
  compressed payloads. The host may pass a Writer to stream the encoded patch."
  [^Appendable output before-size write-before! after-size write-after!]
  (.append output "GIT binary patch\n")
  (write-literal-body! output after-size write-after!)
  (write-literal-body! output before-size write-before!)
  output)

(defn patch-streaming
  "Return a complete forward/reverse literal patch. Each writer receives an
  OutputStream and may feed it bounded content chunks."
  [before-size write-before! after-size write-after!]
  (str (write-patch! (StringBuilder.) before-size write-before!
                     after-size write-after!)))

(defn patch
  "Return a complete forward/reverse Git binary patch body."
  [^bytes before ^bytes after]
  (patch-streaming
   (alength before) #(.write ^OutputStream % before 0 (alength before))
   (alength after) #(.write ^OutputStream % after 0 (alength after))))
