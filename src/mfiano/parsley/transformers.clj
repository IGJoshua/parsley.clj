(ns mfiano.parsley.transformers)

(defn bytes-reverse! [^bytes bytes]
  (let [length (alength bytes)]
    (loop [i 0]
      (when (< i (/ length 2))
        (let [temp (aget bytes i)
              idx (- length i 1)]
          (aset bytes i (aget bytes idx))
          (aset bytes idx temp)
          (recur (inc i)))))))

(defn bytes->uint
  [^bytes bytes]
  (bigint (BigInteger. 1 bytes)))

(defn bytes->int
  [bytes]
  (.longValue (bigint bytes)))

(defn bytes->hex-string
  [bytes]
  (apply str (for [b bytes] (format "%02x" b))))
