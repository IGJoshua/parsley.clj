(ns mfiano.parsley.data-types
  (:refer-clojure :exclude [read read-string])
  (:require [mfiano.parsley.transformers :as tr]
            [net.cgrand.xforms :as xf])
  (:import [java.io InputStreamReader]
           [java.nio ByteBuffer]
           [com.tomgibara.bits Bits BitReader BitStore EndOfBitStreamException]))

(defn- convert-string-encoding
  [{:keys [encoding endian]}]
  (case encoding
    :ascii "US-ASCII"
    :latin1 "ISO-8859-1"
    :utf8 "UTF-8"
    :utf16 (if (= endian :be)
             "UTF-16BE"
             "UTF-16LE")
    (throw (UnsupportedOperationException. (format "Invalid string encoding: %s" encoding)))))

(defn- get-string-bytes
  [s options]
  (vec (.getBytes ^String s
                  ^String (convert-string-encoding options))))

(defn- sequence*
  [xf coll]
  (let [rf (xf (fn [_ v] v))
        f (fn f [coll]
            (lazy-seq (loop [coll coll]
                        (when (seq coll)
                          (if-let [res (rf nil (first coll))]
                            (if (reduced? res)
                              (cons @res nil)
                              (cons res (f (rest coll))))
                            (recur (rest coll)))))))]
    (f coll)))

(defn- get-delimited-string-bytes
  [bytes {:keys [delimiter] :as options}]
  (let [delimiter-bytes (get-string-bytes (or delimiter "") options)
        xf (comp (xf/partition (count delimiter-bytes) 1)
                 (map-indexed vector)
                 (filter #(= (second %) delimiter-bytes))
                 (map first))
        delimiter-index (first (sequence* xf bytes))
        new-bytes (take (or delimiter-index 0)
                        bytes)]
    (if (nil? delimiter-index)
      (byte-array bytes)
      (byte-array new-bytes))))

(defn- read-boolean
  [^BitReader reader]
  (.readBoolean reader))

(defn- read-bits
  [^BitReader reader {:keys [size]}]
  (let [bits (.readBigInt reader size)]
    bits))

(defn- read-bytes
  [^BitReader reader {:keys [size endian]}]
  (let [store (Bits/store size)]
    (.readFrom store reader)
    (let [bytes (.toByteArray store)]
      (case endian
        :be bytes
        :le (doto bytes tr/bytes-reverse!)))))

(defn- bytes-seq
  [^BitReader reader {:keys [size]}]
  (letfn [(f [remaining]
            (when (or (nil? remaining)
                      (pos? remaining))
              (lazy-seq (try (cons (unchecked-byte (.read reader 8))
                                   (f (and remaining
                                           (dec remaining))))
                             (catch EndOfBitStreamException e
                               nil)))))]
    (f (when size
         (int (/ size 8))))))

(defn read-string
  [^BitReader reader {:keys [delimiter size] :as options}]
  (let [loc (.getPosition reader)
        bytes (bytes-seq reader options)
        delimited-bytes ^bytes (get-delimited-string-bytes bytes options)
        ret (String. delimited-bytes
                     ^String (convert-string-encoding options))]
    (.setPosition reader (+ loc
                            (* (count delimited-bytes) 8)
                            (* (count (get-string-bytes delimiter options)) 8)))
    ret))

(defn- read-float
  [^BitReader reader options]
  (let [bytes (read-bytes reader (assoc options :size 32))
        buffer (ByteBuffer/wrap bytes)]
    (.getFloat buffer)))

(defn- read-double
  [^BitReader reader options]
  (let [bytes (read-bytes reader (assoc options :size 64))
        buffer (ByteBuffer/wrap bytes)]
    (.getDouble buffer)))

(defn read
  [data-type
   {:keys [reader spec]}
   & {:as options}]
  (let [defaults (select-keys spec [:endian :encoding])
        options (merge defaults options)]
    (case data-type
      :boolean (read-boolean reader)
      :bits (read-bits reader options)
      :bytes (read-bytes reader options)
      :int (tr/bytes->int (read-bytes reader options))
      :uint (tr/bytes->uint (read-bytes reader options))
      :string (read-string reader options)
      :float (read-float reader options)
      :double (read-double reader options))))
