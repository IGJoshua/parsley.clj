(ns mfiano.parsley.data-types
  (:refer-clojure :exclude [read read-string])
  (:require [mfiano.parsley.transformers :as tr]
            [net.cgrand.xforms :as xf])
  (:import [java.io InputStreamReader]
           [com.tomgibara.bits Bits BitReader BitStore]))

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

(defn- my-sequence
  [xf coll]
  (let [rf (xf (fn [_ v] v))
        f (fn f [coll]
            (lazy-seq (loop [coll coll]
                        (if (seq coll)
                          (if-let [res (rf nil (first coll))]
                            (if (reduced? res)
                              (cons @res nil)
                              (cons res (f (rest coll))))
                            (recur (rest coll)))
                          nil))))]
    (f coll)))

(defn- get-delimited-string-bytes
  [bytes {:keys [delimiter] :as options}]
  (let [delimiter-bytes (get-string-bytes (or delimiter "") options)
        xf (comp (xf/partition (count delimiter-bytes) 1)
                 (map-indexed vector)
                 (filter #(= (second %) delimiter-bytes))
                 (map first))
        delimiter-index (first (my-sequence xf bytes))
        new-bytes (take (or delimiter-index 0)
                        bytes)]
    (byte-array new-bytes)))

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
            (if (or (nil? remaining)
                    (pos? remaining))
              (lazy-seq (try (cons (unchecked-byte (.read reader 8))
                                   (f (and remaining
                                           (- remaining 8))))
                             (catch Exception e
                               (println (clojure.stacktrace/print-throwable e))
                               nil)))
              nil))]
    (f size)))

(defn read-string
  [^BitReader reader {:keys [delimiter] :as options}]
  (let [loc (.getPosition reader)
        bytes (bytes-seq reader (assoc options :endian :be))
        ret (String. ^bytes (get-delimited-string-bytes bytes options)
                     ^String (convert-string-encoding options))]
    (.setPosition reader (+ loc (count ret) (count delimiter)))
    ret))

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
      :string (read-string reader options))))
