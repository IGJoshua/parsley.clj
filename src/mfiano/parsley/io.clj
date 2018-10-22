(ns mfiano.parsley.io
  (:require [clojure.java.io :as io])
  (:import (com.tomgibara.bits Bits)))

(defn open-file
  [spec path]
  (assert (.exists (io/file path))
          (format "File %s does not exist." path))
  (let [stream (io/input-stream path)
        reader (Bits/readerFrom stream)]
    {:path path
     :stream stream
     :reader reader
     :spec spec}))
