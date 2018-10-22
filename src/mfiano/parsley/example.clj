(ns mfiano.parsley.example
  (:require [mfiano.parsley.data-types :as d]
            [mfiano.parsley.io :as io]
            [mfiano.parsley.transformers :as tr]))

(def spec
  {:desc "Free Lossless Audio Codec"
   :url "https://xiph.org/flac/format.html"
   :endian :be
   :encoding :ascii})

(defn doit
  [spec path]
  (let [file-map (io/open-file spec path)]
    [(d/read :string file-map :size 32)
     (d/read :bits file-map :size 1)
     (d/read :bits file-map :size 7)
     (d/read :uint file-map :size 24)
     (d/read :uint file-map :size 16)
     (d/read :uint file-map :size 16)
     (d/read :uint file-map :size 24)
     (d/read :uint file-map :size 24)
     (d/read :bits file-map :size 20)
     (d/read :bits file-map :size 3)
     (d/read :bits file-map :size 5)
     (d/read :bits file-map :size 36)
     (tr/bytes->hex-string (d/read :bytes file-map :size 128))]))
