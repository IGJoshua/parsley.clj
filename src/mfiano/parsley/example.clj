(ns mfiano.parsley.example
  (:require [mfiano.parsley.data-types :as d]
            [mfiano.parsley.io :as io]))

(def spec
  {:desc "Free Lossless Audio Codec"
   :url "https://xiph.org/flac/format.html"
   :endian :be
   :encoding :ascii})

(defn doit
  [spec path]
  (let [file-map (io/open-file spec path)]
    (d/read :string file-map :size 40 :encoding :utf8 :delimiter "\0" :endian :be)))
