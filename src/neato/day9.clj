(ns neato.day9
  (:require [clojure.string :as s]
            [clojure.walk :as w]
            [clojure.tools.trace :refer [trace]]
            [clojure.java.io :as io]))

(defn handle-marker
  [x]
  (if x
   (as-> x marker
     (s/split marker #"x")
     (assoc {} :length-value (Integer. (first marker))
            :multiple-value (Integer. (last marker))))
   (assoc {} :length-value 1 :multiple-value 1)))

(defn parse-section
  [data]
  (loop [x data
         new ""]
    (let [before (first (s/split x #"\("))
          marker (last (re-find  #"\((.*?)\)" x))
          after (second (s/split x #"\)" 2))
          length-value (:length-value (handle-marker marker))
          multiple-value (:multiple-value (handle-marker marker))
          add-to-result (if marker
                          (s/join (repeat multiple-value (subs after 0 length-value)))
                          x)
          remaining-data (if marker
                          (subs after length-value)
                          nil)]
      (if (empty? remaining-data)
        (if marker
          (str before add-to-result new)
          (str add-to-result new))
        (recur remaining-data (str before add-to-result new))))))

(defn decompressed-length
  [x]
  (count (parse-section x)))

(defn parse-dataset
  []
  (let [file "day9data.txt"]
    (-> file
        (io/resource)
        (io/reader)
        (line-seq)
        (first)
        (decompressed-length))))
