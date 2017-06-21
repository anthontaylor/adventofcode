(ns neato.day9
  (:require [clojure.string :as s]
            [clojure.walk :as w]
            [clojure.tools.trace :refer [trace]]
            [neato.shared :refer [parse-dataset]]))

(defn handle-marker
  [x]
  (if x
   (as-> x marker
     (s/split marker #"x")
     (assoc {} :length (Integer. (first marker))
            :multiple (Integer. (last marker))))
   (assoc {} :length 1 :multiple 1)))

(defn decompress-one
  [data]
  (loop [x data
         new ""]
    (let [before (first (s/split x #"\("))
          marker (last (re-find  #"\((.*?)\)" x))
          after (second (s/split x #"\)" 2))
          length (:length (handle-marker marker))
          multiple (:multiple (handle-marker marker))
          add-to-result (if marker
                          (s/join (repeat multiple (subs after 0 length)))
                          x)
          remaining-data (if marker
                           (subs after length)
                           nil)]
      (if (empty? remaining-data)
        (if marker
          (count (str before new add-to-result))
          (count (str new add-to-result)))
        (recur remaining-data (str new before add-to-result))))))

(defn decompress-two
  [data]
  (loop [x data
         counter 0]
    (let [before (first (s/split x #"\("))
          marker (last (re-find  #"\((.*?)\)" x))
          after (second (s/split x #"\)" 2))
          length (:length (handle-marker marker))
          multiple (:multiple (handle-marker marker))
          needs-parsing (if marker
                                  (s/join (vec (repeat multiple (subs after 0 length))))
                                  x)
          remaining-data (if marker
                           (subs after length)
                           nil)]
      (if marker
        (recur (str needs-parsing remaining-data) (+ counter (count before)))
        (+ counter (count (s/join [needs-parsing remaining-data])))))))

(defn decompressed-length-one
  []
  (let [file "day9data.txt"]
    (-> file
        parse-dataset
        first
        decompress-one)))

(defn decompressed-length-two
  []
  (let [file "day9data.txt"]
    (-> file
        parse-dataset
        first
        decompress-two)))
