(ns neato.day9
  (:require [clojure.string :as s]
            [clojure.walk :as w]
            [clojure.java.io :as io]))

;;ignore markers if within marker territory

;;cut off input as I go through it.

;;Use clojure subs like (subs "Clojure" parsed-length)
;;                                (subs "Clojure" 1 3)

;;Have two maps. Once that I add to that will be the length count in the end,
;;and the other which I go through and parse and cut off as I go.

;;parse-section until you find a marker
;;Get the length and multiple value of that
;;Cut off the the old map up to that length
;;Do what you need to do with the section data
;;and append it to the new map

(defn handle-marker
  "retrieves a marker string ex:3x3 and returns
  a map of respective values"
  [x]
  (as-> x marker
    (s/split marker #"x")
    (assoc {} :length-value (Integer. (first marker))
           :multiple-value (Integer. (last marker)))))


;;possibly just loop this until there is no more remaining!
(defn parse-section
  [x]
  (let [before (first (s/split x #"\(")) ;;clean all of this shiz!
        marker (last (re-find  #"\((.*?)\)" x))
        after (second (s/split x #"\)" 2))
        length-value (:length-value (handle-marker marker))
        multiple-value (:multiple-value (handle-marker marker))
        add-to-result (s/join (repeat multiple-value (subs after 0 length-value)))
        remaining-map (subs after length-value)]
    (str
     " Original: " x
     " Before: " before
     " Marker: " marker
     " After: " after
     " Length: " length-value
     " Multiple: " multiple-value
     " result: " add-to-result
     " remaining-map: " remaining-map)))

(defn decompressed-length
  [x])

(defn parse-dataset
  []
  (let [file "day9data.txt"]
    (-> file
        (io/resource)
        (io/reader)
        (line-seq))))