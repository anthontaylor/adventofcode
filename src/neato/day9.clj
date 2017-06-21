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
          (str before new add-to-result)
          (str new add-to-result))
        (recur remaining-data (str new before add-to-result))))))

(defn parse-section-two
  [data]
  (loop [x data
         counter 0]
    (let [before (first (s/split x #"\("))
          marker (last (re-find  #"\((.*?)\)" x))
          after (second (s/split x #"\)" 2))
          length (:length-value (handle-marker marker))
          multiple (:multiple-value (handle-marker marker))
          needs-parsing (if marker
                          (s/join (vec (repeat multiple (subs after 0 length))))
                          x)
          remaining-data (if marker
                           (subs after length)
                           nil)]
      #_(trace "before" before)
      #_(trace "marker" marker)
      #_(trace "needs-parsing" needs-parsing)
      #_(trace "remaining-data" remaining-data)
      #_(trace "after" after)
      #_(trace "length" length)
      #_(trace "multiple" multiple)
      #_(trace "counter" counter)
      (if marker
        (recur (str needs-parsing remaining-data) (+ counter (count before)))
        (+ counter (count (str needs-parsing remaining-data))))))) ;;add to the count each time

(defn decompressed-length-two
  [data]
  (parse-section-two data))

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
        (decompressed-length-two)))) ;;;this is only the first one!
