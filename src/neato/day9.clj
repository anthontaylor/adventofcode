(ns neato.day9
  (:require [clojure.string :as s]
            [clojure.walk :as w]
            [clojure.tools.trace :refer [trace]]
            [neato.shared :refer [parse-dataset]]))

(defn handle-marker
  [data]
  (let [before (first (s/split data #"\("))
        marker (last (re-find  #"\((.*?)\)" data))
        after (second (s/split data #"\)" 2))
        vals {}]
    (if marker
      (let [values (s/split marker #"x")
            length (Integer. (first values))
            multiple (Integer. (last values))]
        (assoc vals :length length :multiple multiple :before before :after after :marker marker))
      (assoc vals :length 1 :multiple 1 :before before :after after :marker marker))))

(defn leftovers
  [{:keys [after length marker]}]
  (if marker
    (subs after length)
    nil))

(defn needs-parsing
  [data {:keys [after length multiple marker]}]
  (if marker
    (s/join (repeat multiple (subs after 0 length)))
    data))

(defn decompress-one
  [x]
  (loop [data x
         new ""]
    (let [values (handle-marker data)
          add-to-result (needs-parsing data values)
          leftover (leftovers values)]
      (if (empty? leftover)
        (if (:marker values)
          (count (str (:before values) new add-to-result))
          (count (str new add-to-result)))
        (recur
         leftover
         (s/join [new (:before values) add-to-result]))))))

(defn decompress-two
  [x]
  (loop [data x
         counter 0]
    (let [values (handle-marker data)
          needs-parsing (needs-parsing data values)
          leftover (leftovers values)]
      (if (:marker values)
        (recur (s/join [needs-parsing leftover])
               (+ counter (count (:before values))))
        (+ counter (count (s/join [needs-parsing leftover])))))))

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
