(ns neato.day4
  (:require [clojure.string :as s]
            [clojure.walk :as w]
            [clojure.java.io :as io]))

(defn sort-map
  [x]
  (into (sorted-map-by
         (fn [key1 key2]
           (compare [(key2 x) key1]
                    [(key1 x) key2]))) x))

(defn keywordize-map [my-map]
  (w/keywordize-keys
   (into {} (for [[k v] my-map]
              [(str k) v]))))

(defn parse-checksum
  [e c]
  (->> e
    (frequencies)
    (keywordize-map)
    (sort-map)
    (take (count c))))

(defn real-room?
  [{e :encrypted-name c :checksum id :sector-id}]
  (if (->> (parse-checksum e c)
        (mapv #(-> % first name))
        (s/join "")
        (compare c)
        (= 0))
    (Integer. id)
    0))

(defn sum-of-IDs
  [coll]
  (reduce + (mapv real-room? coll)))

(defn parse-string
 [x]
  (-> {}
      (assoc :sector-id (re-find  #"\d+" x))
      (assoc :checksum (last (re-find  #"\[(.*?)\]" x)))
      (assoc :encrypted-name (s/replace (re-find  #"[^0-9]*" x) #"-" ""))))

(defn parse-dataset
  []
  (line-seq (io/reader (io/resource "day4data.txt"))))

(defn parse-decoy-data
  []
  (->> (parse-dataset)
       (mapv parse-string)
       (sum-of-IDs)))
