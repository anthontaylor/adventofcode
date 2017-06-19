(ns neato.day4
  (:require [clojure.string :as s]
            [clojure.walk :as w]
            [clojure.java.io :as io]))

(defn sort-map
  [coll]
  (into (sorted-map-by
         (fn [key1 key2]
           (compare [(key2 coll) key1]
                    [(key1 coll) key2]))) coll))

(defn keywordize-map [coll]
  (w/keywordize-keys
   (into {} (for [[k v] coll]
              [(str k) v]))))

(defn parse-checksum
  [encrypted-name checksum]
  (->> encrypted-name
    (frequencies)
    (keywordize-map)
    (sort-map)
    (take (count checksum))))

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
  (->> coll
   (mapv real-room?)
   (reduce +)))

(defn parse-string
 [x]
  (-> {}
      (assoc :sector-id (re-find  #"\d+" x))
      (assoc :checksum (last (re-find  #"\[(.*?)\]" x)))
      (assoc :encrypted-name (s/replace (re-find  #"[^0-9]*" x) #"-" ""))))

(defn parse-dataset
  []
  (let [file "day4data.txt"]
    (-> file
        (io/resource)
        (io/reader)
        (line-seq))))

(defn parse-decoy-data
  []
  (->> (parse-dataset)
       (mapv parse-string)
       (sum-of-IDs)))