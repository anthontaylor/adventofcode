(ns neato.day4
  (:require [clojure.string :as s]
            [clojure.walk :as w]
            [clojure.tools.trace :refer [trace]]
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

(defn parse-string
  [x]
  (-> {}
      (assoc :sector-id (re-find  #"\d+" x))
      (assoc :checksum (last (re-find  #"\[(.*?)\]" x)))
      (assoc :encrypted-name (s/replace (re-find  #"[^0-9]*" x) #"-" ""))))

(defn sum-of-IDs
  [coll]
  (->> coll
   (mapv real-room?)
   (reduce +))) ;;maybe just display first portion of result here

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
       (mapv decrypt-room)
       (remove nil?)
       #_(sum-of-IDs))) ;;split up part one and part two answers to display nicely

(defn decrypt-char
  [letter index]
  (let [alphabet ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o"
                  "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"]
        new-index (.indexOf alphabet (str letter))]
    (if (= -1 new-index)
      "-"
      (nth alphabet (mod (+ new-index index) 26) "Not here"))))

(defn decrypt-room
  [{e :encrypted-name id :sector-id :as room}]
  (if (not= 0 (real-room? room))
    (let [cipher-num (mod (Integer. id) 26)
          name-coll (vec (seq e))]
      (assoc {} :name (s/join "" (mapv #(decrypt-char (str %) cipher-num) name-coll))
             :sector-id id))
    nil))
