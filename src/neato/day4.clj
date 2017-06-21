(ns neato.day4
  (:require [clojure.string :as st]
            [clojure.walk :as w]
            [clojure.tools.trace :refer [trace]]
            [neato.shared :refer [parse-dataset]]))

(def id-regex #"\d+")
(def checksum-regex #"\[(.*?)\]")
(def name-regex #"[^0-9]*")

(defn sort-map
  [coll]
  (into (sorted-map-by
         (fn [k1 k2]
           (compare [(k2 coll) k1]
                    [(k1 coll) k2]))) coll))

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

(defn real?
  [{e :encrypted-name c :checksum id :sector-id}]
  (if (->> (parse-checksum e c)
           (mapv #(-> % first name))
           (st/join "")
           (compare c)
           (= 0))
    (Integer. id)
    0))

(defn parse-string
  [x]
  (-> {}
      (assoc :sector-id (re-find id-regex x))
      (assoc :checksum (last (re-find checksum-regex x)))
      (assoc :encrypted-name (st/replace (re-find name-regex x) #"-" ""))))

(defn sum-of-IDs
  [coll]
  (->> coll
   (mapv real?)
   (reduce +)))

(defn decrypt
  [letter index alpha]
  (let [a-index (.indexOf alpha (str letter))
        length (count alpha)]
    (when (not= -1 a-index)
      (nth alpha (mod (+ a-index index) length) "Not here"))))

(defn decrypt-room
  [{e :encrypted-name id :sector-id :as room}]
  (if (not= 0 (real? room))
    (let [shift (mod (Integer. id) 26)
          chars (vec (seq e))
          alpha ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
                 "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"]]
      (assoc {} :name (->> chars
                           (mapv #(decrypt % shift alpha))
                           (st/join ""))
                :sector-id id))
    nil))

(defn decoy-two
  []
  (->> (parse-dataset "day4data.txt")
       (mapv parse-string)
       (mapv decrypt-room)
       (remove nil?)))

(defn decoy-one
  []
  (->> (parse-dataset "day4data.txt")
       (mapv parse-string)
       (sum-of-IDs)))
