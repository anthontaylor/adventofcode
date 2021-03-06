(ns neato.day4
  (:require [clojure.string :as st]
            [clojure.walk :as w]
            [clojure.tools.trace :refer [trace]]
            [neato.shared :refer [parse-dataset]]
            [com.walmartlabs.datascope :as ds]))

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
       frequencies
       keywordize-map
       sort-map
       (take 5)))

(defn is-real?
  [{e :encrypted-name c :checksum id :sector-id}]
  (->> (parse-checksum e c)
       (map #(-> % first name))
       (st/join "")
       (compare c)
       (= 0)))

(defn string->room
  [x]
  (let [id-regex  #"\d+"
        checksum-regex #"\[(.*?)\]"
        name-regex #"[^0-9]*"]
    {:sector-id (Integer. (re-find id-regex x))
     :checksum (last (re-find checksum-regex x))
     :encrypted-name (st/replace (re-find name-regex x) #"-" "")}))

(defn sum-of-IDs
  [coll]
  (->> coll
   (filter is-real?)
   (map :sector-id)
   (reduce +)))

(defn decrypt
  [letter index alpha]
  (let [a-index (.indexOf alpha (str letter))
        length (count alpha)]
    (when (not= -1 a-index)
      (nth alpha (mod (+ a-index index) length) "Not here"))))

(defn decrypt-room
  [{e :encrypted-name id :sector-id :as room}]
  (when (not= 0 (is-real? room))
    (let [shift (mod (Integer. id) 26)
          chars (vec (seq e))
          alpha (map #(str (char %)) (range 97 123))]
      {:name (->> chars (map #(decrypt % shift alpha)) (st/join "")) :sector-id id})))

(defn decoy-two
  []
  (->> (parse-dataset "day4data.txt")
       (map (comp string->room decrypt-room))
       (remove nil?)))

(defn decoy-one
  []
  (->> (parse-dataset "day4data.txt")
       (map string->room)
       (sum-of-IDs)))
