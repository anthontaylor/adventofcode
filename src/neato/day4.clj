(ns neato.day4
  (:require [clojure.string :as st]
            [clojure.walk :as w]
            [clojure.tools.trace :refer [trace]]
            [neato.shared :refer [parse-dataset]]))

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
       (take (count checksum))))

;; `sum-of-ids` could be `(filter is-real?) (map :sector-id) (reduce +)` to describe better your intention and what's going on....

(defn retrieve-real-room-checksums
  [{e :encrypted-name c :checksum id :sector-id}]
  (if (->> (parse-checksum e c)
           (mapv #(-> % first name))
           (st/join "")
           (compare c)
           (= 0))
    (Integer. id)
    0))

(defn string->room
  [x]
  (let [id-regex  #"\d+"
        checksum-regex #"\[(.*?)\]"
        name-regex #"[^0-9]*"]
    {:sector-id (re-find id-regex x)
     :checksum (last (re-find checksum-regex x))
     :encrypted-name (st/replace (re-find name-regex x) #"-" "")}))

(defn sum-of-IDs
  [coll]
  (->> coll
   (mapv retrieve-real-room-checksums)
   (reduce +)))

(defn decrypt
  [letter index alpha]
  (let [a-index (.indexOf alpha (str letter))
        length (count alpha)]
    (when (not= -1 a-index)
      (nth alpha (mod (+ a-index index) length) "Not here"))))

(defn decrypt-room
  [{e :encrypted-name id :sector-id :as room}]
  (if (not= 0 (retrieve-real-room-checksums room))
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
       (mapv (comp string->room decrypt-room))
       (remove nil?)))

(defn decoy-one
  []
  (->> (parse-dataset "day4data.txt")
       (mapv string->room)
       (sum-of-IDs)))
