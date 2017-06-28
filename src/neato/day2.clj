(ns neato.day2
  (:require [clojure.tools.trace :refer [trace]]
            [clojure.string :as string]
            [neato.shared :refer [parse-dataset]]
            [aprint.core :refer :all]
            [clojure.zip :as zip]))


(defrecord Codes
    [code-1
     code-2
     code-3
     code-4])

(defrecord Direction
    [current-number
     direction
     number-result])

(defn get-number [val direction]
  (cond
    (and (= val 1) (= direction \R)) 2
    (and (= val 1) (= direction \D)) 4
    (and (= val 2) (= direction \L)) 1
    (and (= val 2) (= direction \D)) 5
    (and (= val 2) (= direction \R)) 3
    (and (= val 3) (= direction \L)) 2
    (and (= val 3) (= direction \D)) 6
    (and (= val 4) (= direction \R)) 5
    (and (= val 4) (= direction \U)) 1
    (and (= val 4) (= direction \D)) 7
    (and (= val 5) (= direction \R)) 6
    (and (= val 5) (= direction \L)) 4
    (and (= val 5) (= direction \U)) 2
    (and (= val 5) (= direction \D)) 8
    (and (= val 6) (= direction \U)) 3
    (and (= val 6) (= direction \D)) 9
    (and (= val 6) (= direction \L)) 5
    (and (= val 7) (= direction \U)) 4
    (and (= val 7) (= direction \R)) 8
    (and (= val 8) (= direction \R)) 9
    (and (= val 8) (= direction \L)) 7
    (and (= val 8) (= direction \U)) 5
    (and (= val 9) (= direction \U)) 6
    (and (= val 9) (= direction \L)) 8
    :else
    nil))

#_(defn parse-code
  [data]
  (->> data
       (first)
       (:code)
       (map #(greeting %))))

(defn populate-directions
  [data]
  (->> data
       (map #(assoc {} :code %))))

(defn access-bathroom
  [data starting-point]
  (-> data
      populate-directions))

(defn bathroom-code
  []
  (let [file "day2data.txt"]
    (-> file
        parse-dataset
        access-bathroom 0)))
