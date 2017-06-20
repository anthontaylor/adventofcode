(ns neato.day1
  (:require [clojure.math.numeric-tower :as m]
            [clojure.tools.trace :refer [trace]]
            [clojure.string :as s]
            [clojure.java.io :as io]))

(defn- mh-distance
  [{l1 :x r1 :y} {l2 :x r2 :y}]
  (+ (m/abs (- l1 l2))
     (m/abs (- r1 r2))))

(defn- update-coordinates
  [x1 y1 {x2 :x y2 :y :as coord}]
  (assoc coord :x (+ x1 x2) :y (+ y1 y2)))

(defn- get-direction [{:keys [towards value]} coord]
  (cond
    (= towards "N") (update-coordinates 0 (+ value) coord)
    (= towards "E") (update-coordinates (+ value) 0 coord)
    (= towards "S") (update-coordinates 0 (- value) coord)
    (= towards "W") (update-coordinates (- value) 0 coord)
    :else
    "Coordinates Wrong!"))

(defn calc-coordinates [data]
  (loop [old data
         coordinates {:x 0 :y 0}]
    (if (empty? old)
      coordinates
      (recur
       (vec (rest old))
       (get-direction (first old) coordinates)))))

(defn assign-facing [{:keys [direction value] :as data} facing]
  (cond
    (and (= direction "R")(= facing "N")) (assoc data :towards "E")
    (and (= direction "L")(= facing "N")) (assoc data :towards "W")
    (and (= direction "R")(= facing "E")) (assoc data :towards "S")
    (and (= direction "L")(= facing "E")) (assoc data :towards "N")
    (and (= direction "R")(= facing "S")) (assoc data :towards "W")
    (and (= direction "L")(= facing "S")) (assoc data :towards "E")
    (and (= direction "R")(= facing "W")) (assoc data :towards "N")
    (and (= direction "L")(= facing "W")) (assoc data :towards "S")
    :else
    (assoc data :towards "Something went wrong!")))

(defn populate-facing [data]
  (loop [old data
         new []
         compass "N"]
    (let [point (-> old first (assign-facing compass))]
      (if (empty? old)
        new
        (recur
         (vec (rest old))
         (conj new point)
         (:towards point))))))

(defn- populate-map
  [x]
  (let [d (-> x first str)
        v (->> x (re-find  #"\d+") Integer.)
        point {}]
    (if (= "L" d)
      (assoc point :direction d :value v)
      (assoc point :direction d :value v))))

(defn calculate
  [data]
  (as-> data x
    (s/split x #", ")
    (mapv populate-map x)
    (populate-facing x)
    (calc-coordinates x)
    (mh-distance {:x 0 :y 0} x)))

(defn parse-dataset
  []
  (let [file "day1data.txt"]
    (-> file
        (io/resource)
        (io/reader)
        (line-seq)
        (first)
        (calculate))))
