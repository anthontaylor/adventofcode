(ns neato.day1
  (:require [clojure.math.numeric-tower :as m]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [clojure.tools.trace :refer [trace]]
            [clojure.set :as st]
            [clojure.string :as string]))

(defn- gen-visited
  [from to]
  (let [distance (m/abs (- from to))]
    (if (< to from)
      (take distance (iterate dec (dec from)))
      (take distance (iterate inc (inc from))))))

(defn populate-visited
  [{old-x :x old-y :y} {new-x :x new-y :y}]
  (let [x-dist (- old-x new-x)
        y-dist (- old-y new-y)
        x-values (gen-visited old-x new-x)
        y-values (gen-visited old-y new-y)]
    (if (not= old-x new-x)
      (into #{} (map #(conj {:x % :y old-y}) x-values))
      (into #{} (map #(conj {:x old-x :y %}) y-values)))))

(defn- manhattan-distance
  [{l1 :x r1 :y} {l2 :x r2 :y}]
  (+ (m/abs (- l1 l2))
     (m/abs (- r1 r2))))

(defn- add-path-history
  [x1 y1 {x2 :x y2 :y total-visited :visited :as coord}]
  (let [new-x (+ x1 x2)
        new-y (+ y1 y2)
        new-visited (populate-visited {:x x2 :y y2} {:x new-x :y new-y})
        visited (st/union new-visited total-visited)
        coord (assoc coord :x new-x :y new-y :visited visited)]
    (if-let [hq (some new-visited total-visited)]
      (if (empty? (:hq2 coord))
        (assoc coord :hq2 hq)
        coord)
      coord)))

(defn- get-direction [{:keys [towards value]} coord]
  (cond
    (= towards "N") (add-path-history 0 (+ value) coord)
    (= towards "E") (add-path-history (+ value) 0 coord)
    (= towards "S") (add-path-history 0 (- value) coord)
    (= towards "W") (add-path-history (- value) 0 coord)
    :else
    (throw (Exception. "Error when getting direction"))))

(defn calc-coordinates [data]
  (loop [old data
         coordinates {:x 0 :y 0 :visited #{}}]
    (if (empty? old)
      coordinates
      (recur
       (vec (rest old))
       (get-direction (first old) coordinates)))))

(defn- assign-cardinal-direction [{:keys [direction value] :as data} facing]
  (cond
    (and (= direction "R")(= facing "N")) (assoc data :towards "E")
    (and (= direction "L")(= facing "N")) (assoc data :towards "W")
    (and (= direction "R")(= facing "E")) (assoc data :towards "S")
    (and (= direction "L")(= facing "E")) (assoc data :towards "N")
    (and (= direction "R")(= facing "S")) (assoc data :towards "W")
    (and (= direction "L")(= facing "S")) (assoc data :towards "E")
    (and (= direction "R")(= facing "W")) (assoc data :towards "N")
    (and (= direction "L")(= facing "W")) (assoc data :towards "S")))

(defn- populate-cardinal-direction [data]
  (loop [old data
         new []
         compass "N"]
    (let [point (-> old first (assign-cardinal-direction compass))]
      (if (empty? old)
        new
        (recur
         (vec (rest old))
         (conj new point)
         (:towards point))))))

(defn- parse-instructions
  [x]
  (let [d (-> x first str)
        v (-> x (.substring 1) read-string)]
    {:direction d :value v}))

(defn get-manhattan-distance
  [hq1? x]
  (if hq1?
    (manhattan-distance {:x 0 :y 0} x)
    (manhattan-distance {:x 0 :y 0} (:hq2 x))))

(defn find-distance-to-hq
  [hq1? x]
  (->> (string/split x #", ")
    (mapv parse-instructions)
    populate-cardinal-direction
    calc-coordinates
    (get-manhattan-distance hq1?)))
