(ns neato.day1
  (:require [clojure.math.numeric-tower :as m]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [clojure.tools.trace :refer [trace]]
            [clojure.set :as st]
            [clojure.string :as s]
            [clojure.java.io :as io]))

(defn gen-values
  [from to]
  (let [distance (m/abs (- from to))]
    (if (< to from)
      (take distance (iterate dec (- from 1))) ;;put a dec and inc within the - from 1 instead here
      (take distance (iterate inc (+ from 1))))))

(defn populate-visited
  [{oldx :x oldy :y} {newx :x newy :y}]
  (let [x-dist (- oldx newx)
        y-dist (- oldy newy)
        x-values (gen-values oldx newx)
        y-values (gen-values oldy newy)]
    (if (not= oldx newx)
      (into #{} (map #(conj {:x % :y oldy}) x-values))
      (into #{} (map #(conj {:x oldx :y %}) y-values)))))

(defn- mh-distance
  [{l1 :x r1 :y} {l2 :x r2 :y}]
  (+ (m/abs (- l1 l2))
     (m/abs (- r1 r2))))

(defn- update-coordinates
  [x1 y1 {x2 :x y2 :y total-visited :visited :as coord}]
  (let [new-visited (populate-visited {:x x2 :y y2} {:x (+ x1 x2) :y (+ y1 y2)})
        visited (st/union new-visited total-visited)]

    #_(trace "total-visited" total-visited)
    (if-let [hq (some new-visited total-visited)]
      (if (empty? (:new-hq coord))
        (assoc coord :x (+ x1 x2) :y (+ y1 y2) :visited visited :new-hq hq)
        (assoc coord :x (+ x1 x2) :y (+ y1 y2) :visited visited))
      (assoc coord :x (+ x1 x2) :y (+ y1 y2) :visited visited))))

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
         coordinates {:x 0 :y 0 :visited #{}}]
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
    (str "First Headquarters: "(mh-distance {:x 0 :y 0} x) " Second Headquarters: "
         (mh-distance {:x 0 :y 0} (:new-hq x))))) ;;fix testing since second headquarters could be nil

(defn parse-dataset
  []
  (let [file "day1data.txt"]
    (-> file
        (io/resource)
        (io/reader)
        (line-seq)
        (first)
        (calculate))))
