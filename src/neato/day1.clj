(ns neato.day1
  (:require [clojure.math.numeric-tower :as m]
            [clojure.set :as st]
            [debux.core :as dd]
            [clojure.string :as string]))

(defn- manhattan-distance
  [{l1 :x r1 :y} {l2 :x r2 :y}]
  (+ (m/abs (- l1 l2))
     (m/abs (- r1 r2))))

(defn- get-manhattan-distance
  [hq1? x]
  (if hq1?
    (manhattan-distance {:x 0 :y 0} x)
    (manhattan-distance {:x 0 :y 0} (:hq2 x))))

(defn- gen-visited
  [from to]
  (let [distance (m/abs (- from to))
        f (if (< to from) dec inc)]
    (->> (f from)
         (iterate f)
         (take distance))))

(defn populate-visited
  [{old-x :x old-y :y} {new-x :x new-y :y}]
  (let [x-dist (- old-x new-x)
        y-dist (- old-y new-y)
        x-values (gen-visited old-x new-x)
        y-values (gen-visited old-y new-y)]
    (if (not= old-x new-x)
      (set (map #(conj {:x % :y old-y}) x-values))
      (set (map #(conj {:x old-x :y %}) y-values)))))

(defn- add-path-history
  [[x1 y1] {x2 :x y2 :y total-visited :visited :as coord}]
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

(defn- directions->coord
  [coord {:keys [towards value]}]
  (let [values (case towards
                 \N [0 (+ value)]
                 \E [(+ value) 0]
                 \S [0 (- value)]
                 \W [(- value) 0])]
    (add-path-history values coord)))

(defn- assign-cardinal-direction
  [compass {:keys [direction value] :as data}]
  (case [direction compass]
    [\R \N] \E
    [\L \N] \W
    [\R \E] \S
    [\L \E] \N
    [\R \S] \W
    [\L \S] \E
    [\R \W] \N
    [\L \W] \S
    [\R nil]\E
    [\L nil]\W))

(defn- populate-cardinal-direction
  [new data]
  (let [compass (:towards (last new))
        direction (assign-cardinal-direction compass data)]
    (conj new (assoc data :towards direction))))

(defn- parse-instructions
  [x]
  (let [d (first x)
        v (-> x (.substring 1) read-string)]
    {:direction d :value v}))

(defn find-distance-to-hq
  [hq1? x]
  (let [coordinates  {:x 0 :y 0 :visited #{}}]
    (->> (string/split x #", ")
         (map parse-instructions)
         (reduce populate-cardinal-direction [])
         (reduce directions->coord coordinates)
         (get-manhattan-distance hq1?))))
