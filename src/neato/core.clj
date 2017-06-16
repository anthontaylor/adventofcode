(ns neato.core
  (:require [clojure.math.numeric-tower :as m]
            [clojure.tools.trace :refer [trace]]
            [clojure.string :as s])
  (:gen-class))

(defn- mh-distance
  [{l1 :x r1 :y} {l2 :x r2 :y}] ;;destructure here
  (+ (m/abs (- l1 l2))
     (m/abs (- r1 r2))))

(defn- update-coordinates
  [x y coord]
  (assoc coord :x (+ x (:x coord)) :y (+ y (:y coord))))

(defn- get-direction [{:keys [direction towards value]} coord]
  (cond
    (and (= direction "R")(= towards "N")) (update-coordinates (+ value) 0 coord)
    (and (= direction "L")(= towards "N")) (update-coordinates (- value) 0 coord)
    (and (= direction "R")(= towards "E")) (update-coordinates 0 (- value) coord)
    (and (= direction "L")(= towards "E")) (update-coordinates 0 (+ value) coord)
    (and (= direction "R")(= towards "S")) (update-coordinates (- value) 0 coord)
    (and (= direction "L")(= towards "S")) (update-coordinates (+ value) 0 coord)
    (and (= direction "R")(= towards "W")) (update-coordinates 0 (+ value) coord)
    (and (= direction "L")(= towards "W")) (update-coordinates 0 (- value) coord)
    :else
    "Coordinates Wrong!"))

(defn calc-coordinates [data]
  (loop [old data
         coordinates {:x 0 :y 0}]
    (trace "calc-coordinates" old)
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
    (assoc data :towards "Something went wrong")))

(defn populate-facing [data]
  (loop [old data
         new []
         compass "N"] ;;may need to create as list because of conj/compass
    (let [point (assign-facing (first old) compass)
          new-compass (:towards point)] ;;I'm not assigning the compass
      (trace "HEY" new-compass)
      (if (empty? old)
        new
        (recur
         (vec (rest old))
         (conj new point)
         new-compass)))))

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
    #_(calc-coordinates x)
    #_(mh-distance {:x 0 :y 0} x)))

(defn -main
  "Run Code"
  [& args]
  (println "Hello, World!"))
