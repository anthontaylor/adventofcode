(ns neato.core-test
  (:require [clojure.test :refer :all]
            [neato.core :refer :all]))

(deftest basic-distance
  (is (= 5 (calculate "R2, L3"))))

(deftest turn-distance
  (is (= 2 (calculate "R2, R2, R2"))))

(deftest med-distance
  (is (= 12 (calculate "R5, L5, R5, R3"))))

(deftest nowhere-distance
  (is (= 0 (calculate "R2, R2, R2, R2"))))

(deftest calc-coords
  (is (= {:x 10 :y 2} (calc-coordinates [{:direction "R", :value 5, :facing "N"} {:direction "L", :value 5, :facing "E"} {:direction "R", :value 5, :facing "N"} {:direction "R", :value 3, :facing "E"}]))))

(deftest calc-another
  (is (= {:x 2 :y 0} (calc-coordinates [{:direction "R", :value 1, :facing "N"} {:direction "L", :value 2, :facing "E"} {:direction "R", :value 1, :facing "N"} {:direction "R", :value 2, :facing "E"}]))))

(deftest another
  (is (= {:x 2 :y 2} (calc-coordinates [{:direction "R", :value 1, :facing "N"} {:direction "L", :value 2, :facing "E"} {:direction "R", :value 1, :facing "N"}]))))

(deftest baby
  (is (= {:x 1 :y 2} (calc-coordinates [{:direction "R", :value 1, :facing "N"} {:direction "L", :value 2, :facing "E"}]))))

(deftest facing
  (is (= {:direction "R" :value 1 :facing "N"} (assign-facing {:direction "R" :value 1} nil))))

(deftest facing-1
  (is (= {:direction "R" :value 1 :facing "E"} (assign-facing {:direction "R" :value 1} "N"))))

(deftest facing-2
  (is (= {:direction "L" :value 1 :facing "N"} (assign-facing {:direction "L" :value 1} "E"))))

[{:direction "R", :value 5} {:direction "L", :value 5} {:direction "R", :value 5} {:direction "R", :value 3}]
(deftest pop-facing
  (is (= [{:direction "R", :value 1, :facing "N"} {:direction "L", :value 2, :facing "E"} {:direction "R", :value 1, :facing "N"}]
         (populate-facing [{:direction "R", :value 5} {:direction "L", :value 5} {:direction "R", :value 5} {:direction "R", :value 3}]))))
