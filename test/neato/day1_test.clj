(ns neato.day1-test
  (:require [clojure.test :refer :all]
            [neato.day1 :refer [calculate populate-visited]]))

(def data "L1, R3, R1, L5, L2, L5, R4, L2, R2, R2, L2, R1, L5, R3, L4, L1, L2, R3, R5, L2, R5, L1, R2, L5, R4, R2, R2, L1, L1, R1, L3, L1, R1, L3, R5, R3, R3, L4, R4, L2, L4, R1, R1, L193, R2, L1, R54, R1, L1, R71, L4, R3, R191, R3, R2, L4, R3, R2, L2, L4, L5, R4, R1, L2, L2, L3, L2, L1, R4, R1, R5, R3, L5, R3, R4, L2, R3, L1, L3, L3, L5, L1, L3, L3, L1, R3, L3, L2, R1, L3, L1, R5, R4, R3, R2, R3, L1, L2, R4, L3, R1, L1, L1, R5, R2, R4, R5, L1, L1, R1, L2, L4, R3, L1, L3, R5, R4, R3, R3, L2, R2, L1, R4, R2, L3, L4, L2, R2, R2, L4, R3, R5, L2, R2, R4, R5, L2, L3, L2, R5, L4, L2, R3, L5, R2, L1, R1, R3, R3, L5, L2, L2, R5")

(deftest easter-bunny-hq

  (testing "basic-distance"
    (is (= 5 (calculate "R2, L3"))))
  (testing "turn-distance"
    (is (= 2 (calculate "R2, R2, R2"))))
  (testing "med-distance"
    (is (= 12 (calculate "R5, L5, R5, R3"))))
  (testing "nowhere"
    (is (= 0 (calculate "R2, R2, R2, R2"))))
  (testing "advent"
    (is (= 278 (calculate data)))))

(deftest add-visited-coordinates-test

  (is (= #{{:x 0 :y 1} {:x 0 :y 2} {:x 0 :y 3}}
         (populate-visited {:x 0 :y 0} {:x 0 :y 3})))

  (is (= #{{:x 0 :y -1} {:x 0 :y -2} {:x 0 :y -3}}
         (populate-visited {:x 0 :y 0} {:x 0 :y -3})))

  (is (= #{{:x 1 :y 0} {:x 2 :y 0} {:x 3 :y 0}}
         (populate-visited {:x 0 :y 0} {:x 3 :y 0})))

  (is (= #{{:x -1 :y 0} {:x -2 :y 0} {:x -3 :y 0}}
         (populate-visited {:x 0 :y 0} {:x -3 :y 0})))

  (is (= #{{:x 2 :y 4} {:x 3 :y 4}}
         (populate-visited {:x 1 :y 4} {:x 3 :y 4})))

  (is (= #{{:x 3 :y 3} {:x 3 :y 2} {:x 3 :y 1} {:x 3 :y 0} {:x 3 :y -1}}
         (populate-visited {:x 3 :y 4} {:x 3 :y -1}))))
