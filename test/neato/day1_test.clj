(ns neato.day1-test
  (:require [clojure.test :refer :all]
            [neato.day1 :refer [calculate populate-visited
                                headquarters-two headquarters-one]]))

(deftest easter-bunny-hq

  (testing "basic-distance"
    (is (= 5 (calculate "R2, L3" true))))
  (testing "turn-distance"
    (is (= 2 (calculate "R2, R2, R2" true))))
  (testing "med-distance"
    (is (= 12 (calculate "R5, L5, R5, R3" true))))
  (testing "nowhere"
    (is (= 0 (calculate "R2, R2, R2, R2" true))))
  (testing "Headquarters 1"
    (is (= 278 (headquarters-one))))
  (testing "Headquarters 2"
    (is (= 161 (headquarters-two)))))

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
