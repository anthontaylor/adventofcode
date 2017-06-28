(ns neato.day2-test
  (:require [neato.day2 :as day2]
            [clojure.test :refer :all]))

(deftest bathroom-code

  (is (= 1 (day2/access-bathroom "ULL" 0)))
  (is (= 9 (day2/access-bathroom "RRDDD" 1)))
  (is (= 8 (day2/access-bathroom "LURDL" 9)))
  (is (= 5 (day2/access-bathroom "UUUUD" 8))))
