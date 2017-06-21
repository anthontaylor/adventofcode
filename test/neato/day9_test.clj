(ns neato.day9-test
  (:require [neato.day9 :refer :all]
            [clojure.test :refer :all]))

(deftest decompress-one-test

  (testing "basic-length"
    (is (= 6 (decompress-one "ADVENT"))))
  (testing "basic-decompress-one"
    (is (= 7 (decompress-one "A(1x5)BC"))))
  (testing "med-decompress-one"
    (is (= 9 (decompress-one "(3x3)XYZ"))))
  (testing "harder-decompress-one"
    (is (= 11 (decompress-one "A(2x2)BCD(2x2)EFG"))))
  (testing "1-decompress-one"
    (is (= 6 (decompress-one "(6x1)(1x3)A"))))
  (testing "2-decompress-one"
    (is (= 18 (decompress-one "X(8x2)(3x3)ABCY")))))

(deftest handle-marker-test

  (testing "simple-mark"
    (is (= {:length 1 :multiple 2} (handle-marker "1x2"))))
  (testing "med-mark"
    (is (= {:length 4 :multiple 2} (handle-marker "4x2"))))
  (testing "longer-mark"
    (is (= {:length 400 :multiple 232} (handle-marker "400x232")))))

(deftest decompress-two-test

  (is (= 6 (decompress-two "ADVENT")))
  (is (= 7 (decompress-two "A(1x5)BC")))
  (is (= 9 (decompress-two "(3x3)XYZ")))
  (is (= 20 (decompress-two "X(8x2)(3x3)ABCY")))
  (is (= 241920 (decompress-two "(27x12)(20x12)(13x14)(7x10)(1x12)A")))
  (is (= 445 (decompress-two "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"))))

(deftest data

  (is (= 74532 (decompressed-length-one))))
