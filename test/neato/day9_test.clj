(ns neato.day9-test
  (:require [neato.day9 :refer :all]
            [clojure.test :refer :all]))

(deftest parse-section-test

  (testing "basic-length"
    (is (= "ADVENT" (parse-section "ADVENT"))))
  (testing "basic-parse-section"
    (is (= "ABBBBBC" (parse-section "A(1x5)BC"))))
  (testing "med-parse-section"
    (is (= "XYZXYZXYZ" (parse-section "(3x3)XYZ"))))
  (testing "harder-parse-section"
    (is (= "ABCBCDEFEFG" (parse-section "A(2x2)BCD(2x2)EFG"))))
  (testing "1-parse-section"
    (is (= "(1x3)A" (parse-section "(6x1)(1x3)A"))))
  (testing "2-parse-section"
    (is (= "X(3x3)ABC(3x3)ABCY" (parse-section "X(8x2)(3x3)ABCY")))))

(deftest decompressed-length-test

  (testing "basic-length"
    (is (= 6 (decompressed-length "ADVENT"))))
  (testing "basic-decompressed-length"
    (is (= 7 (decompressed-length "A(1x5)BC"))))
  (testing "med-decompressed-length"
    (is (= 9 (decompressed-length "(3x3)XYZ"))))
  (testing "harder-decompressed-length"
    (is (= 11 (decompressed-length "A(2x2)BCD(2x2)EFG"))))
  (testing "1-decompressed-length"
    (is (= 6 (decompressed-length "(6x1)(1x3)A"))))
  (testing "2-decompressed-length"
    (is (= 18 (decompressed-length "X(8x2)(3x3)ABCY")))))

(deftest parse-marker-test

  (testing "simple-mark"
    (is (= {:length-value 1 :multiple-value 2} (handle-marker "1x2"))))
  (testing "med-mark"
    (is (= {:length-value 4 :multiple-value 2} (handle-marker "4x2"))))
  (testing "longer-mark"
    (is (= {:length-value 400 :multiple-value 232} (handle-marker "400x232")))))
