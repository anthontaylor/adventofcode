(ns neato.day4-test
  (:require [neato.day4 :refer [real-room?]]
            [clojure.test :refer :all]))

(deftest decoy

  (testing "basic-decoy"
    (is (= true (real-room? {:sector-id "123", :checksum "abxyz", :encrypted-name "aaaaabbbzyx"}))))
  (testing "basic"
    (is (= true (real-room? {:sector-id "987", :checksum "abcde", :encrypted-name "abcdefgh"}))))
  (testing "not-a-real-room"
    (is (= true (real-room? {:sector-id "404", :checksum "oarel", :encrypted-name "notarealroom"}))))
  (testing "totaly-a-real-room"
    (is (= false (real-room? {:sector-id "200", :checksum "decoy", :encrypted-name "totallyrealroom"}))))
  (testing "false-tie"
    (is (= false (real-room? {:sector-id "660", :checksum "qhiwu", :encrypted-name "hqcfqwydwfbqijyswhqiihuiuqhsx"}))))
  (testing "true-tie"
    (is (= true (real-room? {:sector-id "660", :checksum "qhiwf", :encrypted-name "hqcfqwydwfbqijyswhqiihuiuqhsx"}))))
  (testing "middle-tie-off"
    (is (= false (real-room? {:sector-id "660", :checksum "qihwf", :encrypted-name "hqcfqwydwfbqijyswhqiihuiuqhsx"})))))
