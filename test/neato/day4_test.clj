(ns neato.day4-test
  (:require [neato.day4 :refer [real? decrypt-room decoy-one]]
            [clojure.test :refer :all]))

(deftest decoy

  (testing "basic-decoy"
    (is (= 123 (real? {:sector-id "123" :checksum "abxyz"
                       :encrypted-name "aaaaabbbzyx"}))))
  (testing "basic"
    (is (= 987 (real? {:sector-id "987" :checksum "abcde"
                       :encrypted-name "abcdefgh"}))))
  (testing "not-a-real-room"
    (is (= 404 (real? {:sector-id "404" :checksum "oarel"
                       :encrypted-name "notarealroom"}))))
  (testing "totaly-a-real-room"
    (is (= 0 (real? {:sector-id "200" :checksum "decoy"
                     :encrypted-name "totallyrealroom"}))))
  (testing "false-tie"
    (is (= 0 (real? {:sector-id "660" :checksum "qhiwu"
                     :encrypted-name "hqcfqwydwfbqijyswhqiihuiuqhsx"}))))
  (testing "true-tie"
    (is (= 660 (real? {:sector-id "660" :checksum "qhiwf"
                       :encrypted-name "hqcfqwydwfbqijyswhqiihuiuqhsx"}))))
  (testing "middle-tie-off"
    (is (= 0 (real? {:sector-id "660" :checksum "qihwf"
                     :encrypted-name "hqcfqwydwfbqijyswhqiihuiuqhsx"})))))

(deftest decrypt-name

  (is (= {:name "veryencryptedname" :sector-id "343"}
         (decrypt-room {:sector-id "343"
                        :checksum "zimth"
                        :encrypted-name "qzmtzixmtkozyivhz"}))))

(deftest parse-data

  (is (= 245102 (decoy-one))))
