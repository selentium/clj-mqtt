(ns clj-mqtt.core-test
  (:require [clojure.test :refer :all]
            [clj-mqtt.core :refer :all]))


(deftest test-integer->variant
  (testing "Integer to VBI conversion"
    (is (= [32] (integer->variant 32)))
    (is (= [128 1] (integer->variant 128)))
    (is (= [128 128 1] (integer->variant 16384)))
    (is (= [128 128 128 1] (integer->variant 2097152)))))


(deftest test-variant->integer
  (testing "VBI to integer conversion"
    (is (= 32 (variant->integer [32])))
    (is (= 128 (variant->integer [128 1])))
    (is (= 16384 (variant->integer [128 128 1])))
    (is (= 2097152 (variant->integer [128 128 128 1])))))