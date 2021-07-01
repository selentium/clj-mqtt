(ns clj-mqtt.core-test
  (:require [clojure.test :refer :all]
            [clj-mqtt.core :refer :all]
            [gloss.io :as io]
            [gloss.core :as gloss]
            [gloss.core.formats :as f]
            [gloss.data.bytes.core :as bs]
            [byte-streams]
            [gloss.data.bytes :as db]))


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



(deftest test-varint-codec
  (testing "VBI decode"
    (is (= 32 (io/decode varint (f/to-byte-buffer (map unchecked-byte [32])))))
    (is (= 128 (io/decode varint (f/to-byte-buffer (map unchecked-byte [128 1])))))
    (is (= 16384 (io/decode varint (f/to-byte-buffer (map unchecked-byte [128 128 1])))))
    (is (= 2097152 (io/decode varint (f/to-byte-buffer (map unchecked-byte [128 128 128 1]))))))
  (testing "VBI encode"
    (is (byte-streams/bytes= (f/to-byte-buffer (map unchecked-byte [32])) (io/encode varint 32) ))
    (is (byte-streams/bytes= (f/to-byte-buffer (map unchecked-byte [128 1]))   (io/encode varint 128)))
    (is (byte-streams/bytes= (f/to-byte-buffer (map unchecked-byte [128 128 1])) (io/encode varint 16384)))
    (is (byte-streams/bytes= (f/to-byte-buffer (map unchecked-byte [128 128 128 1])) (io/encode varint 2097152)))))

  
