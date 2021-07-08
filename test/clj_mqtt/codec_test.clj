(ns clj-mqtt.codec-test
  (:require [clojure.test :refer :all]
            [clj-mqtt.codec :refer :all]
            [gloss.io :as io]
            [gloss.core.formats :as f]
            [byte-streams :as bs]))


(deftest test-fixed-header-first-byte
  (testing "Encoding"
    (is (bs/bytes= (io/encode fixed-header-first-byte {:packet-type :connect :flags 0}) (f/to-byte-buffer [16]) )))
  (testing "Decoding"
    (is (= (io/decode fixed-header-first-byte (f/to-byte-buffer [16])) {:packet-type :connect :flags 0}))))

