(ns clj-mqtt.codec-test
  (:require [clojure.test :refer :all]
            [clj-mqtt.codec :refer :all]
            [gloss.io :as io]
            [gloss.core.formats :as f]
            [gloss.core :as gloss]
            [byte-streams :as bs]))


(deftest test-fixed-header-first-byte
  (testing "Encoding"
    (is (bs/bytes= 
         (io/encode
          fixed-header-first-byte 
          {:packet-type :connect :flags 0}) 
         (f/to-byte-buffer [16]))))
  (testing "Decoding"
    (is (= 
         (io/decode fixed-header-first-byte (f/to-byte-buffer [16]))
         {:packet-type :connect :flags 0}))))


(defn- str->bytes [s]
  (vec (map (comp byte int) s)))

(deftest test-two-byte-int
  (testing "Encoding"
    (is
     (bs/bytes=
      (io/encode two-byte-int 4)
      (f/to-byte-buffer [0 4]))))
  (testing "Decoding"
    (is
     (= 4
      (io/decode two-byte-int (f/to-byte-buffer [0 4]))))))



(deftest test-four-byte-int
  (testing "Encoding"
    (is
     (bs/bytes=
      (io/encode four-byte-int 4)
      (f/to-byte-buffer [0 0 0 4]))))
  (testing "Decoding"
    (is
     (= 4
        (io/decode four-byte-int (f/to-byte-buffer [0 0 0 4]))))))

(deftest test-string 
  (let [plain-string "mqtt" 
        encoded-string [(f/to-byte-buffer [0 4])
                        (f/to-byte-buffer [\m \q \t \t])]]
    (testing "Encoding"
      (is (bs/bytes=
           (io/encode string plain-string)
           encoded-string)))
    (testing "Decoding"
      (is (=
           plain-string
           (io/decode string encoded-string))))))


(deftest test-string-pair 
  (let [plain ["key" "value"]
        encoded [(f/to-byte-buffer [0 3])
                 (f/to-byte-buffer [\k \e \y])
                 (f/to-byte-buffer [0 5])
                 (f/to-byte-buffer [\v \a \l \u \e])]]
    (testing "Encoding"
      (is (bs/bytes=
           (io/encode string-pair plain)
           encoded)))
    (testing "Decoding"
      (is (= plain
             (io/decode string-pair encoded))))))


(deftest test-property-name
  (let [plain :payload-format-indicator
        encoded (f/to-byte-buffer [1])]
    (is (bs/bytes=
         (io/encode prop-name plain)
         encoded))
    (is (= plain
           (io/decode prop-name encoded)))))


(deftest test-prop
  (let [plain {:name :payload-format-indicator :value 1}
        encoded (f/to-byte-buffer [1 1])]
    (is (bs/bytes=
         (io/encode prop plain)
         encoded))
    (is (= plain
           (io/decode prop encoded)))))


(deftest test-properties-codec
  (let [plain [{:name :payload-format-indicator :value 1}]
        encoded [(f/to-byte-buffer [2]) (f/to-byte-buffer [1 1])]]
    (is (bs/bytes=
         (io/encode properties-codec plain)
         encoded))
    (is (= plain (io/decode properties-codec encoded)))))
