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

(deftest test-reason-code-codec
  (let [plain :success
        encoded (f/to-byte-buffer [0])]
    (is (bs/bytes= encoded
                   (io/encode reason-codes-codec plain)))
    (is (= plain (io/decode reason-codes-codec encoded)))))


(deftest test-connect-variable-header
  (let [plain {:packet-type :connect
               :protocol-name "MQTT"
               :protocol-version 5
               :connect-flags {:username false
                               :password true
                               :will-retain true
                               :will-qos 2
                               :will true
                               :clean-start true
                               :reserved false}
               :keep-alive 300
               :props [{:name :payload-format-indicator :value 1}]}
        encoded [(f/to-byte-buffer [0 4])
                 (f/to-byte-buffer [\M \Q \T \T])
                 (f/to-byte-buffer [5])
                 (f/to-byte-buffer [118])
                 (f/to-byte-buffer [1 44])
                 (f/to-byte-buffer [2])
                 (f/to-byte-buffer [1 1])]]
    (is (bs/bytes= encoded (io/encode connect-variable-header plain)))
    (is (= plain (io/decode connect-variable-header encoded)))))


(deftest test-connect-codec
  (let [plain {:variable-header {:packet-type :connect
                                 :protocol-name "MQTT"
                                 :protocol-version 5
                                 :connect-flags {:username false
                                                 :password true
                                                 :will-retain true
                                                 :will-qos 2
                                                 :will true
                                                 :clean-start true
                                                 :reserved false}
                                 :keep-alive 300
                                 :props [{:name :payload-format-indicator :value 1}]}
               :client-id "123"
               :will-props [{:name :payload-format-indicator :value 1}]}
        encoded [(f/to-byte-buffer [0 4])
                 (f/to-byte-buffer [\M \Q \T \T])
                 (f/to-byte-buffer [5])
                 (f/to-byte-buffer [118])
                 (f/to-byte-buffer [1 44])
                 (f/to-byte-buffer [2])
                 (f/to-byte-buffer [1 1])
                 (f/to-byte-buffer [0 3])
                 (f/to-byte-buffer [\1 \2 \3])
                 (f/to-byte-buffer [2])
                 (f/to-byte-buffer [1 1])]]
    (is (= plain (io/decode connect-codec encoded)))))


(comment
  (deftest test-mqtt-connect
    (let [plain {:fixed-header-first-byte {:packet-type :connect :flags 0}
                 :variable-header-and-payload {:variable-header {:packet-type :connect
                                                                 :protocol-name "MQTT"
                                                                 :protocol-version 5
                                                                 :connect-flags {:username false
                                                                                 :password true
                                                                                 :will-retain true
                                                                                 :will-qos 2
                                                                                 :will true
                                                                                 :clean-start true
                                                                                 :reserved false}
                                                                 :keep-alive 300
                                                                 :props [{:name :payload-format-indicator :value 1}]}
                                               :client-id "123"
                                               :will-props [{:name :payload-format-indicator :value 1}]}}]
      (is (= plain (io/decode mqtt-codec (io/encode mqtt-codec plain))))))

  (let [plain {:variable-header {:packet-type :connect
                                 :protocol-name "MQTT"
                                 :protocol-version 5
                                 :connect-flags {:username false
                                                 :password true
                                                 :will-retain true
                                                 :will-qos 2
                                                 :will true
                                                 :clean-start true
                                                 :reserved false}
                                 :keep-alive 300
                                 :props [{:name :payload-format-indicator :value 1}]}
               :client-id "123"
               :will-props [{:name :payload-format-indicator :value 1}]}
        encoded [(f/to-byte-buffer [0 4])
                 (f/to-byte-buffer [\M \Q \T \T])
                 (f/to-byte-buffer [5])
                 (f/to-byte-buffer [118])
                 (f/to-byte-buffer [1 44])
                 (f/to-byte-buffer [2])
                 (f/to-byte-buffer [1 1])
                 (f/to-byte-buffer [0 3])
                 (f/to-byte-buffer [\1 \2 \3])
                 (f/to-byte-buffer [2])
                 (f/to-byte-buffer [1 1])]]
    (io/encode dummy plain)))

(let [plain {
             :first-byte {:packet-type :connect :flags 0}
             :variable-header-and-payload {:variable-header {:packet-type :connect
                                                             :protocol-name "MQTT"
                                                             :protocol-version 5
                                                             :connect-flags {:username false
                                                                             :password true
                                                                             :will-retain true
                                                                             :will-qos 2
                                                                             :will true
                                                                             :clean-start true
                                                                             :reserved false}
                                                             :keep-alive 300
                                                             :props [{:name :payload-format-indicator :value 1}]}
                                           :client-id "123"
                                           :will-props [{:name :payload-format-indicator :value 1}]}}]
  (io/encode mqtt-codec plain))

(def test-mqtt-codec-connect
  (let [plain {:first-byte {:packet-type :connect :flags 0}
               :variable-header-and-payload {:variable-header {:packet-type :connect
                                                               :protocol-name "MQTT"
                                                               :protocol-version 5
                                                               :connect-flags {:username false
                                                                               :password true
                                                                               :will-retain true
                                                                               :will-qos 2
                                                                               :will true
                                                                               :clean-start true
                                                                               :reserved false}
                                                               :keep-alive 300
                                                               :props [{:name :payload-format-indicator :value 1}]}
                                             :client-id "123"
                                             :will-props [{:name :payload-format-indicator :value 1}]}}]
    (is (= plain (io/decode mqtt-codec (io/encode mqtt-codec plain))))))