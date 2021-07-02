(ns clj-mqtt.codec
  (:require
   [gloss.core :as gloss]
   [gloss.core.formats :as f]
   [gloss.io :as io]
   [clj-mqtt.varint :refer [varint]]))

;;;primitives
(def two-byte-int :uint16-be)

(def four-byte-int :uint32-be)

(def string (gloss/finite-frame two-byte-int (gloss/string :utf-8)))

(def binary
  (gloss/finite-block two-byte-int))


;;;fixed header
(def fixed-header-first-byte 
  (gloss/compile-frame
   (gloss/bit-map :packet-type 4 :flags 4)))

(def packet-types [
                   [:reserved 0 :forbidden]
                   [:connect 1 :client-server]
                   [:connack 2 :server-client]
                   [:publish 3 :both]
                   [:puback 4 :both]
                   [:pubrec 5 :both]
                   [:pubrel 6 :both]
                   [:pubcomp 7 :both]
                   [:subscribe 8 :client-server]
                   [:suback 9 :server-client]
                   [:unsubscribe 10 :client-server]
                   [:unsuback 11 :server-client]
                   [:pingreq 12 :client-server]
                   [:pingresp 12 :server-client]
                   [:disconnect 14 :both]
                   [:auth 15 :both]])

(defn- packet-types-by-name- []
  (let [pt packet-types]
    (loop [fst (first pt)
           rst (rest pt)
           result {}]
      (if (= 0 (count rst))
        result
        (recur 
         (first rst)
         (rest rst)
         (assoc result (first fst) {:value (second fst), :direction (nth fst 2), :name (first fst)}))))))

(def packet-types-by-name (packet-types-by-name-))

(defn- packet-types-by-value- []
  (let [pt packet-types]
    (loop [fst (first pt)
           rst (rest pt)
           result {}]
      (if (= 0 (count rst))
        result
        (recur
         (first rst)
         (rest rst)
         (assoc result (second fst) {:value (second fst), :direction (nth fst 2), :name (first fst)}))))))

(def packet-types-by-value (packet-types-by-value-))