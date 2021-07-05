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

(def string-pair [string string])

(def binary
  (gloss/finite-block two-byte-int))


;;;fixed header
(def fixed-header-first-byte 
  (gloss/compile-frame
   (gloss/bit-map :packet-type 4 :flags 4)))

(defonce packet-types [
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

(def fixed-header-remaining-bytes varint)


;;;variable header
(def packet-identifier two-byte-int)

(def packet-types-requiring-packet-identifier #{:publish :puback :pubrec :pubrel :pubcomp :subscribe :suback :unsibscribe :unsuback})

(defn requires-packet-identifier? [packet-type]
  (contains? packet-types-requiring-packet-identifier packet-type))


(defonce properties [
                 [1 :payload-format-indicator :byte #{:publish :will}]
                 [2 :message-expiry-interval four-byte-int #{:publish :will}]
                 [3 :content-type string #{:publish :will}]
                 [8 :response-topic string #{:publish :will}]
                 [9 :correlation-data binary #{:publish :will}]
                 [11 :subscription-identifier varint #{:publish :subscribe}]
                 [17 :session-expiry-interval four-byte-int #{:connect :connack :disconnect}]
                 [18 :assigned-client-identifier string #{:connack}]
                 [19 :server-keep-alive two-byte-int #{:connack}]
                 [21 :authentication-method string #{:connect :connack :auth}]
                 [22 :authentication-data binary #{:connect :connack :auth}]
                 [23 :request-problem-information :byte #{:connect}]
                 [24 :will-delay-interval four-byte-int #{:will}]
                 [25 :request-response-information :byte #{:connect}]
                 [26 :response-information string #{:connack}]
                 [28 :server-reference string #{:connack :disconnect}]
                 [31 :reason-string string #{:connack :puback :pubrec :pubrel :pubcomp :suback :unsuback :disconnect :auth}]
                 [33 :receive-maximum two-byte-int #{:connect :connack}]
                 [34 :topic-alias-maximum two-byte-int #{:connect :connack}]
                 [35 :topic-alias two-byte-int #{:publish}]
                 [36 :max-qos :byte #{:connack}]
                 [37 :retain-available :byte #{:connack}]
                 [38 :user-property string-pair #{:connect :connack :publish :will :puback :pubrec :pubrel :pubcomp :subscribe :suback :unsubscribe :unsuback :disconnect :auth}]
                 [39 :maximum-packet-size four-byte-int #{:connect :connack}]
                 [40 :wildcard-subscription-available :byte #{:connack}]
                 [41 :subscription-identifier-available :byte #{:connack}]
                 [42 :shared-subscription-available :byte #{:connack}]
])

(defn- property-name->value []
  (into {} (map (fn [item]
                  [(second item) (first item)]) properties)))

(defn- property-name->codec []
  (into {} (map (fn [item]
                  [(second item) (nth item 2)]) properties)))

(defn prop-value-codec [name]
  (let [codecs-map (property-name->codec)]
    {:name name :value (name codecs-map)}))




;made it as byte to make gloss happy
;MQTT spec says it should be VBI
(def prop-name
  (gloss/compile-frame
   (gloss/enum :byte (property-name->value))))

(def prop 
  (gloss/compile-frame
   (gloss/header
    prop-name
    prop-value-codec
    :name)))

(def properties-codec
  (gloss/compile-frame
   (gloss/finite-frame varint
                       (gloss/repeated prop :prefix :none))))