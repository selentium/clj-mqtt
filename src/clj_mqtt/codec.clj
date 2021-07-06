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


(def reason-codes [
                   [0 :success #{:connack :puback :pubrec :pubrel :pubcomp :unsuback :auth :disconnect :suback}]
                   [1 :granted-qos1 #{:suback}]
                   [2 :granted-qos2 #{:suback}]
                   [4 :disconnect-with-will-message #{:disconnect}]
                   [16 :no-matching-subscribers #{:puback :pubrec}]
                   [17 :no-subscription-existed #{:unsuback}]
                   [24 :continue-authentication #{:auth}]
                   [25 :re-authenticate #{:auth}]
                   [-128 :unspecified-error #{:connack :puback :pubrec :suback :unsuback :disconnect}]
                   [-127 :malformed-packet #{:connack :disconnect}]
                   [-126 :protocol-error #{:connack :disconnect}]
                   [-125 :implementation-specific-error #{:connack :puback :pubrec :suback :unsuback :diasconnect}]
                   [-124 :unsupported-protocol-version #{:connack}]
                   [-123 :client-identifier-not-valid #{:connack}]
                   [-122 :bad-username-or-password #{:connack}]
                   [-121 :not-authorized #{:connack :puback :pubrec :suback :unsuback :disconnect}]
                   [-120 :server-unavailable #{:connack}]
                   [-119 :server-busy #{:connack :disconnect}]
                   [-118 :banned #{:connack}]
                   [-117 :server-shutting-down #{:disconnect}]
                   [-116 :bad-authentication-method #{:connack :disconnect}]
                   [-115 :keep-alive-timeout #{:disconnect}]
                   [-114 :session-taken-over #{:disconnect}]
                   [-113 :topic-filter-invalid #{:suback :unsuback :disconnect}]
                   [-112 :topic-name-invalid #{:connack :puback :pubrec :disconnect}]
                   [-111 :packet-identifier-in-use #{:puback :pubrec :suback :unsiback}]
                   [-110 :packet-identifier-not-found #{:pubrel :pubcomp}]
                   [-109 :receive-maximum-exceeded #{:disconnect}]
                   [-108 :topic-alias-invalid #{:disconnect}]
                   [-107 :packet-too-large #{:connack :disconnect}]
                   [-106 :message-rate-too-high #{:disconnect}]
                   [-105 :quota-exceeded #{:connack :puback :pubrec :suback :disconnect}]
                   [-104 :administrative-action #{:disconnect}]
                   [-103 :payload-format-invalid #{:connack :puback :pubrec :disconnect}]
                   [-102 :retain-not-supported #{:connack :disconnect}]
                   [-101 :qos-not-supported #{:connack :disconnect}]
                   [-100 :use-another-server #{:connack :disconnect}]
                   [-99 :server-moved #{:connack :disconnect}]
                   [-98 :shared-subscriptions-not-supported #{:suback :disconnect}]
                   [-97 :connection-rate-exceeded #{:connack :disconnect}]
                   [-96 :maximum-connect-time #{:disconnect}]
                   [-95 :subscription-identifiers-not-supported #{:suback :disconnect}]
                   [-94 :wildcard-subscriptions-not-supported #{:suback :disconnect}]
])

(def reason-codes-codec
  (gloss/compile-frame
   (gloss/enum :byte (into {}
                           (map (fn [rc] [(second rc) (first rc)] ) reason-codes)))))


(def payload-requirements {
                           :connect :required
                           :connack :none 
                           :publish :optional 
                           :puback :none
                           :pubrec :none
                           :pubrel :none
                           :pubcomp :none
                           :subscribe :required
                           :suback :required
                           :unsubscribe :required
                           :unsuback :required
                           :pingreq :none
                           :pingresp :none
                           :disconnect :none
                           :auth :none

})