(ns clj-mqtt.codec
  (:require
   [gloss.core :as gloss]
   [clj-mqtt.varint :refer [varint]]))

(def two-byte-int :uint16-be)
(def four-byte-int :uint32-be)
(def string (gloss/finite-frame two-byte-int (gloss/string :utf-8)))
(def binary
  (gloss/finite-block two-byte-int))

