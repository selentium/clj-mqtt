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



