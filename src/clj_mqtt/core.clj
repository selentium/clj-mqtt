(ns clj-mqtt.core
  (:gen-class)
  (:require
   [manifold.deferred :as d]
   [manifold.stream :as s]
   [clojure.edn :as edn]
   [aleph.tcp :as tcp]
   [gloss.core :as gloss]
   [gloss.io :as io]
   [byte-streams :as bs]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))



(defn- div [a b]
  (int (/ a b)))


(defn- encode-byte [n]
  (let [x (div n 128)
        eb (mod n 128)]
    (if (> x 0)
      [x (bit-or eb 128)]
      [x eb])))

(defn integer->variant [n]
  (let [first-encoding (encode-byte n)]
    (loop [x (first first-encoding)
           eb (last first-encoding)
           result [eb]]
      (if (= x 0)
        result
        (let [enc (encode-byte x)]
          (recur (first enc) (last enc) (conj result (last enc))))))))



(defn variant->integer [bs]
  (loop [multiplier 1
         encoded-byte (first bs)
         remaining-bytes (rest bs)
         value (* multiplier (bit-and encoded-byte 127))]
    (if (= 0 (bit-and encoded-byte 128))
      value
      (let [mul (* multiplier 128)
            eb (first remaining-bytes)
            rb (rest remaining-bytes)
            v (+ value (* mul (bit-and eb 127)))]
        (recur mul eb rb v)))))

