(ns clj-mqtt.varint
  (:require
   [gloss.core.protocols :as protocols]
   [gloss.data.bytes :as db]
   [gloss.core.formats :as f]
   [gloss.data.bytes.core :as corebytes]))





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




(defn first-byte [buf-seq]
  (.get (first (db/take-bytes buf-seq 1))))



(defn rest-bytes [buf-seq]
  (db/drop-bytes buf-seq 1))

(def varint
  (reify
    protocols/Writer
    (sizeof [_] nil)
    (write-bytes [_ buf val]
      (print "writer" val)
      (let [bs (map unchecked-byte (integer->variant val))]
        (if-not buf
          (f/to-buf-seq (f/to-byte-buffer bs))
          (corebytes/write-to-buf (f/to-byte-buffer bs) buf))))
        protocols/Reader
        (read-bytes [this buf-seq]
            (print "reader")
            (let [buf-seq (db/dup-bytes buf-seq)]
              (loop [multiplier 1
                     encoded-byte (first-byte buf-seq)
                     remaining-bytes (rest-bytes buf-seq)
                     value (* multiplier (bit-and encoded-byte 127))]
                (if (= 0 (bit-and encoded-byte 128))
                  [true value remaining-bytes]
                  (let [mul (* multiplier 128)
                        eb (first-byte remaining-bytes)
                        rb (rest-bytes remaining-bytes)
                        v (+ value (* mul (bit-and eb 127)))]
                    (recur mul eb rb v))))))))





