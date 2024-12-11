(ns aoc.day1
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn even-digits? [stone]
  (let [n (mapv identity (str stone))]
    (= 0 (mod (count n) 2))))

;(split-stone 112055)
(defn split-stone [stone]
  (let [n (mapv identity (str stone))
        nn (quot (count n) 2)
        l (subvec n 0 nn)
        r (subvec n nn)]
    [(Long. (apply str l)) (Long. (apply str r))]))

(defn rules [stone]
  (cond
    (= 0 stone) 1
    (even-digits? stone)  (split-stone stone)
    :else (* stone 2024)))

(defn blink [stones cnt]
  (loop [stones stones cnt cnt]
    (if (> cnt 0)
       (recur (flatten (map rules stones)) (dec cnt))
       stones)))

(defn part-1 []
  (count (blink [0 89741 316108 7641 756 9 7832357 91] 25)))

(part-1)
