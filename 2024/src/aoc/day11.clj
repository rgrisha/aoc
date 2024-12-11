(ns aoc.day11
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn even-digits? [stone]
  (let [n (mapv identity (str stone))]
    (= 0 (mod (count n) 2))))

(defn split-stone [stone cnt]
  (let [n (mapv identity (str stone))
        nn (quot (count n) 2)
        l (subvec n 0 nn)
        r (subvec n nn)]
    [[(Long. (apply str l)) cnt]  [(Long. (apply str r)) cnt]]))

(defn rules [[stone cnt]]
  (cond
    (= 0 stone) [[1 cnt]]
    (even-digits? stone)  (split-stone stone cnt)
    :else [[(* stone 2024) cnt]]))

(defn compress [stones]
  (->> stones
       (group-by first)
       vals
       (mapv (fn [l] [(ffirst l) (apply + (map second l))]))))

(defn blink [stones cnt]
  (loop [stones (map (fn [st] [st 1]) stones) cnt cnt]
    (if (> cnt 0)
       (recur (compress (mapcat rules stones)) (dec cnt))
       (apply + (map second stones)))))


(defn part-1 []
  (blink [0 89741 316108 7641 756 9 7832357 91] 25))

(defn part-2 []
  (blink [0 89741 316108 7641 756 9 7832357 91] 75))


