(ns aoc.day3
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn line-fn [s]
  (mapv (fn [c] (- (int c) (int \0))) (seq s)))

(defn get-data [t]
  (u/get-day-data 3 line-fn t))


(defn get-max-of-line-1 [l]
  (let [ewi (into [] (map-indexed (fn [i e] [i e]) l))
        max-first (reduce
                    (fn [[ai ae] [ei ee]] (if (> ee ae) [ei ee] [ai ae]))
                    [0 0]
                    (drop-last ewi))
        max-second (reduce
                     (fn [[ai ae] [ei ee]] (if (> ee ae) [ei ee] [ai ae]))
                     [0 0]
                     (drop (inc (first max-first)) ewi))]
    (->> max-first
         second
         (* 10)
         (+ (second max-second)))))

(defn get-max-of-line-2 [digits-to-go l]
  (loop [ds digits-to-go nums l acc []]
    ;(println "in loop" ds nums acc)
    (if (= ds 0)
      (Long/parseLong (apply str acc))
      (let [ewi (into [] (map-indexed (fn [i e] [i e]) nums))
            numse (take (- (count nums) (dec ds)) ewi)
            [i maxn] (reduce (fn [[ai ae] [ei ee]] (if (> ee ae) [ei ee] [ai ae]))
                             [0 0]
                             numse)]
        ;(println "+numse i maxn" numse i maxn)
        (recur (dec ds)
               (drop (inc i) nums)
               (conj acc maxn))))))


(defn part-1 []
  (let [data (get-data nil)
        max-of-line (fn [e] (get-max-of-line-2 2 e))]
    (apply + (map max-of-line data))))

(defn part-2 []
  (let [data (get-data nil)
        max-of-line (fn [e] (get-max-of-line-2 12 e))]
    (apply + (map max-of-line data))))

