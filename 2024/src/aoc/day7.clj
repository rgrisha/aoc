  (ns aoc.day7
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn line-fn [s]
  (let [nums (s/split s #": | ")]
    (map (fn [s] (Long. s)) nums)))

(defn get-data [& t]
  (u/get-day-data 7 line-fn (first t)))

(defn universum-results [l]
  (let [[h & t] l]
    (if (seq t)
      (let [rs (universum-results t)]
        (concat
          (map (fn [r] (+ r h)) rs)
          (map (fn [r] (* r h)) rs)))
      [h])))

(defn line-valid? [l]
  (let [[h & t] l]
    (some #(= h %) (universum-results (reverse t)))))

(defn prnn [p]
  (println "p " p)
  p)

(defn part-1 [& t]
  (->> (get-data (first t))
       (filter line-valid?)
       (map first)
       (apply +)))





