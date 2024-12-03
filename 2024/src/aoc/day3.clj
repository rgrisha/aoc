(ns aoc.day3
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn get-data []
  (apply str (u/get-day-data 3 identity)))

(defn get-mults [s]
  (re-seq #"mul\(\d\d?\d?,\d\d?\d?\)|do\(\)|don't\(\)" s))

(defn prn-ident [msg arg]
  (println msg arg)
  arg)

(defn parse-mult [s]
  (->> s
       (re-seq #"mul\((\d+),(\d+)\)")
       first
       (drop 1)
       (map (fn [s] (Integer. s)))))

(defn part-1 []
  (->> (get-data)
       get-mults
       (map parse-mult)
       (filter seq)
       (map (fn [l] (apply * l)))
       (apply +)))

(defn calc-state [cmd]
  (cond
    (re-matches #"do\(.*" cmd) true
    (re-matches #"don.*" cmd)  false))

(defn reduce-fn [[state acc] cmd]
  (let [l (parse-mult cmd)]
    (if (seq l)
      [state            (if state
                          (+ acc (apply * l))
                          acc)]
      [(calc-state cmd) acc])))

(defn part-2 []
  (->> (get-data)
       get-mults
       (reduce reduce-fn [true 0])))


