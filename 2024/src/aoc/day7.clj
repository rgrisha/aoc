  (ns aoc.day7
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn line-fn [s]
  (let [nums (s/split s #": | ")]
    (map (fn [s] (Long. s)) nums)))

(defn get-data [& t]
  (u/get-day-data 7 line-fn (first t)))

(defn op-list-fn [op ll]
  (map (fn [[a & [b & t]]] (cons (op a b) t)) ll))

(defn gen-universum-fn [ops]
  (let [u-fn (fn universum [ll]
                (if (= 1 (count (first ll)))
                  (map first ll)
                  (universum (apply concat (map (fn [op] (op-list-fn op ll)) ops)))))]
    (fn [l] (u-fn [l]))))

(defn line-valid? [universum-fn l]
  (let [[h & t] l]
    (some #(= h %) (universum-fn t))))

(defn part-1 [& t]
  (let [universum-fn (gen-universum-fn [+ *])]
    (->> (get-data (first t))
         (filter (fn [l] (line-valid? universum-fn l)))
         (map first)
         (apply +))))

(defn glue-fn [a b]
  (Long. (str a b)))

(defn part-2 [& t]
  (let [universum-fn (gen-universum-fn [+ * glue-fn])]
    (->> (get-data (first t))
         (filter (fn [l] (line-valid? universum-fn l)))
         (map first)
         (apply +))))


