(ns aoc.day2
  (:require [aoc.utils :as u]
            [clojure.string :as s]))

(defn data-parser-fn [sl]
  (s/split sl #" " ))


(def data (u/get-day-data 2 data-parser-fn))


(defn parse-data-line [[occur-nums chr s]]
  (let [ons (s/split occur-nums #"-")]
   [(Long/parseLong (first ons))
    (Long/parseLong (last ons))
    (first chr)
    s]))


(defn occurencies [s c]
  (->> (map (fn [& v] v) s (repeat c))
       (filter (fn [[a b]] (= a b)))
       count))

(defn password-valid? [[f l c s]]
  (let [ocs (occurencies s c)]
    (and (>= ocs f) (<= ocs l))))

(defn password-valid-2? [[f l c s]]
  (->> [(dec f) (dec l)]
       (map (fn [idx] (get {c 1} (nth s idx))))
       (filter (comp not nil?))
       count
       (= 1)))

(defn run-1 []
  (let [data (u/get-day-data 2 data-parser-fn)
        data-2 (map parse-data-line data)]
    (->> data-2
         (filter password-valid?)
         count))) 
 
(defn run-2 []
  (let [data (u/get-day-data 2 data-parser-fn)
        data-2 (map parse-data-line data)]
    (->> data-2
         (filter password-valid-2?)
         count)))
