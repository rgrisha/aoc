(ns aoc.day5
  (:require [clojure.string :as s]
            [aoc.utils      :as u]
            [clojure.set    :as cs]))
 
  
(defn get-data [ & params] 
  (u/get-day-data 5 identity (first params)))

(defn parse-seeds [seeds]
  (let [[ _ seeds] (s/split seeds #":")] 
    (mapv parse-long (s/split (s/trim seeds) #" +"))))

(defn parse-map [[ name & maps]]
  [name
   (mapv 
     (fn [m] (mapv parse-long (s/split (s/trim m) #" +"))) 
     maps)])


(defn parse-data [lines]
  (let [[seeds, lines] (split-at 1 lines) 
        lines (drop 1 lines)
        maps (partition-by s/blank? lines)
        maps (remove (fn [m] (and (= 1 (count m)) (s/blank? (first m)))) maps)]
    [(parse-seeds (first seeds)), (into {} (mapv parse-map maps))]))

(defn map-evaluator [map]
  (fn [v]
    (loop [map map]
      (if (seq map)
        (let [[dest src cnt] (first map)]
          (if (and (>= v src) (<= v (+ src cnt -1)))
            (+ dest (- v src))
            (recur (next map))))
        v))))


(defn map-fn-chain [maps]
  (comp 
    (map-evaluator (get maps "humidity-to-location map:")) 
    (map-evaluator (get maps "temperature-to-humidity map:"))
    (map-evaluator (get maps "light-to-temperature map:"))
    (map-evaluator (get maps "water-to-light map:"))
    (map-evaluator (get maps "fertilizer-to-water map:"))
    (map-evaluator (get maps "soil-to-fertilizer map:"))
    (map-evaluator (get maps "seed-to-soil map:"))))

(defn part-1 [& params]
  (let [data (get-data (first params))
        [seeds maps] (parse-data data)
        comp-maps-fn (map-fn-chain maps)]
    (apply min (mapv comp-maps-fn seeds))))

(defn find-min-for-range [maps-fn [seed count]]
  (let [seeds (range seed (+ seed count))]
    (reduce min (map maps-fn seeds)))) 
  

(defn ranged-map-evaluator [map]
  (fn [[start size]]
    (loop [map map ranges [] rest-starte start res-size size]

      (if (empty? map)
        ranges
        
        (let [[dest src cnt] (first map)]
          (if (and (>= start src) (< start (+ src cnt)))
            (if (<= size cnt)
              (cons [(+ dest (- start src)) size] ranges)
              (recur
                (cons [(+ dest (- start src)) cnt])
                ?rest-start
                ?rest-size))))))))

;  
; 50 98 2
; 52 50 48
; 
; 1 - 100   1-49 52-97 98-99
;
;
;
;


(defn part-2 [& params]
  (let [data (get-data (first params))
        [seeds maps] (parse-data data)
        seed-pairs (partition 2 seeds) 
        comp-maps-fn (map-fn-chain maps)]
        
    maps))    
    ;    fp (first seed-pairs)
    ;(find-min-for-range comp-maps-fn fp)))
