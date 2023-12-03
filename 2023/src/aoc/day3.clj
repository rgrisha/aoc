(ns aoc.day3
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(def non-symbols (into #{} (seq "0123456789.")))

(defn collect-line-symbols [symbols line-pos line]
  (let [line-symbols (seq line)] 
    (loop [i 0 ls line-symbols coll-symbols symbols]
      (cond (nil? ls) 
            coll-symbols
            
            (contains? non-symbols (first ls))
            (recur (inc i) (next ls) coll-symbols)            

            :else
            (recur (inc i) (next ls) (assoc coll-symbols [i line-pos] (first ls)))))))
            
(defn collect-symbols [lines]
  (loop [symbols {} i 0 ls lines]
    (if (nil? ls) 
      symbols
      (recur (collect-line-symbols symbols i (first ls)) (inc i) (next ls)))))
          

(defn get-data [] 
  (u/get-day-data 3 identity)) 

(defn adjacent? [symbols [x y]]
  (or (symbols [(dec x) (dec y)])
      (symbols [(dec x) y])
      (symbols [(dec x) (inc y)])
      (symbols [x (dec y)])
      (symbols [x (inc y)])
      (symbols [(inc x) (dec y)])
      (symbols [(inc x) y])
      (symbols [(inc x) (inc y)])))

(def numbers (into #{} (seq "0123456789")))

(defn find-numbers [line]
  (loop [chars (map-indexed (fn [i c] [i c]) (seq line)) acc []]
    (if (seq chars)
       (let [nums-begin (drop-while (fn [[ _ c]] (not (contains? numbers c))) chars)
             [l r] (split-with (fn [[ _ c]] (contains? numbers c)) nums-begin)]
         (recur r (conj acc l)))
      acc)))

(defn any-digit-adjacent [line-num digits symbols]
  (some (fn [[x _]] (adjacent? symbols [x line-num])) digits))
  
(defn find-numbers-adjacent-to-symbols [symbols lines]
  (loop [lines lines i 0 acc []]
    (if lines
      (let [nums (find-numbers (first lines))
            adjacent-nums (filter (fn [digits] (any-digit-adjacent i digits symbols)) nums)]
        (recur (next lines) (inc i) (concat acc adjacent-nums)))
      acc)))

(defn part-1 []
  (let [lines (get-data)
        symbols (collect-symbols lines)
        adj-numbers (find-numbers-adjacent-to-symbols symbols lines) 
        adj-numbers (map (fn [adj-num] (parse-long (apply str (map second adj-num)))) adj-numbers)]
    (reduce + 0 adj-numbers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn adjacent [symbols [x y]]
  (let [adjacents [(symbols [(dec x) (dec y)])
                   (symbols [(dec x) y])
                   (symbols [(dec x) (inc y)])
                   (symbols [x (dec y)])
                   (symbols [x (inc y)])
                   (symbols [(inc x) (dec y)])
                   (symbols [(inc x) y])
                   (symbols [(inc x) (inc y)])]]
    (remove nil? adjacents)))
      
(defn adjacent-gears-for-number [line-num gear-idx num]
  (let [adjacent-gear-idxs (mapcat (fn [[x _]] (adjacent gear-idx [x line-num])) num)]
    (into #{} adjacent-gear-idxs)))

(defn map-numbers-to-gears-for-line [gear-ids gears line line-num]
  (let [numbers (find-numbers line)]
    (loop [numbers numbers gears gears]
      (if numbers
        (let [number (first numbers)
              int-number (parse-long (apply str (map second number)))
              adj-gears (adjacent-gears-for-number line-num gear-ids number)
              ;_ (println "adj gears" adj-gears ". " (mapv (fn [k] [k int-number]) adj-gears))
              adj-gears-num (into {} (mapv (fn [k] [k [int-number]]) adj-gears))
              gears-merged (merge-with into gears adj-gears-num)]
          (recur (next numbers) gears-merged))
        gears))))

(defn map-numbers-to-gears-for-lines [gear-symbols lines]
  (let [gear-ids (into #{} (map first gear-symbols))
        gears (into {} (map (fn [[k _]] [k []]) gear-symbols))]
    (loop [lines lines i 0 gears gears]
      (if lines
        (let [gears (map-numbers-to-gears-for-line gear-ids gears (first lines) i)]
          (recur (next lines) (inc i) gears))
        gears))))
  

(defn part-2 []
  (let [lines (get-data)
        symbols (collect-symbols lines)
        gear-symbols (into {} (filter (fn [[_ v]] (= \* v)) symbols))
        gears (map-numbers-to-gears-for-lines gear-symbols lines) 
        real-gears (filter (fn [[_ v]] (= 2 (count v))) gears)
        real-gear-parts (map second real-gears)
        _ (println real-gear-parts)
        real-part-ratios (map (fn [m] (apply * m)) real-gear-parts)]
    (reduce + 0 real-part-ratios))) 

