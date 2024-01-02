(ns aoc.day14
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn line-fn [[y _ round-stones square-stones] s]
  (let [[ex rsa ssa] (reduce (fn [[x rs ss] c]
                              (cond
                                (= c \#) [(inc x) rs (conj ss [y x])] 
                                (= c \O) [(inc x) (conj rs [y x]) ss] 
                                :else    [(inc x) rs ss]))
                            [0 round-stones square-stones] s)]
    [(inc y) ex rsa ssa]))


(defn get-data [& args] 
  (u/get-day-data-reduce 14 line-fn [0 0 #{} #{}] (first args))) 

(defn ppp [t x]
  (println t x)
  x)

(defn print-col [max-y x round-stones square-stones]
  (println (for [y (range max-y)] (cond (contains? round-stones [y x]) "O" (contains? square-stones [y x]) "#" :else "."))))




(defn find-free [max-y x y rs ss]
  (loop [y y]
    (cond
      (>= y max-y)
      max-y

      (or (contains? rs [y x]) (contains? ss [y x]))
      (recur (inc y))

      :else
      y)))

(defn calculate-col-north [max-y x round-stones square-stones]
  (loop [y 0 last-free-y (find-free max-y x 0 round-stones square-stones) rs round-stones]
    (if (>= y max-y)
      rs
      (cond (contains? square-stones [y x])
            (recur (inc y) (find-free max-y x y rs square-stones) rs)

            (contains? rs [y x])
            (if (> y last-free-y)
              (let [new-rs (conj (disj rs [y x]) [last-free-y x])]
                (recur (inc y) 
                       (find-free max-y x (inc last-free-y) new-rs square-stones)
                       new-rs))
              (recur (inc y) last-free-y rs)) 

            :else
            (recur (inc y) last-free-y rs)))))

(defn calculate-fall-north [max-y max-x round-stones square-stones]
  (reduce (fn [rs x] (calculate-col-north max-y x rs square-stones)) round-stones (range max-x))) 



(defmacro bench
  " Times the execution of your function,
    discarding the output and returning the elapsed time in seconds
    (you can modify this by changing the divisor from 1e9 (i.e. for milliseconds it would be 1e6."
  ([& forms]
   `(let [start# (System/nanoTime)]
      ~@forms
      (double (/ (- (System/nanoTime) start#) 1e9)))))   ; Time in ms


(defn calculate-load [max-y rs]
  (reduce (fn [a [y _]] (+ a (- max-y y))) 0 rs)) 

;112048
(defn part-1 [& args]
  (let [[max-y max-x round-stones square-stones] (get-data (first args))
        round-stones (calculate-fall-north max-y max-x round-stones square-stones)] 
    (calculate-load max-y round-stones)))

;--------------------

(defn find-sq-stone-positions [maxy x ss]
  (let [last-idx (dec maxy)
        idxs (keep (fn [i] (when (or (= i 0) (= i last-idx) (contains? ss [i x])) i)) (range maxy))]
    (map (fn [a b] [a b]) idxs (next idxs))))

(defn move-in-range-x [round-stones from to x]
  (loop [i from found 0 round-stones round-stones]
    (if (<= i to)
      (if (contains? round-stones [i x])
        (recur (inc i) (inc found) (disj round-stones [i x]))
        (recur (inc i) found round-stones))
      (reduce (fn [a i] (conj a [i x])) round-stones (range from (+ from found))))))

(defn move-north-for-x [maxy sq-positions x round-stones]
  (loop [sq-positions sq-positions round-stones round-stones]
    (if (seq sq-positions)
      (let [[from to] (first sq-positions)
            round-stones (move-in-range-x round-stones from to x)] 
         (recur (next sq-positions) round-stones))
      round-stones)))
    

(defn part-1-opt [& args]
  (let [[maxy maxx round-stones square-stones] (get-data (first args))
        sq-pos (mapv (fn [x] (find-sq-stone-positions maxy x square-stones)) (range maxx))]
    (bench
      (loop [round-stones round-stones x 0]
        (if (< x maxx)
          (recur (move-north-for-x maxy (sq-pos x) x round-stones) (inc x))
          (calculate-load maxy round-stones))))))


