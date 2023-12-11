(ns aoc.day10
  (:require [clojure.string :as s]
            [aoc.utils      :as u]
            [clojure.set    :as cs]))

 
(defn get-data [ & params] 
  (u/get-day-data 10 (fn [s] (into [] (seq s))) (first params)))


(defn find-start [imap]
  (ffirst
    (keep-indexed (fn [y cols] (seq (keep-indexed (fn [x c] (when (= c \S) [y x])) cols))) imap)))

(find-start (get-data))


;directions clockwise:  0 up ; 1 right ; 2 down ; 3 left

(def positions 
  {:u [-1  0]
   :d [ 1  0]
   :l [ 0 -1]
   :r [ 0  1]})

(def pipes
  {[\| :u] :u
   [\| :d] :d
   [\- :l] :l
   [\- :r] :r
   [\L :d] :r
   [\L :l] :u 
   [\J :d] :l
   [\J :r] :u
   [\7 :r] :d 
   [\7 :u] :l
   [\F :u] :r 
   [\F :l] :d}) 

(defn add-coords [[cy cx] [dy dx]]  
  [(+ cy dy) (+ cx dx)])

(defn get-pipe-by-pos [imap [cy cx]]
  (nth (nth imap cy) cx))
    
(defn find-next [imap direction current-pos]
  (let [next-pos (add-coords current-pos (positions direction))
        next-pipe (get-pipe-by-pos imap next-pos) 
        next-direction (pipes [next-pipe direction])]
    [next-pos next-pipe next-direction]))


(defn find-first-pipe [imap current-pos]  
  (some (fn [dir]
          (let [pipe-pos (add-coords current-pos (positions dir))
                pipe (get-pipe-by-pos imap pipe-pos)] 
            (pipes [pipe dir])))
        [:u :d :l :r]))

(defn walk-pipes-1 [imap]
  (let [start-pos (find-start imap)
        first-direction (find-first-pipe imap start-pos)]
    (loop [pos start-pos direction first-direction pipe nil steps 0]
      (if (= pipe \S)
        steps 
        (let [[next-pos next-pipe next-direction] (find-next imap direction pos)]
          (println "next pos pipe direction" next-pos next-pipe next-direction)
          (recur next-pos next-direction next-pipe (inc steps)))))))
    
   
(defn part-1 [& args]
  (-> (get-data (first args))
      walk-pipes-1
      (bit-shift-right 1)))
       
