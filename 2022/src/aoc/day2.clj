(ns aoc.day2
  (:require [clojure.string :as s]
            [aoc.utils      :as u]
            [clojure.set    :as cset]))
        

(defn get-data [] 
  (let [v (u/get-day-data 2 (fn [s] (s/split s #" ")))]
    v))

(def points {:r 1 :p 2 :s 3})

(def all #{:r :p :s})

(def wins {:r :s
           :s :p
           :p :r})

(def looses {:s :r
             :p :s
             :r :p})

(defn calc-points [[a b]]
  (let [ak (get {"A" :r "B" :p "C" :s} a) 
        bk (get {"X" :r "Y" :p "Z" :s} b)]
    ;(println ak bk (get points bk))
    (+ (get points bk)
       (cond (= ak bk)                3
             (= ak (get wins bk))     6
             :else                    0))))

(defn calc-strategy [[a s]]
  (let [ak (get {"A" :r "B" :p "C" :s} a)
        st (get {"X" :loose "Y" :draw "Z" :win} s)]
    (get {:r "X" :p "Y" :s "Z"}
         (case st
               :draw  ak
               :win   (get looses ak) 
               (get wins ak))))) 
                  

(defn run-1 []
  (reduce + (map calc-points (get-data))))

(defn run-2 []
  (reduce + (map (fn [[a b]] (calc-points [a (calc-strategy [a b])])) (get-data))))
