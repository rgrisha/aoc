(ns aoc.day1
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn line-fn [s]
  (seq s)) 

(defn digit? [c] (and (>= 0 (compare \0 c)) 
                      (>= 0 (compare c \9))))

(defn numval [x]
  (Character/digit x 10)) 

(defn get-data-part-1 [] 
  (u/get-day-data 1 line-fn)) 

(defn part-1 []
  (->> (get-data-part-1)
       (map (fn [e] (filter digit? e)))
       (map (fn [e] (+ (* 10 (numval (first e)))
                       (numval (last e))))) 
       (reduce + 0)))
                        

(defn get-data-part-2 []
  (u/get-day-data 1 identity)) 

(def nums [#"^zero" #"^one" #"^two" #"^three" #"^four" #"^five" #"^six" #"^seven" #"^eight" #"^nine"])

(defn find-numword-and-replace [s]
  (loop [ns nums]  
    (if (seq ns)
      (if (re-find (first ns) s)
        (s/replace s (first ns) (str (- 10 (count ns))))
        (recur (rest ns)))
      s)))
    

(defn words-to-nums [s]
  (loop [sacc "" srem s]
    (if (= srem "")
      sacc
      (let [srepl (find-numword-and-replace srem)]
        (recur (str sacc (subs srepl 0 1)) (subs srem 1))))))
    
  
;(words-to-nums "honemkmbfbnlhtbq19twonekbp")

(defn part-2 []
  (->> (get-data-part-2)
       (map words-to-nums)       
       (map seq)
       (map (fn [e] (filter digit? e)))
       (map (fn [e] (+ (* 10 (numval (first e)))
                       (numval (last e))))) 
       (reduce + 0)))

;(map (fn [a b] [a b]) (part-2) (get-data-part-2))

(part-2)
