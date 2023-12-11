(ns aoc.day7
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn parse-data [s]
  (let [[l r] (s/split s #" ")]
    [l (Integer/parseInt r)]))


(defn rank-fn-1 [s]
  (let [fs (frequencies s)
        rev (sort-by (fn [[n _]] (- 5 n)) (map (fn [[a b]] [b a]) fs))
        [[n1 h1] [n2 h2]] rev] 
     ;(println "rev " rev (ffirst rev) (first (second rev)) "; " (and (= 3 (ffirst rev)) (= 2 (first (second rev)))))
     (cond
       (= 5 n1)
       7 ;Five of a kind
      
       (= 4 n1)
       6 ;Four of a kind
      
       (and (= 3 n1 ) (= 2 n2))
       5 ;Full house

       (= 3 n1)
       4 ;Three of a kind

       (and (= 2 n1) (= 2 n2))
       3 ;Two pairs) 

       (= 2 n1)
       2 ;One pair

       :else
       1)))


(defn string-of-rank-gen [rank-idx-fn hands-idx]
   (fn [s]
      (let [r (rank-idx-fn s)
            rs (map hands-idx s)]
        (apply str (cons r rs)))))

(defn compare-gen [rank-to-str-fn]
   (fn [r1 r2]
      (compare (rank-to-str-fn (first r1)) (rank-to-str-fn (first r2)))))

(defn hands-idx [hands]
   (->> (map-indexed (fn [i c] [c (char (+ (int \0) i))]) hands)
        (into []) 
        (into {}))) 

  

(defn part-fn [hands rank-fn & params]
   (let [data (u/get-day-data 7 parse-data (first params))
         hands-idx (hands-idx hands)
         string-of-rank-fn (string-of-rank-gen rank-fn hands-idx)
         compare-fn (compare-gen string-of-rank-fn)]
      (->> data
           (sort compare-fn)
           (map-indexed (fn [i [_ n]] (* (inc i) n)))   
           (reduce + 0))))

(def hands-1 (reverse (seq "AKQJT98765432")))

;251121738
(defn part-1 [& params]
   (part-fn hands-1 rank-fn-1 (first params)))

(def hands-2 (reverse (seq "AKQT98765432J")))

(defn any-joker? [& cards]
   (contains? (into #{} cards) \J))

(defn rank-fn-2 [s]
  (let [fs (frequencies s)
        rev (sort-by (fn [[n _]] (- 5 n)) (map (fn [[a b]] [b a]) fs))
        [[n1 h1] [n2 h2] [_ h3] [_ h4] [_ h5]] rev] 
     ;(println "rev " rev (ffirst rev) (first (second rev)) "; " (and (= 3 (ffirst rev)) (= 2 (first (second rev)))))
     (cond
       (= 5 n1)
       7 ;Five of a kind

       (and (= 4 n1) (any-joker? h1 h2))
       7  
      
       (and (= 3 n1 ) (= 2 n2) (any-joker? h1 h2))
       7

       (= 4 n1)
       6 ;Four of a kind

       (and (= 3 n1) (any-joker? h1 h2 h3))
       6

       (and (= 2 n1) (= 2 n2) (any-joker? h1 h2))
       6
      
       (and (= 2 n1) (= 2 n2) (= \J h3))
       5

       (and (= 3 n1 ) (= 2 n2))
       5 ;Full house

       (= 3 n1)
       4 ;Three of a kind
       
       (and (= 2 n1) (any-joker? h1 h2 h3 h4))
       4

       (and (= 2 n1) (= 2 n2))
       3 ;Two pairs) 

       (= 2 n1)
       2 ;One pair

       (any-joker? h1 h2 h3 h4 h5)
       2  

       :else
       1)))


;250961722 too low
(defn part-2 [& params]
   (part-fn hands-2 rank-fn-2 (first params)))

