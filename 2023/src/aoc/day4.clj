(ns aoc.day4
  (:require [clojure.string :as s]
            [aoc.utils      :as u]
            [clojure.set    :as cs]))
 
(defn line-fn [s]
  (let [[card-str all-nums] (s/split s #": ")
        [_ card-str] (s/split card-str #" +")
        card (parse-long card-str)
        [l r] (s/split all-nums #" \| +")
        card-nums (map parse-long (s/split (s/trim l) #" +")) 
        win-nums (map parse-long (s/split (s/trim r) #" +"))] 
    [(str card) [card-nums win-nums]]))
  
(defn get-data [& args] 
  (u/get-day-data 4 line-fn (first args))) 

(defn winning-points [[_ [card-nums win-nums]]]
  (let [common-nums (cs/intersection (into #{} card-nums) (into #{} win-nums))
        size (count common-nums)]
    (if (> size 0)
      (bit-shift-left 1 (dec size))
      0)))

(defn part-1 []
  (->> (get-data)
       (map winning-points)
       (reduce + 0)))

;----------
      
(defn winning-numbers-for-card [cards]
  (fn [card-num]
    (let [[card-nums win-nums] (get cards (str card-num))
          common-nums (cs/intersection (into #{} card-nums) (into #{} win-nums))
          cnt (count common-nums)]
          ;_ (println "calc for card" common-nums card-num ":" cnt)]
      (take cnt (iterate inc (inc card-num))))))
      

;   1 + (2 3 4 5) |
;   2 + (3,4)     | 2 -> (3,4), 3 -> (4,5), 4 -> (5) , 5 -> (), 3->4,5 4->5

    
(defn game-2 [cards]
  (let [points-fn (winning-numbers-for-card cards)
        game-fn   (fn game-fn [card]
                    (let [copies (points-fn card)]
                      (concat [card] copies (map game-fn copies))))]
    (game-fn 1)))

(defn game-2-2 [cards]
  (let [cards-cnt (count cards)
        _ (println "cards count" cards-cnt)
        points-fn (winning-numbers-for-card cards)]
    (loop [i 1 freqs [{1 1}]]
      ;(println "layer " i prev-freqs)
      
      (if (< i cards-cnt)
        (let [prev-freqs (first freqs)
              nf (mapcat (fn [[k v]] (map (fn [a] {a v} ) (points-fn k))) prev-freqs)]
          (println i "nf " nf "prev" prev-freqs "nf merged" (apply merge-with + nf) "recur with " (merge-with + {(inc i) 1} (apply merge-with + nf)))
          
          (recur (inc i) (cons (merge-with + {(inc i) 1} (apply merge-with + nf)) freqs)))
        freqs))))
                            

; 3001829 too low
(defn part-2 [& args]
  (->> (get-data (first args))
       (into {})
       (game-2-2) 
       (apply merge-with +)
       vals
       (reduce + 0)))  

