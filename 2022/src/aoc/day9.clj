(ns aoc.day9
  (:require [aoc.utils :as u]
            [clojure.string :as s]))
             



(defn get-data [& ds]
  (u/get-day-data 9 
                  (fn [line] (let [[d n] (s/split line #" ")]
                               [(-> d s/lower-case keyword) (Integer/parseInt n)])) 
                  (first ds)))

(defn new-pos [[x y] d]
  (case d
    :u [x (dec y)]
    :d [x (inc y)]
    :l [(dec x) y]
    :r [(inc x) y]))

(defn iabs [x]
  (if (< x 0) (* x -1) x))

(defn dist [a b]
  (iabs (- a b)))
    
(defn head-near-tail? [[hx hy] [tx ty]]
  (and (>= 1 (dist hx tx)) 
       (>= 1 (dist hy ty))))

(defn move-tail-after-head [[tx ty :as tp] [hx hy :as hp]]
  (if (head-near-tail? tp hp)
    tp
    (let [diff-1-x (if (< hx tx) -1 1)
          diff-1-y (if (< hy ty) -1 1)]

      (cond (= tx hx)  
            [tx (+ ty diff-1-y)]
      
            (= ty hy) 
            [(+ tx diff-1-x) ty]

            :else
            [(+ tx diff-1-x) (+ ty diff-1-y)]))))

(defn move-1 [[tp hp all-tps] [d n]]
  (loop [tp tp hp hp atp all-tps n n]
    ;(println "t h" tp hp)
    (if (= n 0)
      [tp hp atp]
      (let [nhp (new-pos hp d)
            ntp (move-tail-after-head tp nhp)] 
        (recur ntp nhp (conj atp ntp) (dec n))))))
          

(defn run-1 [& ds]
  (let [data (get-data (first ds))
        [_ _ all-tail-pos] (reduce move-1 [[0 0] [0 0] [[0 0]]] data)]
    ;(println "taip pos: " all-tail-pos)
    (count (into #{} all-tail-pos))))
                 
(def tail-length 9)


(defn move-long-tail-once [[tps hp all-tps] d]
  (doseq [i (range tail-length)]
    (let [parent-num (dec i)
          parent (if (< parent-num 0) hp (nth tps parent-num))
          ntp (move-tail-after-head (nth tps i) parent)]
      ;(println "moved " i "th from" (nth tps i) "to" ntp "because head is" parent)
      (assoc! tps i ntp)))
  [tps (conj all-tps (nth tps (dec tail-length)))])
        
(defn move-long-tail [[tps hp all-tps] [d nn]]
  (loop [tps tps hp hp atp all-tps n nn]
    (if (= n 0)
      (do
        ;(println "move " d nn "->"  [tps hp atp])
        [tps hp atp])
      (let [nhp (new-pos hp d)
            [ntps new-all-tps] (move-long-tail-once [tps nhp atp] d)] 
        (recur ntps nhp new-all-tps (dec n))))))
 
(defn run-2 [& ds]
  (let [data (get-data (first ds))
        [_ _ all-tail-pos] (reduce move-long-tail [(transient (into [] (take tail-length (repeat [0 0])))) [0 0] [[0 0]]] data)]
    (println "taip pos: " all-tail-pos)
    (count (into #{} all-tail-pos))))
 

(comment
  ; 5883
  (run-1))


