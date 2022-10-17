(ns aoc.day8
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as cset]))

(defn parse-segment [s]
  (map (comp keyword str) (seq s)))

(defn parse-part [p]
  (map parse-segment (s/split p #" ")))

(defn parse-line [l]
  (let [ps (s/split l #" \| ")]
    (map parse-part ps)))


(defn run-1 []
  (let [data  (u/get-day-data 8 parse-line)]
    (->> data
         (map second)
         (map (fn [v] (filter (fn [vv] (#{2 3 4 7} (count vv))) v)))
         (map count)
         (reduce +))))

(def digits 
  {#{0 1 2 4 5 6}   0
   #{2 5}           1
   #{0 2 3 4 6}     2
   #{0 2 3 5 6}     3
   #{1 2 3 5}       4
   #{0 1 3 5 6}     5
   #{0 1 3 4 5 6}   6
   #{0 2 5}         7
   #{0 1 2 3 4 5 6} 8
   #{0 1 2 3 5 6}   9})


;rules calculation run in repl

(def n->segs
  (into {} (map (fn [[a b]] [b a]) digits)))
  
(def single-seg-diffs
  (filter (fn [[_ _ c]] (= 1 (count c))) 
    (map (fn [[a b]] [a b (cset/difference (get n->segs a) (get n->segs b))])
      (reduce concat 
        (for [a (keys n->segs)]
          (for [b (keys n->segs)]
            [a b]))))))
  

(defn for-unrecognized [f]
  (fn [state]
    (loop [nums (:un state)]
      (if nums
        (if-let [new-state (f state (first nums))]
          new-state 
          (recur (next nums)))
        (println "Error! Empty list of unrecognized numbers" state)))))
        

(defn assign-number [asg-num & tests]
  (fn [s n]
    ;(println "--assign number got " n s)
    (when (every? (fn [f-tst] (f-tst s n)) tests)
      ;(println "--test ok with n" n)
      {:segs (:segs s)
       :n    (assoc (:n s) asg-num n)
       :un   (filter (fn [x] (not= x n)) (:un s))})))
      
   
(defn seg-count [sc]
  (fn [_ n]
    (= sc (count n))))
   
(defn has-segment [sn]
  (fn [state n]
    (let [seg-val (get-in state [:segs sn])]
      (contains? n seg-val))))
 
(defn num-diff [n1 n2]
  (fn [state]
    ;(println "\n----- diffing nums: " n1 n2)
    (cset/difference (get-in state [:n n1]) (get-in state [:n n2]))))

(defn num-minus-segs [n & segs]
  (fn [state]
    (let [numset (get-in state [:n n])]
      (reduce (fn [a v] (disj a (get-in state [:segs v]))) numset segs)))) 
    
(defn assign-segment [sn f]
  (fn [state]
    (let [ss (f state)]
      (when (not= 1 (count ss))
        (println "Error! Assignment of segment not singular: " sn ss))
      (update-in state [:segs sn] (fn [_] (first ss)))))) 
 

(def rules
  [(for-unrecognized (assign-number 1  (seg-count 2)))
   (for-unrecognized (assign-number 7  (seg-count 3)))
   (for-unrecognized (assign-number 4  (seg-count 4)))
   (for-unrecognized (assign-number 8  (seg-count 7)))
   (assign-segment 0 (num-diff 7 1))
   (for-unrecognized (assign-number 9  (seg-count 6) (fn [st n] (= 2 (count (cset/difference n (get-in st [:n 4])))))))  
   (assign-segment 4 (num-diff 8 9))
   (for-unrecognized (assign-number 2  (seg-count 5) (has-segment 4))) 
   (assign-segment 5 (num-diff 1 2))
   (for-unrecognized (assign-number 3  (seg-count 5) (fn [st n] (= 3 (count (cset/difference n (get-in st [:n 1])))))))  
   (assign-segment 1 (num-diff 9 3))
   (assign-segment 2 (num-minus-segs 1 5)) 
   (assign-segment 3 (num-minus-segs 4 1 2 5)) 
   (assign-segment 6 (num-minus-segs 8 0 1 2 3 4 5))])

(defn find-digits [[segs recg]]
  (let [state {:un (map (fn [x] (into #{} x)) segs) :n {} :segs {}}
        {segs :segs} (reduce (fn [s f]  (f s)) state rules)
        rev-segs (into {} (map (fn [[a b]] [b a]) segs))
        decoded-segs (map (fn [unseg] (map rev-segs unseg)) recg)
        decoded-segs (map (fn [a] (into #{} a)) decoded-segs)]
    (map (fn [s] (get digits s)) decoded-segs))) 

(defn run-2 []
  (let [data  (u/get-day-data 8 parse-line)
        num-fn (fn [nums] (map * nums [1000 100 10 1]))
        nums (map (fn [n] (num-fn (find-digits n))) data)]
   (apply + (flatten nums)))) 
    

