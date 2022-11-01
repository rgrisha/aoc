(ns aoc.day15
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as cset]))

(defn parse-line [line]
  (->> line
       seq
       (mapv #(Integer. (str %1)))))

(defn get-data []
  (let [data (u/get-day-data 15 parse-line)
        data (vec data)]
    (->
      (into {}
        (for [y (range (count data)) x (range (count (get data y)))]
          [[y x] {:v (get-in data [y x])}]))
      (update [0 0] #(assoc %1 :path [[0 0]] :min-v 0)))))

(defn show [t a]
  (println t a)
  a)

(defn find-shortest-node [nodes visited]
  (->> nodes
       (filter (fn [[n {min-v :min-v}]]
                 (and (not (contains? visited n)) 
                      (not (nil? min-v)))))  
       (sort (fn [[_ {a :min-v}] [_ {b :min-v}]]  (compare a b)))
       (first)))

(defn plus0 [a v] (+ (or a 0) v))

(defn update-node [nodes key-of-node-to-update [n {min-v :min-v path :path} :as cn] path-weight-fn]
  (let [upd-fn (fn [{min-vu :min-v :as upd-node}]
                 (let [path-weight (path-weight-fn nodes key-of-node-to-update cn) 
                       path-weight-from-current-node (plus0 min-v path-weight)]
                   (if (or (nil? min-vu) (< path-weight-from-current-node min-vu)) 
                     (assoc upd-node :min-v path-weight-from-current-node 
                                     :path (conj (or path []) key-of-node-to-update))
                     upd-node)))] 
    (update nodes key-of-node-to-update upd-fn)))


(defn update-adjacent [current-node nodes visited neighbours-fn path-weight-fn]
  (reduce 
    (fn [nodes v] (update-node nodes v current-node path-weight-fn))
    nodes
    (neighbours-fn nodes visited current-node)))
    

(defn neighbours-fn-2 [nodes visited [[x y] _]]
  (for [xn [(dec x) x (inc x)] 
        yn [(dec y) y (inc y)] 
        :when (and (not (and (= x xn) (= y yn)))  
                   (not (contains? visited [xn yn])) 
                   (contains? nodes [xn yn]))]
    [xn yn]))

(defn neighbours-fn [nodes visited [[x y] _]]
  (for [[xn yn] [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]]
        :when (and (not (contains? visited [xn yn])) 
                   (contains? nodes [xn yn]))]
    [xn yn]))

(defn path-weight-fn [nodes upd-node-id curr-node]
  (get-in nodes [upd-node-id :v]))

(defn calc-min-path [data start-node-id neighbours-fn path-weight-fn]
  (loop [nodes data visited #{} current-node [start-node-id (get nodes start-node-id)]]
    (let [nodes (update-adjacent current-node nodes visited neighbours-fn path-weight-fn)
          current-node (find-shortest-node nodes visited)]
      (if current-node
        (recur nodes (conj visited (first current-node)) current-node) 
        nodes))))
        

;620 too high
(defn run-1 []
  (let [data (get-data)
        nodes-with-min-paths (calc-min-path data [0 0] neighbours-fn path-weight-fn)]
    (get nodes-with-min-paths [99 99])))

(defn test-algo []
  (let [test-data {0 {:next [1 7]}
                   1 {:next [0 7 2]}
                   7 {:next [0 1 8 6]}
                   2 {:next [1 8 3 5]}
                   8 {:next [7 2 6]}
                   6 {:next [8 7 5]}
                   5 {:next [6 2 3 4]} 
                   3 {:next [2 5 4]} 
                   4 {:next [3 5]}} 
        path-weights {[0 1] 4
                      [0 7] 8
                      [1 2] 8
                      [1 7] 11
                      [7 6] 1
                      [7 8] 7
                      [2 8] 2
                      [8 6] 6
                      [6 5] 2
                      [2 5] 4
                      [2 3] 7
                      [3 5] 14
                      [3 4] 9
                      [5 4] 10} 
         neighbours-fn (fn [nodes visited [_ {next-nodes :next} :as cn]]
                         next-nodes)
         path-weight-fn (fn [nodes upd-node-id [curr-node-id _]]
                          (or (get path-weights [upd-node-id curr-node-id])
                              (get path-weights [curr-node-id upd-node-id])))]
            
    (calc-min-path test-data 0 neighbours-fn path-weight-fn))) 
         
                  

