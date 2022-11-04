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

(defn show [t f a]
  (println t (f a))
  a)


(defn find-shortest-node [nodes visited]
  (->> nodes
       (filter (fn [[n {min-v :min-v}]]
                 (and (not (contains? visited n)) 
                      (not (nil? min-v)))))  
       (sort (fn [[_ {a :min-v}] [_ {b :min-v}]]  (compare a b)))
       (first)))
       

(defn find-shortest-node [nodes visited updated-nodes]
  (let [fn-ret (fn [[k _]] (when k [k (get nodes k)]))]
    (->> 
      updated-nodes
      (sort (fn [[_ v1] [_ v2]] (compare v1 v2)))
      first
      fn-ret))) 
         

(defn plus0 [a v] (+ (or a 0) v))

(defn update-node [nodes key-of-node-to-update [n {min-v :min-v path :path} :as cn] path-weight-fn updated-nodes]
  (let [{min-vu :min-v :as upd-node} (get nodes key-of-node-to-update)
        path-weight (path-weight-fn nodes key-of-node-to-update cn) 
        path-weight-from-current-node (plus0 min-v path-weight)]
    (if (or (nil? min-vu) (< path-weight-from-current-node min-vu)) 
      [(assoc nodes key-of-node-to-update 
                    (assoc upd-node :min-v path-weight-from-current-node)) 
                                    ;:path (conj (or path []) key-of-node-to-update)))
       (assoc updated-nodes key-of-node-to-update path-weight-from-current-node)]
      [nodes updated-nodes])))

(defn update-adjacent [current-node nodes visited neighbours-fn path-weight-fn updated-nodes-i]
  (reduce 
    (fn [[nodes updated-nodes] v] 
      (update-node nodes v current-node path-weight-fn updated-nodes))
    [nodes updated-nodes-i]
    (neighbours-fn nodes visited current-node)))

(defn neighbours-fn [nodes visited [[x y] _]]
  (for [[xn yn] [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]]
        :when (and (not (contains? visited [xn yn])) 
                   (contains? nodes [xn yn]))]
    [xn yn]))

(defn path-weight-fn [nodes upd-node-id curr-node]
  (get-in nodes [upd-node-id :v]))

(defn calc-min-path [data start-node-id neighbours-fn path-weight-fn]
  (loop [nodes data visited #{} current-node [start-node-id (get nodes start-node-id)] updated-nodes (assoc {} start-node-id 0)]
    (let [[nodes updated-nodes] (update-adjacent current-node nodes visited neighbours-fn path-weight-fn updated-nodes)
          current-node (find-shortest-node nodes visited updated-nodes)]
      (if current-node
        (recur nodes (conj visited (first current-node)) current-node (dissoc updated-nodes (first current-node))) 
        nodes))))
        

;673 is the answer

(require '[clj-async-profiler.core :as prof])

(defn run-1-p []
  (let [data (get-data)
        nodes-with-min-paths (calc-min-path data [0 0] neighbours-fn path-weight-fn)]
    (get nodes-with-min-paths [99 99])))

(defn run-1 []
  ;(prof/profile (run-1-p))
  (run-1-p))

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
         
(defn mod91 [x]
  (let [m (mod x 9)]
    (if (zero? m) 9 m)))
                  
(defn make-data-5x [data]
  (into {}
    (for [xm [0 1 2 3 4] ym [0 1 2 3 4] [[x y] {v :v}] data
          :let [v-inc (+ xm ym)]] 
      [[(+ (* 100 xm) x) (+ (* 100 ym) y)] {:v (mod91 (+ v-inc v))}])))
  

;:min-v = 2893
(defn run-2 []
  (let [data (get-data)
        big-data (make-data-5x data)
        big-data (update big-data [0 0] #(assoc %1 :path [[0 0]] :min-v 0))
        nodes-with-min-paths (calc-min-path big-data [0 0] neighbours-fn path-weight-fn)]
    (get nodes-with-min-paths [499 499])))
