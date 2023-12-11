(ns aoc.day8
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn parse-direction [d]
  (let [[a b c] (s/split d #"[^0-9A-Z]+")] 
    [a [b c]]))
 
(defn get-data [ & params] 
  (let [dd (u/get-day-data 8 identity (first params))
        instr (take 1 dd) 
        dirs (map parse-direction (drop 2 dd))]
    [(first instr) (into {} dirs)]))
    
(defn next-instruction-gen [initial]
  (let [instructions (volatile! initial)]
    (fn []
      (let [ret (first @instructions)]
        (if ret
          (do
            (vswap! instructions next)
            ret)
          (do
            (vreset! instructions (next initial))
            (first initial)))))))  
  

;(get-data :test)

(def stepi {\L 0 \R 1})

(defn traverse [next-instr-fn imap]
  (loop [steps 0 current-step "AAA"]
    (if (= current-step "ZZZ")
      steps
      (let [instr (next-instr-fn) 
            instr-idx (get stepi instr)
            _ (println "instr idx " instr instr-idx)
            next-step (nth (get imap current-step) instr-idx)] 
        (recur (inc steps) next-step)))))
            
(defn part-1 [& args]
  (let [[instr imap] (get-data (first args))
        next-instr-fn (next-instruction-gen instr)]
    (traverse next-instr-fn imap)))

;------------ part 2

(defn find-starting-nodes [imap]
  (->> (keys imap)
       (filter  #(= \A (last %)))))

(defn all-nodes-end-with-z? [nodes]
  (every? #(= \Z (last %))  nodes))

(defn step-fn-gen [next-instr-fn imap]
  (fn do-step [nodes]
    (let [instr (next-instr-fn)
          i (get stepi instr)
          next-nodes (mapv (fn [n] (nth (get imap n) i)) nodes)] 
      next-nodes)))

(defn do-step [imap instr nodes]
  (let [i (get stepi instr)
        next-nodes (mapv (fn [n] (nth (get imap n) i)) nodes)] 
    next-nodes))


(defn part-2 [& args]
  (let [[instr imap] (get-data (first args))
        start-nodes (find-starting-nodes imap)
        next-instr-fn (next-instruction-gen instr)
        step-fn (step-fn-gen next-instr-fn imap)]
    (loop [steps 0 nodes start-nodes]
      (if (all-nodes-end-with-z? nodes)
        steps
        (recur (inc steps) (step-fn nodes))))))

    

(defn part-22 [& args]
  (let [[instructions imap] (get-data (first args))
        start-nodes (find-starting-nodes imap)
        vsteps (volatile! 0)  
        nodes (volatile! start-nodes) 
        instrs (volatile! instructions)]
    (while
      (not (every? #(= \Z (last %)) @nodes))
      (let [instr (first @instrs)]
        (vreset! instrs (or (next @instrs) instructions))
        (vreset! nodes (do-step imap instr @nodes))
        (vswap! vsteps inc)))
    @vsteps))

