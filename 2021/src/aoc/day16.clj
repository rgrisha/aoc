(ns aoc.day16
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as cset]))

(defn get-data []
  (let [data (u/get-day-data 16 identity) ]
    (first data)))



(defn dec2bins [d]
  (loop [acc '() n d step 4]
    (if (zero? step)
      acc
      (recur (cons (rem n 2) acc) (quot n 2) (dec step)))))

(defn hexs-to-bins [s]
  (reduce (fn [a v]
            (let [cn (int v)]
              (into a (if (> cn (int \9)) (dec2bins (+ 10 (- cn (int \A)))) (dec2bins (- cn (int \0))))))) 
          [] 
          (seq s)))

(defn bins-to-dec [ss]
  (reduce (fn [a v] (+ v (bit-shift-left a 1)) ) ss))

(defn parse-bits [keyname length check-fn convert-fn acc bits]
  (let [v (take length bits)]
    (when (check-fn v)
      [(assoc acc keyname (convert-fn v)) (drop length bits)])))

(def parse-packet)

(def true-fn (constantly true))

(def packet-ver (partial parse-bits :ver 3 true-fn bins-to-dec))
(def literal-type (partial parse-bits :type 3 #(= %1 [1 0 0]) bins-to-dec))
(def operator-type (partial parse-bits :type 3 #(not= %1 [1 0 0]) bins-to-dec))
(def op-len-type (partial parse-bits :lentype 1 #(= %1 [0]) bins-to-dec))
(def op-cnt-type (partial parse-bits :lentype 1 #(= %1 [1]) bins-to-dec))
(def op-length (partial parse-bits :length 15 true-fn bins-to-dec))
(def op-count (partial parse-bits :count 11 true-fn bins-to-dec))

(defn literal [a bits]
  (loop [s bits acc []]
    (let [[h t] (split-at 1 s)]
      (if (zero? (first h))
        [(assoc a :literal (bins-to-dec (concat acc (take 4 t)))) (drop 4 t)]
        (recur (drop 4 t) (concat acc (take 4 t)))))))

(defn combine [fns a bits]
  (loop [fs fns acc {} bs bits]
    (if-let [f (first fs)]
      (let [[a b] (f acc bs)]
        (when a
          (recur (next fs) a b)))
      [acc bs])))

(defn parse-children-by-length-gen [parse-fn acc bits]
  (let [bs (take (:length acc) bits)
        children
        (loop [children- [] bsl bs]
          (if (empty? bsl)
            children-
            (let [[acc bsl-] (parse-fn {} bsl)]
              (recur (conj children- acc) bsl-))))]
    [(assoc acc :children children) (drop (:length acc) bits)]))

(defn parse-children-by-count-gen [parse-fn acc bits]
 (let [c (:count acc)
       ret
        (loop [children [] bsl bits cnt c]
          (if (zero? cnt)
            [(assoc acc :children children) bsl]  
            (let [[acc bsl-] (parse-fn {} bsl)]
              (recur (conj children acc) bsl- (dec cnt)))))]
   ret))

(defn combine-or [fns acc bits]
  (loop [fs fns]
    (let [[a bl] ((first fs) acc bits)]
      (if a
        [a bl]
        (recur (next fs))))))

(defn parse-packet [acc bits]
  (let [parse-children-by-length (partial parse-children-by-length-gen parse-packet)
        parse-children-by-count  (partial parse-children-by-count-gen parse-packet)]
    (combine-or [
      (partial combine [packet-ver operator-type op-len-type op-length parse-children-by-length])
      (partial combine [packet-ver operator-type op-cnt-type op-count parse-children-by-count])
      (partial combine [packet-ver literal-type literal])] 
      
      acc bits)))


(defn decode [s]
  (let [nseq (hexs-to-bins s)]
    (parse-packet {} nseq)))

(defn count-versions [pt]
  (let [flat (tree-seq (fn [e] (:children e)) (fn [e] (:children e)) pt)
        versions (map :ver flat)]
    (reduce + versions)))

(def ops {0 (fn [cs] (reduce + cs))
          1 (fn [cs] (reduce * cs)) 
          2 (fn [cs] (apply min cs))
          3 (fn [cs] (apply max cs))
          5 (fn [[a b]] (if (> a b) 1 0))
          6 (fn [[a b]] (if (< a b) 1 0))
          7 (fn [[a b]] (if (= a b) 1 0)) })

(defn walk-with-ops [pt]
  (if (:children pt)
    (let [cvs (map walk-with-ops (:children pt))
          f (get ops (:type pt))]
      (f cvs))
    (:literal pt)))

(defn run-1 []
  (let [data (get-data)]
    (count-versions (first (decode data)))))

(defn run-2 []
  (let [data (get-data)]
    (walk-with-ops (first (decode data)))))
