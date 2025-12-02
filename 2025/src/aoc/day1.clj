(ns aoc.day1
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn line-fn [s]
  (let [sn (.substring s 1)
        sg (.substring s 0 1)
        n  (Integer/parseInt sn)]
    (if (= sg "L")
      (* -1 n)
      n)))

(defn get-data []
  (u/get-day-data 1 line-fn))

(defn red-fn [[acc cnt] n]
  (let [a (+ acc n)
        a (mod a 100)]
    [a (if (= a 0) (inc cnt) cnt)]))

(defn make-neg [a]
  (if (<= a 0)
    a
    (- a 100)))

(defn make-pos [a]
  (if (>= a 0)
    a
    (+ a 100)))

(defn red-fn-2 [[acc cnt] n]
  (let [d     (abs (quot n 100))]
    (if (< n 0)
      (let [acc2 (make-pos acc)
            m    (make-neg (mod n 100))
            accf (+ acc2 m)]
        ;(println "acc acc2 n m accf d" acc acc2 n m accf d)
        [(mod accf 100) (+ cnt d (if (< accf 0) 1 0))])
      (let [acc2 (make-neg acc)
            m    (mod n 100)
            accf (+ acc2 m)]
        ;(println "acc acc2 n m accf d" acc acc2 n m accf d)
        [(mod accf 100) (+ cnt d (if (> accf 0) 1 0))]))))

(defn part-1 []
  (let [data (get-data)]
    (second (reduce red-fn [50 0] data))))

;6226 too low
(defn part-2 []
  (let [data (get-data)]
    (second (reduce red-fn-2 [50 0] data))))

