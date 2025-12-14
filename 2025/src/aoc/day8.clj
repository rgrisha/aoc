(ns aoc.day8
  (:require [clojure.string :as s]
            [aoc.utils      :as u]
            [clojure.math   :as cm]
            [clojure.set    :as cs]))

(defn line-fn [s]
  (let [ss (s/split s #",")]
    (mapv parse-long ss)))

(defn get-data [t]
  (u/get-day-data 8 line-fn t))

(defn distance [[x1 y1 z1] [x2 y2 z2]]
  (cm/sqrt (+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1)) (* (- z2 z1) (- z2 z1)))))

(defn find-circuit-idx [[a b] circuits]
  (first
    (keep-indexed (fn [i c] (when (or (contains? c a) (contains? c b)) i))
                  circuits)))

(defn add-pairs [boxes acc]
  (let [fb (first boxes)]
    (loop [boxes (next boxes) acc acc]
      (if boxes
        (recur (next boxes) (conj! acc [fb (first boxes)]))
        acc))))

(defn gen-box-pairs [boxes]
  (loop [boxes boxes acc (transient [])]
    (if boxes
      (recur (next boxes) (add-pairs boxes acc))
      (persistent! acc))))

(defn make-connections [pairs n]
  (let [ps (sort-by (fn [[a b]] (distance a b)) pairs)
        ps (take n ps)]
    (loop [ps ps acc #{}]
      (if ps
        (let [[a b] (first ps)
              css (filter (fn [ac] (or (contains? ac a) (contains? ac b)))
                          acc)]
          (if (seq css)
            (let [cssu (reduce into #{a b} css)
                  accu (conj (cs/difference acc css) cssu)]
              (recur (next ps) accu))
            (recur (next ps) (conj acc #{a b}))))

        acc))))


(defn part-1-pm [t p c]
  (let [boxes (get-data t)
        pairs (gen-box-pairs boxes)
        conns (make-connections pairs p)
        top-conns (sort-by count conns)
        biggest (drop (- (count top-conns) c) top-conns)]
    ;(println "biggest " (count biggest) biggest)
    (reduce (fn [a e] (* a (count e))) 1 biggest)))

(defn part-1 []
  ;(part-1-pm :test 10 3)
  (part-1-pm nil 1000 3))

(defn part-2-pm [t]
  (let [boxes (get-data t)
        cnt (count boxes)
        pairs (gen-box-pairs boxes)
        ps (sort-by (fn [[a b]] (distance a b)) pairs)]
    (loop [ps ps acc #{}]
      (let [fps (first ps)
            accn (conj acc (first fps))
            accn (conj accn (second fps))]
        (if (= cnt (count accn))
          (* (first (first fps)) (first (second fps)))
          (recur (next ps) accn))))))

(defn part-2 []
  (part-2-pm nil))
