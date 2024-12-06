(ns aoc.day6
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn get-data [t]
  (u/get-day-data 6 (fn [s] (mapv identity s)) (first t)))

(defn get-char [data [x y]]
  (nth
    (nth data y)
    x))

(defn find-guard [data]
  (loop [y 0]
    (let [line (nth data y)]
      (if-let [x (seq (keep-indexed #(when (= \^ %2) %1) line))]
        [(first x) y]
        (recur (inc y))))))

(defn direction [idx]
  (nth [[0 -1] [1 0] [0 1] [-1 0]] (mod idx 4)))

(defn next-guard-step [[x y] idx]
  (let [[dx dy] (direction idx)]
    [(+ x dx) (+ y dy)]))

(defn obstacle? [data [x y]]
  (= \# (nth (nth data y) x)))


(defn part-1 [& t]
  (let [data (get-data t)
        guard (find-guard data)
        visited (transient [guard])]
    (try
      (loop [guard guard direction-idx 0]
        (let [next-step (next-guard-step guard direction-idx)]
          (if (obstacle? data next-step)
            (recur guard (inc direction-idx))
            (do
              (conj! visited next-step)
              (recur next-step  direction-idx)))))

      (catch IndexOutOfBoundsException e
        (->> visited
             persistent!
             (into #{})
             count)))))

(defn guard-loops? [data guard additional-obstacle]
  (let [visited-corners (transient #{})]
    (try
      (loop [guard guard direction-idx 0 prev-corner nil]
        (let [next-step (next-guard-step guard direction-idx)]
          (if (or (= next-step additional-obstacle) (obstacle? data next-step))
            (if (and (not (= guard prev-corner)) (contains? visited-corners guard))
              true
              (do
                (conj! visited-corners guard)
                (recur guard (inc direction-idx) guard)))
            (recur next-step direction-idx prev-corner))))

      (catch IndexOutOfBoundsException e
        false))))

(defn part-2 [& t]
  (let [data (get-data t)
        guard (find-guard data)
        loop-count (volatile! 0)]
    (doseq [y (range 0 (count data))
            x (range 0 (count (first data)))]
      (when (= \. (nth (nth data y) x))
        (when (guard-loops? data guard [x y])
          (vswap! loop-count inc))))
    @loop-count))

