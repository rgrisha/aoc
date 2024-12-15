(ns aoc.day14
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(def rex #"p=(\d+),(\d+) v=(-?\d+),(-?\d+)")

(defn line-fn [s]
  (let [ms (re-matches rex s)]
    (mapv (fn [si] (Integer. si)) (drop 1 ms))))

(defn get-data [t]
  (u/get-day-data 14 line-fn t))


(defn gen-one-second-motion [w h]
  (let [new-pos (fn [c vc d]
                  (let [nc (+ c vc)]
                    (cond
                      (< nc 0) (+ nc d)
                      (>= nc d) (mod nc d)
                      :else nc)))
        move-robot (fn [[x y vx vy]]
                     [(new-pos x vx w) (new-pos y vy h) vx vy])]
    (fn [robots _]
      (mapv move-robot robots))))

(defn gen-quadrant [w h]
  (let [wd (quot w 2)
        hd (quot h 2)]
    (fn [[x y _ _]]
      (cond
        (and (< x wd) (< y hd)) 0
        (and (< x wd) (> y hd)) 1
        (and (> x wd) (< y hd)) 2
        (and (> x wd) (> y hd)) 3
        :else -1))))

;(part-1 :test)
(defn part-1 [& t]
  (let [test? (first t)
        [w h] (if test? [11 7] [101 103])
        robots (get-data (first t))
        move-fn (gen-one-second-motion w h)
        quadrant-fn (gen-quadrant w h)
        robots (reduce move-fn robots (range 0 100))
        quadrants (group-by quadrant-fn robots)
        qc (mapv (fn [q] (count (get quadrants q)) ) [0 1 2 3])]
    (apply * qc)))

(defn all-in-unique-positions? [robots]
  (let [s (into #{} (map (fn [[x y _ _]] [x y]) robots))]
    (= (count s) (count robots))))

(defn save-to-file [robots w h]
  (let [sri (into #{} (map (fn [[x y _ _]] [x y]) robots))]
    (doseq [y (range 0 h)]
      (spit "/tmp/tree.txt"
            (str
              (apply str (for [x (range 0 w)]
                           (if (contains? sri [x y]) "#" " ")))
              "\n")
            :append true))))


(defn part-2 []
  (let [[w h] [101 103]
        wl (dec w)
        robots (get-data nil)
        move-fn (gen-one-second-motion w h)
        quadrant-fn (gen-quadrant w h)]
    (loop [robots robots secs 0]
      (if (all-in-unique-positions? robots)
        (do
          (save-to-file robots w h)
          secs)
        (recur (move-fn robots :nothing) (inc secs))))))






