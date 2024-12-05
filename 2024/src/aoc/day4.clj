(ns aoc.day4
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))


(defn get-data [t]
  (u/get-day-data 4 seq t))

(def directions  (drop 1 (for [a [0 -1 1] b [0 -1 1]] [a b])))

(defn get-xy [data x y]
  (try
    (nth
      (nth data y)
      x)
    (catch Exception e (println "oob for x y " x " " y))))

(defn word-in-direction [data w h x y [dx dy]]
  (for [n [0 1 2 3]
        :let [nx (+ x (* n dx))
              ny (+ y (* n dy))]
        :when (and (>= nx 0) (>= ny 0) (< nx w) (< ny h))]

    (nth
      (nth data nx)
      ny)))

(defn get-words-4 [data w h x y]
  (map (fn [dir] (word-in-direction data w h x y dir)) directions))

(defn get-count [data w h x y]
  (count
    (filter (fn [l] (= l [\X \M \A \S]))
      (get-words-4 data w h x y))))


(defn part-1 [& t]
  (let [data (get-data (first t))
        width (count (first data))
        height (count data)]
    (apply +
           (for [x (range 0 width)
                 y (range 0 height)]
            (get-count data width height x y)))))

;(get-chars (get-data :test) 2 1 [[-1 -1] [1 1]])
;(get-chars (get-data :test) 2 1 [[-1 1] [1 -1]])
(defn get-chars [data x y [[p1x p1y] [p2x p2y]]]
  [(get-xy data (+ x p1x) (+ y p1y))
   (get-xy data (+ x p2x) (+ y p2y))])

(defn is-ms? [l]
  (or (= l [\M \S])
      (= l [\S \M])))

(defn is-mas? [data x y]
  (and
    (is-ms? (get-chars data x y [[-1 -1] [1 1]]))
    (is-ms? (get-chars data x y [[-1 1] [1 -1]]))))

(defn part-2 [& t]
  (apply +
    (let [data (get-data (first t))]
      (for [y (range 1 (dec (count data)))
            x (range 1 (dec (count (first data))))]
        (if (and (= \A (get-xy data x y)) (is-mas? data x y))
            1
            0)))))

