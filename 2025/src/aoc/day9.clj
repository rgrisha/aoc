(ns aoc.day9
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn line-fn [s]
  (mapv parse-long (s/split s #",")))

(defn get-data [t]
  (u/get-day-data 9 line-fn t))

(defn rectangles [[y x] tiles]
  (for [[ty tx] tiles]
    (* (inc (abs (- ty y))) (inc (abs (- tx x))))))

(defn max-rectangle [t tiles]
    (apply max (rectangles t tiles)))

(defn part-1-pm [t]
  (let [data (get-data t)]
    (apply max (mapv (fn [p] (max-rectangle p data)) data))))

(defn part-1 []
  (part-1-pm nil))

(defn make-lines [data]
  (let [p2 (conj (into [] (rest data)) (first data))]
    (mapv (fn [a b] [a b]) data p2)))


(defn make-lines-2 [data]
  (let [p2 (conj (into [] (rest data)) (first data))]
    (mapv (fn [[yf xf] [yt xt]]
            (cond
              (and (= yf yt) (< xf xt))
              [[yf xf] [yf (dec xt)]]

              (and (= yf yf) (> xf xt))
              [[yf (dec xt)] [yf xf]]

              (and (= xf xt) (< yf yt))
              [[yf xf] [(dec yt) xf]]

              (and (= xf xt) (> yf yt))
              [[(dec yt) xf] [yf xf]]

              :else
              (throw (Exception. (str "Incorrect pair for line coords: " yf xf "-" yt xt)))))
          data
          p2)))



(defn svg-line [[[y1 x1] [y2 x2]]]
  (let [n 10]
    (format "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"black\" stroke-width=\"10\" />\n"
            (int (/ y1 n)) (int (/ x1 n)) (int (/ y2 n)) (int (/ x2 n)))))

(defn make-svg [data]
  (spit "/tmp/svg"
    (apply str
      (let [lines (make-lines data)]
        (mapv svg-line lines)))))


