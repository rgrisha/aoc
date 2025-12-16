(ns aoc.day9
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn line-fn [s]
  (mapv parse-long (s/split s #",")))

(defn get-data [t]
  (u/get-day-data 9 line-fn t))

(defn make-lines [data]
  (let [p2 (conj (into [] (rest data)) (first data))]
    (mapv (fn [[yf xf] [yt xt]]
            (cond
              (and (= yf yt) (< xf xt))
              [[yf xf] [yf xt]]

              (and (= yf yf) (> xf xt))
              [[yf xt] [yf xf]]

              (and (= xf xt) (< yf yt))
              [[yf xf] [yt xf]]

              (and (= xf xt) (> yf yt))
              [[yt xf] [yf xf]]

              :else
              (throw (Exception. (str "Incorrect pair for line coords: " yf xf "-" yt xt)))))
          data
          p2)))

;
;        1
;    1   #   x1 = x2
;    2   #
;    3   #
;
;    5   #####   y1 = y2
;

(defn mk-intervals [l]
  (when (odd? (count l))
    (throw (Exception. "Odd number of items in the list")))
  (into [] (partition 2 l)))

(defn merge-points-and-lines-for-number [pl]
  (let [points (into [] (sort (filter number? pl)))
        lines  (filterv coll? pl)]
    (cond
      (empty? points)
      (do
        (when (> (count lines) 1) (throw (Exception. (str "no points and more than 1 line: ps "  points " ls " lines " pl " pl))))
        [[(ffirst lines) (second (first lines))]])

      (seq lines)
      (let [minl (ffirst (sort-by first lines))
            maxl (second (first (sort (fn [[_ a] [_ b]] (- b a)) lines)))
            lastp (dec (count points))]
        ;(println "-----lll" pl points lines "-m-" minl maxl)
        (-> points
            (update 0     min minl)
            (update lastp max maxl)
            mk-intervals))

      :else
      (mk-intervals points))))

(defn merge-points-and-lines [point-map]
  (loop [pm point-map acc (transient {})]
    (if pm
      (recur (next pm)
             (let [[k v] (first pm)]
               ;(println "mapping" k "->" v)
               (assoc! acc k (merge-points-and-lines-for-number v))))
      (persistent! acc))))


(defn lines-for-ray [lines]
  (loop [lines lines y-to-xs {} x-to-ys {}]
    (if lines
      (let [[[x1 y1] [x2 y2]] (first lines)]
        (cond
          (= x1 x2)
          (recur (next lines)
                 (reduce (fn [a y] (update a y conj x1)) y-to-xs (range y1 y2))
                 (update x-to-ys x1 conj [y1 y2]))

          (= y1 y2)
          (recur (next lines)
                 (update y-to-xs y1 conj [x1 x2])
                 (reduce (fn [a x] (update a x conj y1)) x-to-ys (range x1 x2)))

          :else
          (throw (Exception. (str "in ray: bad line " (first lines))))))

      [y-to-xs x-to-ys])))

(defn is-inside-figure? [c ray-map f t]
  (let [ranges (get ray-map c)]
    ;(println "got ranges " ranges "for" c)
    (some (fn [[a b]] (and (>= f a) (<= t b))) ranges)))

(defn valid-fn-gen [y-to-xs x-to-ys]
  (fn [[x1 y1] [x2 y2]]
    (let [l (if (< x1 x2) x1 x2)
          r (if (< x1 x2) x2 x1)
          u (if (< y1 y2) y1 y2)
          d (if (< y1 y2) y2 y1)]
      (if (or (> 2 (- r l)) (> 2 (- d u)))
        false  ; for this task this is ok
        (and (is-inside-figure? l x-to-ys u d)
             (is-inside-figure? (inc l) x-to-ys u d)
             (is-inside-figure? r x-to-ys u d)
             (is-inside-figure? (dec r) x-to-ys u d)

             (is-inside-figure? u y-to-xs l r)
             (is-inside-figure? (inc u) y-to-xs l r)
             (is-inside-figure? d y-to-xs l r)
             (is-inside-figure? (dec d) y-to-xs l r))))))

(defn rectangles [valid-fn [x y] tiles]
  (for [[tx ty] tiles
        :when (valid-fn [x y] [tx ty])]
    (* (inc (abs (- tx x))) (inc (abs (- ty y))))))

(defn max-rectangle [valid-fn t tiles]
  (let [rs (rectangles valid-fn t tiles)]
    (if (seq rs)
      (apply max rs)
      0)))

(defn part-1-pm [data valid-fn]
  (apply max (mapv (fn [p] (max-rectangle valid-fn p data)) data)))

(defn part-1 []
  (part-1-pm (get-data nil) (constantly true)))


(defn part-2-pm [t]
  (let [data (get-data t)
        lines (make-lines data)
        [y-to-xs x-to-ys] (lines-for-ray lines)
        y-to-xs (merge-points-and-lines y-to-xs)
        x-to-ys (merge-points-and-lines x-to-ys)
        _ (println "ray-maps-done")
        valid-fn (valid-fn-gen y-to-xs x-to-ys)]
    (part-1-pm data valid-fn)))

(defn part-2 []
  (part-2-pm nil))


(defn svg-line [[[x1 y1] [x2 y2]]]
  (let [n 10]
    (format "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"black\" stroke-width=\"10\" />\n"
            (int (/ x1 n)) (int (/ y1 n)) (int (/ x2 n)) (int (/ y2 n)))))

(defn make-svg [data]
  (spit "/tmp/svg"
    (apply str
      (let [lines (make-lines data)]
        (mapv svg-line lines)))))


