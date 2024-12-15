(ns aoc.day13
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn int2 [[_ i1 i2]]
  [(Integer. i1) (Integer. i2)])

(defn get-data [mul t]
  (let [data (u/get-day-data 13 identity t)
        rex #".*: X[+=](\d+), Y[+=](\d+)"]

    (loop [data data acc []]
      (if (seq data)
        (let [[a b p] (take 3 data)
              [ax ay] (int2 (re-matches rex a))
              [bx by] (int2 (re-matches rex b))
              [px py] (int2 (re-matches rex p))]
          (recur (drop 4 data) (conj acc {:ax ax :ay ay :bx bx :by by :px (+ mul px) :py (+ mul py)})))
        acc))))

(defn brute-force-buttons [{:keys [ax ay bx by px py :as m]}]
  (for [a (range 1 101)
        b (range 1 101)
        :when (and
                (= px (+ (* a ax) (* b bx)))
                (= py (+ (* a ay) (* b by))))]
    (+ (* 3 a) b)))

(defn part-1 [& t]
  (->> (get-data 1 (first t))
       (map brute-force-buttons)
       (filter seq)
       (map (fn [l] (apply min l)))
       (apply +)))

(defn backwards-try-vector-a [{:keys [ax ay bx by px py :as m]}]
  (let [start-step (min (quot px ax) (quot py ay))
        max-steps (max (* ax bx) (* ay by))]
    (loop [step 0]
      (when (and (< step max-steps) (< step start-step))
        (let [main-vec-steps (- start-step step)
              x (* main-vec-steps ax)
              y (* main-vec-steps ay)
              dx (- px x)
              dy (- py y)
              vec-2-count (quot dx bx)]
          ;(println "maxst" max-steps main-vec-steps step x y dx dx vec-2-count)
          (if (and
                 (= dy (* by vec-2-count))
                 (= dx (* bx vec-2-count)))
            [main-vec-steps vec-2-count]
            (recur (inc step))))))))

(defn backwards-try-vector-b [{:keys [ax ay bx by px py :as m]}]
  (let [start-step (min (quot px bx) (quot py by))
        max-steps (max (* ax bx) (* ay by))]
    (loop [step 0]
      (when (and (< step max-steps) (< step start-step))
        (let [main-vec-steps (- start-step step)
              x (* main-vec-steps bx)
              y (* main-vec-steps by)
              dx (- px x)
              dy (- py y)
              vec-2-count (quot dx ax)]
          ;(println "maxst" max-steps main-vec-steps step x y dx dx vec-2-count)
          (if (and
                 (= dy (* ay vec-2-count))
                 (= dx (* ax vec-2-count)))
            [main-vec-steps vec-2-count]
            (recur (inc step))))))))

(defn loop-vector-a [{:keys [ax ay bx by px py :as m]}]
  (loop [step 0]
    (when (= 0 (mod step 100000))
      (println "x"))
    (let [x (* step ax)
          y (* step ay)
          dx (- px x)
          dy (- py y)
          v2-steps (quot dx bx)]
      (if (= dx (* v2-steps bx))
        (if (= dy (* v2-steps by))
          [step v2-steps]
          (recur (inc step)))
        (recur (inc step))))))


;(backwards-try-vector-a {:ax 24, :ay 90, :bx 85, :by 62, :px 10000000006844, :py 10000000006152})
;(backwards-try-vector-a {:ax 17, :ay 86, :bx 84, :by 37, :px 7870, :py 6450})
;(loop-vector-a {:ax 17, :ay 86, :bx 84, :by 37, :px 7870, :py 6450})


;(part-2)
(defn part-2 [& t]
  (->> (get-data 10000000000000 (first t))))
       ;(map backwards-try-vector-b)
       ;(filter seq)))
       ;(map (fn [[a b]] (+ b (* 3 a))))
       ;(apply +)))

