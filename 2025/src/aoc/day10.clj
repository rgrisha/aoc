(ns aoc.day10
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn strip-ends [s]
  (subs s 1 (dec (.length s))))

(defn map-btn [s]
  (mapv #(Integer/parseInt %) (s/split (strip-ends s) #",")))

(defn line-fn [s]
  (let [ss (s/split s #" ")
        sch   (first ss)
        jolts (last ss)
        btns  (drop 1 ss)
        btns  (take (dec (count btns)) btns)]
    {:target (mapv {\. 0 \# 1} (strip-ends sch))
     :btns   (mapv map-btn btns)
     :jolts  (map-btn jolts)}))

(defn get-data [t]
  (u/get-day-data 10 line-fn t))

(defn gen-apply-button [flip-fn]
  (fn [state button]
    (println "in apply btn fn" state button)
    (reduce (fn [a e]
              (update a e flip-fn))
            state
            button)))

(defn binary-flip [n]
  (if (= n 0) 1 0))

(defn next-generation-for-state [appl-button-fn state buttons]
  (mapv (fn [b] (appl-button-fn state b))  buttons))

(defn generations-until-target-found [target buttons]
  (let [apply-button-fn (gen-apply-button binary-flip)]
    (loop [states #{(mapv (constantly 0) target)} generation 0]
      (if (contains? states target)
        generation
        (recur
          (->> states
               (map (fn [state] (next-generation-for-state apply-button-fn state buttons)))
               (apply concat)
               (into #{}))
          (inc generation))))))

(defn part-1-pm [t]
  (let [data (u/get-day-data 10 line-fn t)]
    (apply +
      (map (fn [{:keys [target btns]}] (generations-until-target-found target btns)) data))))


(defn part-1 []
  (part-1-pm nil))


(defn apply-btn-2 [state button]
  (reduce (fn [a e] (update a e inc)) state button))

(defn compare-state [state target]
  (cond
    (every? true? (map = state target))
    :eq

    (every? true? (map <= state target))
    :less

    :else
    :gt))

(comment defn walk-jolts-generation [g target buttons state-stack]
  (when buttons
    (println "current state: " state-stack)
    (let [next-st (apply-btn-2 (first state-stack) (first buttons))
          cmp-res (compare-state next-st target)]
      (case cmp-res
        :eq   (inc g)
        :gt   (walk-jolts-generation g target (next buttons) state-stack)
        :less (walk-jolts-generation (inc g) target buttons (cons next-st state-stack))))))

(defn walk-jolts-generation [g target buttons state]
  (when-let [button (first buttons)]
    (let [next-st (apply-btn-2 state button)
          cmp-res (compare-state next-st target)]
      (case cmp-res
        :eq   (inc g)
        :gt   (walk-jolts-generation g target (next buttons) state)
        :less (or (walk-jolts-generation (inc g) target buttons next-st)
                  (walk-jolts-generation g target (next buttons) state))))))



(defn generations-for-jolts [buttons target]
  (let [zero-state  (mapv (constantly 0) target)
        buttons-st  (reverse (sort-by count buttons))]
    (walk-jolts-generation 0 target buttons-st zero-state)))


(defn part-2-pm [t]
  (let [data (u/get-day-data 10 line-fn t)]
    (doseq [d data]
      (println
        ;(map (fn [{:keys [btns jolts]}] (generations-for-jolts btns jolts)) data)
        (generations-for-jolts (:btns d) (:jolts d))))))

