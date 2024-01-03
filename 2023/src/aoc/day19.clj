(ns aoc.day19
  (:require [clojure.string :as s]
            [aoc.utils      :as u]
            [clojure.pprint :as pp]))

(defn get-data [& args] 
  (->> (u/get-day-data 19 identity (first args)) 
       (partition-by #{""})
       (remove #(= "" (first %)))))

(defn parse-action [sa]
  (cond
    (= sa "A")
    {:action :accept}

    (= sa "R")
    {:action :reject}

    (re-matches #"^[a-z]+$" sa)
    {:action {:next sa}}))


(defn parse-rule [rs]
  (let [[r a] (s/split rs #":")]

    (or (parse-action r)
        (let [[n op v] (drop 1 (re-matches #"([a-z]+)([^[a-z0-9]])([0-9]+)" r))
              v (parse-long v)]
          (merge
            {:cond [n ({"<" (fn [x] (< x v)) ">" (fn [x] (> x v))} op)]
             :cond-save [n op v]}
            (parse-action a))))))

(defn parse-pipeline [ps]
  (let [name (first (s/split ps #"\{"))
        body (second (re-matches #".*\{([^\}]+)\}.*" ps)) 
        rules (mapv parse-rule (s/split body #","))]
    {name rules}))    
    
;(parse-pipeline "px{a<2006:qkq,m>2090:A,rfg}")
  
(defn parse-part [ps]
  (reduce-kv (fn [a k v] (assoc a (name k) v)) 
             {}
             (read-string (s/replace ps #"=" " "))))

(defn check-condition [cond part]
  (let [[v f] cond
        v (part v)]  
    (f v)))

(defn run-workflow [rulesp p]
  (loop [rules rulesp]
    (when (nil? rules) (throw (Exception. "Out of rules " rulesp)))
    (let [{cond :cond action :action cond-save :cond-save} (first rules)]
      (println "cond " cond "cond save" cond-save "action" action "part" p)
      (if (or (nil? cond) (check-condition cond p))
        (do 
          (println "condition satisfied, going to action" action)
          action)
        (do
          (println "condition not satisfied, next rule")
          (recur (next rules)))))))


(defn follow-workflows [workflows part]
  (loop [current-workflow "in"]
    (println "current workflow: " current-workflow)
    (let [wkf (workflows current-workflow)
          result (run-workflow wkf part)]
      (if (#{:accept :reject} result)
        [result part]
        (recur (:next result)))))) 
    

(defn part-1 [& args]
  (let [[pipelines parts] (get-data (first args))
        pipelines (apply merge (mapv parse-pipeline pipelines))
        parts (mapv parse-part parts)
        flow-result (mapv (fn [p] (follow-workflows pipelines p)) parts)]
    (->> flow-result
         (keep (fn [[r p]] (when (= r :accept) p)))
         (map (fn [p] (reduce (fn [a [_ v]] (+ a v)) 0 p)))
         (reduce + 0))))

; ---------------------------------

(defn apply-formula [[v op n] ranges]
  (let [range (ranges v)
        formulas {"<"  (fn [v] (< v n))
                  ">"  (fn [v] (> v n))}] 
    (assoc ranges v (filter (formulas op) range)))

 (defn tree-from-pipelines [wks wk-name]
  (let [resolve-action (fn [label {cond :cond-save next-act :action}]
                         (if (keyword? next-act)
                            [label cond next-act]
                            [label cond (tree-from-pipelines wks (:next next-act))]))
        conds (mapv #(dissoc % :cond) (get wks wk-name))
        [conds [unsat-cond & _]] (split-at (dec (count conds)) conds)]

     (conj
       (mapv (fn [cnd] (resolve-action :sat cnd)) conds)
       (resolve-action :unsat (assoc unsat-cond :cond-save (mapv :cond-save conds)))))))


(defn calc-ranges [ranges cond-fn vn]
  (if cond-fn
    (let [rng (get ranges vn)
          split-range (group-by cond-fn rng)
          sat-range (get split-range true)
          sat-ranges (assoc ranges vn sat-range)
          unsat-range (get split-range false) 
          next-ranges (assoc ranges vn unsat-range)]
      [sat-ranges next-ranges])
    [ranges nil]))

(defn gen-walk-pipelines [wks]
  (fn walk-pipeline [wk-name ranges]
    (let [wk (wks wk-name)]
      (loop [ranges ranges accepted-ranges '() rules wk]
        (if rules
          (let [{[vn cond-fn] :cond action :action} (first rules)
                [sat-ranges next-ranges] (calc-ranges ranges cond-fn vn)]
            (cond 
              (= action :reject)
              (recur next-ranges accepted-ranges (next rules))

              (= action :accept)
              (recur next-ranges (cons sat-ranges accepted-ranges) (next rules))

              :else
              (let [{next-wk :next} action]
                (recur next-ranges 
                       (concat accepted-ranges (walk-pipeline next-wk sat-ranges)) 
                       (next rules)))))
          accepted-ranges)))))
      

(defn part-2 [ftype & _]
  (let [[pipelines _] (get-data ftype)
        pipelines (apply merge (mapv parse-pipeline pipelines))
        init-range (range 1 4001)
        _ (println pipelines)
        accepted-ranges ((gen-walk-pipelines pipelines) "in" {"x" init-range "m" init-range "a" init-range "s" init-range})]
    (apply + 
      (map (fn [m] (apply * (map #(count (second %)) m))) 
           accepted-ranges)))) 

