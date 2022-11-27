(ns aoc.day18
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as cset]
            [clojure.edn :as edn]))

(defn get-data []
  (let [data (u/get-day-data 18 edn/read-string)
        data (vec data)]
    data))


(comment
  (defn cex [m] (-> m to-matrix explode from-matrix))
  (explode [ [1 0] [2 4] [9 4] [5 3]])
   
  ; [[[[[9,8],1],2],3],4] becomes [[[[0,9],2],3],4] 
  (cex [[[[[9,8],1],2],3],4])
  ; [7,[6,[5,[4,[3,2]]]]] becomes [7,[6,[5,[7,0]]]]
  (cex [7,[6,[5,[4,[3,2]]]]])
  ; [[6,[5,[4,[3,2]]]],1] becomes [[6,[5,[7,0]]],3] 
  (cex [[6,[5,[4,[3,2]]]],1])
  ; [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]] becomes [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]
  (cex [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]])
  ; [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]] becomes [[3,[2,[8,0]]],[9,[5,[7,0]]]]
  (cex [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]])
    
  (from-matrix (split (to-matrix [5 [11 1]]))) 
  
  (from-matrix (reduce-snail-number (to-matrix [[[[[4,3],4],4],[7,[[8,4],9]]] [1 1]]))))



(comment
  (reduce-once [[[[[9,8],1],2],3],4] 0 0 nil)
  (defn cex [a] (reduce-once a 0 0 nil))

  (cex [[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]] [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]))

       ;= [[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]
  


(comment
  (if (> b 9
       {:ret [(quot b 2) (+ (quot b 2) (rem b 2))]
        :ops [:split]
        :idx (dec idx)}
       {:ret b
        :ops ops
        :idx (inc idx)})))


(defn calculate-explode [t i lev ops]
  (let [ops-fn (fn [b idx lev ops]
                 ;(println "in ops-fn b=" b idx lev ops)
                 (if ops
                   {:ret b :ops ops :idx idx}
                   (if (number? b)
                     {:ret b
                      :ops ops
                      :idx (inc idx)}
                     (let [{ops :ops ret :ret idx :idx :as res} (calculate-explode b idx (inc lev) ops)]
                       res)))) 
                     
        [l r] t] 
    (if (> lev 3)
      (do
        (when (or (coll? l) (coll? r))
          (throw (Exception. (str "Pair in level " lev " is not number pair: " t))))
        {:ret 0
         :idx (inc i) 
         :ops [:add [[(dec i) l] [(inc i) r]]]})
      (let [{lops :ops lidx :idx lret :ret} (ops-fn l i    lev ops)
            {rops :ops ridx :idx rret :ret} (ops-fn r lidx lev (or ops lops))]
        {:ret [lret rret] :ops (or ops lops rops) :idx ridx}))))    
        
(defn apply-explode [[l r] idx ops]
  (let [[l lidx] (if (number? l)
                   [(reduce (fn [a [i v]] (if (= idx i) (+ a v) a)) l ops) (inc idx)]
                   (apply-explode l idx ops))] 
    (let [[r ridx] (if (number? r)
                     [(reduce (fn [a [i v]] (if (= lidx i) (+ a v) a)) r ops) (inc lidx)]
                     (apply-explode r lidx ops))]
      ;(println "got l r" l r)
      [[l r] ridx])))
    
(comment
  (split [[[[0,7],4],[15,[0,13]]],[1,1]] false))
  

(defn split [[l r] occured]
  (let [bfn (fn [b]
              (if (number? b)
                (if (> b 9)
                  [[(quot b 2) (+ (quot b 2) (rem b 2))] true]
                  [b false])
                (split b false)))]
                
    (if occured
      [[l r] occured]
      (let [[lres loccured] (bfn l)]
        (if loccured
          [[lres r] true]
          (let [[rres rocurred] (bfn r)]
            [[lres rres] rocurred]))))))
    
(defn explode [m]
  (loop [m m]
    (let [{ret :ret ops :ops} (calculate-explode m 0 0 nil)]
      (if ops
        (let [[ret _] (apply-explode ret 0 (second ops))]
          (recur ret))
        m))))
  
 
(defn reduce-snail-number [n]  
  (loop [n n]
    (let [ret (explode n)
          [sret split-occured?] (split ret false)]
      (if split-occured?
        (recur sret)
        sret))))
     
(defn snail-add [a b] 
  (reduce-snail-number [a b]))

(defn magnitude [[l r]]
  (+
    (* 2 (if (number? r)
           r
           (magnitude r)))
    (* 3 (if (number? l)
           l
           (magnitude l)))))
  
    
(defn run-1 [] 
  (magnitude
    (reduce snail-add (get-data))))
   
 
(defn run-2 []
  (reduce (fn [a v] (max a 
                         (magnitude (reduce-snail-number v)))) 
          0
          (for [a (get-data) b (get-data)
                :when (not= a b)]
            [a b])))
               
