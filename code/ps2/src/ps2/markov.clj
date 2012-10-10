(ns ps2.markov
  (:use [clojure.math.numeric-tower]))

(def fact
  (memoize
   (fn [x]
     (loop [n x acc 1]
       (if (< n 1)
         acc
         (recur (dec n) (* n acc)))))))

(defn n-choose [n k]
  (int (/ (fact n) (* (fact k) (fact (- n k))))))

(n-choose 4 2)
;; 6

(defn alignment-score [bases]
  (let [base-counts (atom {})]
    (doseq [base bases]
      (swap! base-counts assoc base
             (inc (or (@base-counts base) 0))))
    (reduce +
            (for [[k v] @base-counts]
              (int (n-choose v 2))))))

(alignment-score [:A :A :C :C])
;; 2

(def es
  {:N {0 0.1,  1 0.35, 2 0.25, 3 0.2, 6 0.1}
   :C {0 0.05, 1 0.15, 2 0.2,  3 0.3, 6 0.3}})

(get-in es [:N 0])
;; 0.1

(defn rand-emissions [state n]
  (let [probs      (es state)
        hun-scores (flatten (for [[k v] probs]
                              (repeat (int (* 100 v)) k)))]
    (for [i (range n)]
      (rand-nth hun-scores))))
      
(rand-emissions :C 5)

(defn p-scores|state [scores state]
  (apply * (map #(get-in es [state %]) scores)))

(p-scores|state [3 3 3] :C)
;; 0.027
(p-scores|state [6 2 3 0] :N)
;; 5e-4


(def alignment-1
  (map alignment-score
       [[:A :C :T :A] [:C :A :T :G] [:G :G :C :A] [:A :A :C :T]  [:C :C :T :G]
        [:G :G :C :T] [:A :C :T :G] [:C :T :G :A] [:T :G :A :C] [:A :A :T :T]]))

(def alignment-2
  (map alignment-score
       [[:A :A :T :A] [:C :A :C :C] [:A :A :A :A] [:A :A :T :T] [:C :C :C :C]
        [:G :G :G :T] [:A :A :A :A] [:G :A :G :A] [:T :T :T :C] [:A :A :T :T]]))

alignment-1 ;; (1 0 1 1 1 1 0 0 0 2)
alignment-2 ;; (3 3 6 2 6 3 6 2 3 2)

(for [a [alignment-1 alignment-2]]
  (for [s [:N :C]]
    (p-scores|state a s)))

(p-scores|state alignment-2 :N)

;; p(N|a1),                |   p(C|a1)                
;; 1.313046875E-7          |   9.4921875E-11
;;
;; p(N|a2),                |   p(C|a2)
;; 2.5000000000000012E-8   |   1.7496000000000002E-6

