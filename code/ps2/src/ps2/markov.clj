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

; (n-choose 4 2)
;; 6

(defn alignment-score [bases]
  (let [base-counts (atom {})]
    (doseq [base bases]
      (swap! base-counts assoc base
             (inc (or (@base-counts base) 0))))
    (reduce +
            (for [[k v] @base-counts]
              (int (n-choose v 2))))))

; (alignment-score [:A :A :C :C])
;; 2

(def es
  {:N {0 0.1,  1 0.35, 2 0.25, 3 0.2, 6 0.1}
   :C {0 0.05, 1 0.15, 2 0.2,  3 0.3, 6 0.3}})

; (get-in es [:N 0])
;; 0.1

(defn rand-emissions [state n]
  (let [probs      (es state)
        hun-scores (flatten (for [[k v] probs]
                              (repeat (int (* 100 v)) k)))]
    (for [i (range n)]
      (rand-nth hun-scores))))
      
; (rand-emissions :C 5)

(defn p-scores|state [scores state]
  (apply * (map #(get-in es [state %]) scores)))

; (p-scores|state [3 3 3] :C)
;; 0.027
; (p-scores|state [6 2 3 0] :N)
;; 5e-4

(def alignment-1
  (map alignment-score
       [[:A :C :T :A] [:C :A :T :G] [:G :G :C :A] [:A :A :C :T]  [:C :C :T :G]
        [:G :G :C :T] [:A :C :T :G] [:C :T :G :A] [:T :G :A :C] [:A :A :T :T]]))

(def alignment-2
  (map alignment-score
       [[:A :A :T :A] [:C :A :C :C] [:A :A :A :A] [:A :A :T :T] [:C :C :C :C]
        [:G :G :G :T] [:A :A :A :A] [:G :A :G :A] [:T :T :T :C] [:A :A :T :T]]))

; alignment-1 ;; (1 0 1 1 1 1 0 0 0 2)
; alignment-2 ;; (3 3 6 2 6 3 6 2 3 2)

; (for [a [alignment-1 alignment-2]]
;   (for [s [:N :C]]
;     (p-scores|state a s)))

; (p-scores|state alignment-2 :N)

;; p(N|a1),                |   p(C|a1)                
;; 1.313046875E-7          |   9.4921875E-11
;;
;; p(N|a2),                |   p(C|a2)
;; 2.5000000000000012E-8   |   1.7496000000000002E-6

;; It's much more likely that a1 came from N, and that a2 came from C

(defn simulate-sequences [trials n state]
  (let [wins (atom {:C 0 :N 0})]
    (letfn [(rand-trial []
              (rand-emissions state n))]
     (doseq [_ (range trials)]
       (let [scores (rand-trial)
             [pc pn] (for [s [:C :N]] (p-scores|state scores s))
             winner (if (> pc pn) :C :N)]
         (swap! wins assoc winner (inc (@wins winner))))))
    @wins))

; (simulate-sequences 10000 10 :N)
;; b. The probability of a false positive p(C|sequence) > p(N|sequence)
;; is surprisingly high (> 10%):
;; Relative frequences ~ N = 8745 : C = 1255

; (simulate-sequences 10000 10 :C)
;; c. Likewise: N = 1416, C = 8584

;; d. There's a substantial chance for error when classifying based
;; on a simple likelihood estimate

;; e. This would allow us to make a better approximation using Bayes' rule:
;; The background probability described = P(C). P(N) = 1 - P(C), and from here,
;; since we know P(seq|C) and P(seq|N), we could determine the conditional
;; probabilities P(C|seq) and P(N|seq):
;; P(N|seq) = P(seq|N)*P(N) / P(seq)


