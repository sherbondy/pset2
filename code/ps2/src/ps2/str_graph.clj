(ns ps2.str-graph
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as s])
  
  (:use     [clojure.java.shell :only [sh]]))

;; Some comments: This is written in clojure, a lisp that sits atop the jvm.
;; for more info, see: http://www.clojure.org
;; I'm assuming 1a contains a typo: "three *letter* string" => *word*

(defn add-elem!
  "Expects an atom representing a map whose values are sets as input.
   Adds elem v to the set associated with k."
  [a k v]
  (let [existing-k (@a k)]
    (let [swap-val (if existing-k
                     (conj existing-k v)
                     #{v})]
      (swap! a assoc k swap-val))))

(defn str-array [str-or-array]
  (if (string? str-or-array)
    (str/split str-or-array #"\s+")
    str-or-array))

(defn str-of-range
  "Returns a string of the elements from arr
   in the range (rstart..rend) joined by spaces."
  [arr rstart & [rend]]
  (let [arr (str-array arr)]
    (let [rend (if rend
                 (min (count arr) (inc rend))
                 (count arr))]
      (str/join " " (map #(nth arr %)
                         (range rstart rend))))))

(defn ps-len
  "The length of a prefix/suffix"
  [words]
  (- (count (str-array words)) 2))

(defn str-prefix [words]
  (str-of-range words 0 (ps-len words)))

(defn str-suffix [words]
  (str-of-range words (- (dec (count (str-array words)))
                         (ps-len words))))

(defn last-word [words]
  (last (str-array words)))

; (str-prefix ["it" "was" "the"])
; (str-prefix "it was the")

(defn fill-fixes
  "Populate the prefix and suffix maps"
  [filename]
  (let [text (slurp filename)
        lines (str/split text #"\n")
        prefixes (atom {})
        suffixes (atom {})
        V        (into [] (set lines))
        v-counts (into {} (map-indexed (fn [idx v]
                                         [idx (count (filter #(= v %) lines))])
                                       V))]
    (doseq [i (range (count V))]
      (let [line (V i)]
        (let [words (str/split line #"\s+")]
          (let [prefix     (str-prefix words)
                suffix     (str-suffix words)]
            (add-elem! prefixes prefix i)
            (add-elem! suffixes suffix i)))))
  [V v-counts @prefixes @suffixes]))

; (def VCPS (fill-fixes "../../dickens_reads.txt"))

(defn make-initial-graph [filename]
  (let [[V v-counts prefixes suffixes] (fill-fixes filename)
        E (atom {})]
    (doseq [[suffix from-vs] suffixes]
      (let [to-vs (prefixes suffix)]
        (doseq [from-v from-vs to-v to-vs]
          (add-elem! E from-v to-v))))
    [V v-counts @E]))

; (def vce (make-initial-graph "../../dickens_reads.txt"))

;; {0 #{6}, 1 #{12}, 2 #{9 13 14}, 3 #{9 13 14}, 5 #{0 4}, 6 #{7}, 7 #{2},
;; 8 #{15}, 9 #{1}, 10 #{8}, 11 #{2}, 12 #{15}, 13 #{10}, 14 #{5}, 15 #{11}}

(defn remove-transitive-overlaps [V C E]
  (let [E (atom E)
        v-range (range (count V))]
    (doseq [x v-range
          y v-range
          z v-range]
      (if (and (contains? (@E x) y)
               (contains? (@E y) z))
        (let [new-set (disj (@E x) z)]
          (swap! E assoc x new-set))))
  [V C @E]))

; (apply remove-transitive-overlaps vce)

;; {0 #{6}, 1 #{12}, 2 #{9 13 14}, 3 #{9 13 14}, 5 #{0 4}, 6 #{7}, 7 #{2},
;; 8 #{15},9 #{1}, 10 #{8}, 11 #{2}, 12 #{15}, 13 #{10}, 14 #{5}, 15 #{11}}

(defn in-degrees [V E]
  (let [v-count (count V)
        in-counts (int-array (repeat v-count 0))]
    (doseq [v (range v-count)]
      (doseq [in-v (E v)]
        (aset in-counts in-v
              (inc (aget in-counts in-v)))))
    (into [] in-counts)))

(defn out-degrees [V E]
  (let [v-count (count V)
        out-counts (int-array (repeat v-count 0))]
    (doseq [v (range v-count)]
      (aset out-counts v (count (E v))))
    (into [] out-counts)))

(defn follow-chain [m v in-degs out-degs]
  (loop [chain [v]]
    (let [chain-end (last chain)
          val (first (m chain-end))]
      (if (and val (= 1 (out-degs chain-end)
                        (in-degs val)))
        (recur (conj chain val))
        chain))))

(defn drop-ends [sequence]
  (drop 1 (drop-last sequence)))

(defn collapse-chains [V C E]
  ;; determine where the chains are
  (let [chains (atom {})
        collapse (atom #{})
        vrange (range (count V))
        in-degs (in-degrees V E)
        out-degs (out-degrees V E)]
    (doseq [v vrange]
      (let [chain (follow-chain E v in-degs out-degs)
            chain-end (last chain)]
        (swap! chains assoc v chain)
        (if (< 1 (count chain))
          (swap! collapse s/union (set (drop 1 chain))))))

    ;; now that we've determined which values should be collapsed
    ;; let's actually do the collapse operation and concatenate the vertex labels
    (let [aE (atom {})
          aV (atom V)]
      (doseq [v vrange]
        (if (not (contains? @collapse v))
          (let [vchain    (@chains v)
                chain-end (last vchain)]
            (if (< 1 (count vchain))
              (do
                (swap! aE assoc v (E chain-end))
                (swap! aV assoc v
                       (str (V (first vchain)) " "
                            (str/join " " (map #(last-word (V %))
                                               (rest vchain))))))
              (if-let [old-val (E v)]
                (swap! aE assoc v old-val)
                (swap! aV assoc v (V v)))))))

      [@aV C @aE])))

(defn derive-paths [V C E]
  ;; run augmented bfs to determine the longest path that visits all edges
  (let [in-degs  (in-degrees V E)
        out-degs (out-degrees V E)
        start (first (filter #(and (= 0 (in-degs %))
                                   (< 0 (out-degs %)))
                             (range (count V))))
        initial-path [[start]]]
    (loop [q initial-path]
      (let [new-q
            (filter #(not (nil? %))
              (apply concat
                   (for [path q]
                     (let [end (last path)
                           vs  (E end)]
                       (if vs
                         (for [v vs]
                           (let [new-path (conj path v)]
                             (if (< (count (filter #(= v %) path)) (C v))
                               new-path)))
                         [path])))))]
        (if (not= q new-q)
          (recur new-q)
          q)))))

(defn graphviz [V C E]
  (let [in-degs (in-degrees V E)
        out-degs (out-degrees V E)]
    (apply str
           (flatten [["digraph G {\n"]
                     (for [node (range (count V))]
                       (if (or (< 0 (in-degs node))
                               (< 0 (out-degs node)))
                         (str "\t" "node" node
                              " [label=\"" (str node ". " (V node)) " *" (C node) "*\"]"
                              ";\n")))

                     (for [[node siblings] E]
                       (for [sibling siblings]
                         (str "\t" "node" node " -> " "node" sibling ";\n")))
                     ["}"]]))))

; (apply graphviz vce)

(defn make-pdf-graph [[V C E] graph-name]
  (let [graph-file (str "data/" graph-name ".dot")
        graph-pdf  (str/replace graph-file "dot" "pdf")]
    (with-open [wrtr (io/writer graph-file)]
      (.write wrtr (graphviz V C E)))
  
    (sh "dot" "-Tpdf" graph-file "-o" graph-pdf)))

; (def collapsed-ve (apply collapse-chains vce))

; (make-pdf-graph vce "initial-graph")
; (make-pdf-graph collapsed-ve "trimmed-graph")


(defn viable-path-values [V C E]
  (let [viable-paths (derive-paths V C E)
        max-path-count (apply max (map count viable-paths))
        max-paths (filter #(= (count %) max-path-count) viable-paths)]
    (for [path max-paths]
      (let [pcount (count path)]
        (loop [i 1 sentence [(V (first path))]]
          (if (< i pcount)
            (let [vi (nth path i)
                  added-str (str-of-range (V vi) 2)]
              (recur (inc i) (conj sentence added-str)))

            (str (str/join " " sentence) ".")))))))
        
; (apply viable-path-values collapsed-ve)

;; "It was the best of times, it was the best of times, it was the age of wisdom, it was the age of foolishness."
;; "It was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness."
;; "It was the best of times, it was the age of wisdom, it was the best of times, it was the age of foolishness."
;; "It was the best of times, it was the age of wisdom, it was the worst of times, it was the age of foolishness."
;; "It was the worst of times, it was the best of times, it was the age of wisdom, it was the age of foolishness."
;; "It was the worst of times, it was the age of wisdom, it was the best of times, it was the age of foolishness."
;; "It was the age of wisdom, it was the best of times, it was the best of times, it was the age of foolishness."
;; "It was the age of wisdom, it was the best of times, it was the worst of times, it was the age of foolishness."
;; "It was the age of wisdom, it was the worst of times, it was the best of times, it was the age of foolishness."

;; As you can see, I am unable to single out the actual sequence from the collapsed graph.
;; All of the above are valid given the available information, due to repeats present in the
;; original sequence.
