(ns str-graph.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  
  (:use     [clojure.java.shell :only [sh]]))

;; Some comments: This is written in clojure, a lisp that sits atop the jvm.
;; for more info, see: http://www.clojure.org
;; I'm assuming 1a contains a typo: "three *letter* string" => *word*

(defn add-elem! [a k v]
  (let [existing-k (@a k)]
    (let [swap-val (if existing-k
                     (conj existing-k v)
                     [v])]
      (swap! a assoc k swap-val))))

(defn str-of-range [arr rstart & [rend]]
  (let [rend (if rend
               (min (count arr) (inc rend))
               (count arr))]
    (str/join " " (map #(nth arr %)
                       (range rstart rend)))))

(defn ps-len
  "The length of a prefix/suffix"
  [words]
  (- (count words) 2))

(defn str-prefix [words]
  (str-of-range words 0 (ps-len words)))

(defn str-suffix [words]
  (str-of-range words (- (dec (count words))
                         (ps-len words))))

(str-suffix ["it" "was" "the"])

(defn fill-fixes [filename]
  (let [text (slurp filename)
        lines (str/split text #"\n")
        prefixes (atom {})
        suffixes (atom {})
        V        (atom [])]
    (for [i (range (count lines))]
      (let [line (lines i)]
        (swap! V conj line)
        (let [words (str/split line #"\s+")]
          (let [prefix     (str-prefix words)
                suffix     (str-suffix words)]
            (add-elem! prefixes prefix i)
            (add-elem! suffixes suffix i))))))
  [@V @prefixes @suffixes])

(defn make-initial-graph [filename]
  (let [[V prefixes suffixes] (fill-fixes filename)
        E (atom {})]
    (doseq [[suffix from-vs] suffixes]
      (let [to-vs (prefixes suffix)]
        (doseq [from-v from-vs to-v to-vs]
          (add-elem! E from-v to-v))))
    [V @E]))

((make-initial-graph "dickens_reads.txt") 1)

{0 [2 7 10 15 24], 2 [3], 3 [11 18], 4 [5 13],
 5 [6 14 22 23], 6 [2 7 10 15 24], 7 [8 26],
 8 [9 21], 9 [4 12], 10 [3], 11 [4 12], 12 [5 13],
 13 [6 14 22 23], 14 [2 7 10 15 24], 15 [16 27],
 16 [1 17 25], 17 [19], 18 [4 12], 19 [20],
 20 [6 14 22 23], 21 [4 12], 22 [2 7 10 15 24],
 23 [2 7 10 15 24], 24 [16 27], 26 [9 21], 27 [1 17 25]}

(defn graphviz [V E]
  (apply str
         (flatten [["digraph G {\n"]
                   (for [node (range (count V))]
                     (str "\t" "node" node
                          " [label=\"" (V node) "\"]"
                          ";\n"))

                   (for [[node siblings] E]
                     (for [sibling siblings]
                       (str "\t" "node" node " -> " "node" sibling ";\n")))
                   ["}"]])))

(graphviz @V @E)

(defn make-pdf-graph [V E graph-name]
  (let [graph-file (str "data/" graph-name ".dot")
        graph-pdf  (str/replace graph-file "dot" "pdf")]
    (with-open [wrtr (io/writer graph-file)]
      (.write wrtr (graphviz V E)))
  
    (sh "dot" "-Tpdf" graph-file "-o" graph-pdf)))

(make-pdf-graph @V @E "testing")

;; Now the idea is to trim things that could only belong to one parent
;; and to remove anything that doesn't follow a chain