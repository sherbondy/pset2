I did problems 1 and 2 using clojure.
To play with them, download
[Leiningen](https://github.com/technomancy/leiningen).

Then run:
```
cd code/ps2
lein repl
```

From the repl, try copying and pasting the commented-out code (denoted by ";")
in markov.clj and str_graph.clj. For example:

```
(def vce (make-initial-graph "../../dickens_reads.txt"))
(def collapsed-ve (apply collapse-chains vce))
(make-pdf-graph collapsed-ve "trimmed-graph")
```

Enjoy!
