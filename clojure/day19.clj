^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day19
  {:nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc :collapsed}
  (:require
   [aoc-utils.core :as aoc]
   [clojure.string :as str]))


;; # Day 19: Linen Layout
;;
;; Today we're at the hot sprins on Gear Island.
;; We are given a list of towel patterns (first line of the input) and
;; a list of desired designs.
;; It looks like this:
;;
(def example "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb")


;; ## Input parsing
;;
;; From the first line we need to extract the patterns, which are separated
;; by `, ` (comma, space).
;; The designs are each on their own line, nothing to do there:
;;
(defn parse-data [input]
  (let [[[patterns] designs] (aoc/parse-paragraphs input)]
    [(str/split patterns #", ")
     designs]))

(def example-data (parse-data example))
(def data (parse-data (aoc/read-input 19)))







;; ## Solution
;;
;; Both parts share 99% of the same logic, so we'll write a solution that
;; works on both parts.
;;
;; For each design we need to check if it can be created from the provided
;; patterns (Part 1) and how many different ways there are to create
;; it (Part 2).
;;
;; The idea is to check each design recursively.
;; We start with a complete design (e.g. `abcdef`), then we `filter` the
;; list of patterns to keep only those that the design `starts-with?`.
;;
(defn valid-patterns [patterns design]
  (filter #(str/starts-with? design %) patterns))

(valid-patterns ["a" "ab" "b" "bc"] "abcdef")



;; We check the remaining part of the design
;; (e.g. if a pattern was `ab`, we now check `cdef`) recursively.\
;; If we have come to the end of a design (i.e. the design is `empty?`), it
;; means that we found a successful way to create a design.
;;
;; We will encounter the same part of a design multiple times (we can reach
;; it with different pattern combinations, e.g. we reach `cdef` either
;; by matching either the `ab` pattern or by matching
;; "first `a` then `b`" patterns).
;; There's no need to repeatedly check if it'll be successful: we've already
;; calculated it before.
;;
;; To keep a cache of already seen situations, we will use the
;; [`memoize` function](https://clojuredocs.org/clojure.core/memoize).
;;
(def count-ways
  (memoize
   (fn [patterns design]
     (if (empty? design)
       1
       (->> design
            (valid-patterns patterns)
            (aoc/sum-map (fn [valid-patt]
                           (count-ways patterns
                                       (subs design (count valid-patt))))))))))



;; Once we get all the ways each design can be created, for Part 1 we
;; need to count only those which have at least one (`pos?`) way to be created,
;; and for Part 2 we're interested in the total number of all possible ways
;; to create the designs.
;;
(defn solve [[patterns designs]]
  (let [possible-designs (mapv #(count-ways patterns %) designs)]
    [(aoc/count-if pos? possible-designs)
     (reduce + possible-designs)]))


(solve example-data)
(solve data)





;; ## Conclusion
;;
;; Another suspiciously easy task, less than a week until the end.
;; That's now two easy ones in a row.
;; I'm afraid of what will this weekend bring.
;;
;; Today's highlight:
;; - `memoize`: cache results of a function
;; - `subs`: create a substring







^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [data (parse-data input)]
    (solve data)))
