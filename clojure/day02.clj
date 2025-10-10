^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day02
  {:nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc :collapsed}
  (:require
   aoc
   [nextjournal.clerk :as clerk]))



;; # Day 2: Red-Nosed Reports
;;
;; Today we're visiting _the Red-Nosed Reindeer nuclear fusion/fission plant_.\
;; The engineers gave us a list of reports with _unusual data_ and we need to
;; find how many reports are valid.
;;



;; ## Input parsing
;;
;; Just like [yesterday](../day01), we have multiple integers per line:
;;
^{:nextjournal.clerk/visibility {:result :hide}}
(def example
  "7 6 4 2 1
   1 2 7 8 9
   9 7 6 2 1
   1 3 2 4 5
   8 6 4 4 1
   1 3 6 7 9")

;; Today we're not dealing with columns, so all we have to do is extract
;; all the numbers from a line.
;; My [aoc module](../aoc) makes that a breeze:
;;
(defn parse-data [input]
  (aoc/parse-lines input :ints))

(def example-reports (parse-data example))
(def reports (parse-data (aoc/read-input 2)))






;; ## Part 1
;;
;; > The engineers are trying to figure out which reports are safe.
;; > The Red-Nosed reactor safety systems can only tolerate levels that are
;; > either gradually increasing or gradually decreasing.
;; > So, a report only counts as safe if both of the following are true:
;; > - The levels are either all increasing or all decreasing.
;; > - Any two adjacent levels differ by at least one and at most three.
;;
;; In other words:
;; either the adjacent levels are _increasing_ by `{1, 2, 3}` or they are
;; _decreasing_ by those values.
;;
;; We can use the built-in [`partition` function](https://clojuredocs.org/clojure.core/partition)
;; to give us a list of adjacent levels.\
;; Then we can calculate the differences between the adjacent levels and
;; we're ready to find if a report is valid or not.
;;
(defn safe? [line]
  (let [adjacent-levels (partition 2 1 line)
        differences (for [[a b] adjacent-levels] (- b a))]
    (or
      (every? #{ 1  2  3} differences)
      (every? #{-1 -2 -3} differences))))


;; There are multiple ways we could have written the predicate function.
;; I chose to use a set as a predicate function, as I find it more elegant
;; than writing e.g. `(fn [d] (<= 1 d 3))` or similar.
;;
;; Now it is a matter of counting how many lines satisfy this condition.
;; Once again, I'll use my `aoc/count-if` helper, but the same could've be done
;; with `(count (filter ...))`.
;;
(aoc/count-if safe? example-reports)
(aoc/count-if safe? reports)






;; ## Part 2
;;
;; For part 2, we should
;; > tolerate a single bad level in what would otherwise be a safe report.
;;
;; This means that a line is either valid (per part 1 rules) or we need
;; to find if there is a single bad level in there.\
;; We need to check each element in the report
;; until we find the problematic one (_if_ there's only one problematic).
;;
;; Initially I have written this using `reduce`, but it felt wrong using it
;; since we're not accumulating anything.
;; We're just trying to find a single index which makes the report safe,
;; so I've rewritten it using `loop`:
;;
(defn single-bad-bit? [line]
  (loop [idx (dec (count line))]
    (let [pre   (subvec line 0 idx)
          post  (subvec line (inc idx))
          line' (into pre post)]
      (cond
        (safe? line') idx
        (zero? idx)   false
        :else         (recur (dec idx))))))

;; We recursively check each index if that's the one with a problematic level.
;; (The decision to start from the last index and go backwards was purely
;; cosmetic.)\
;; To split our line, we're using the built-in
;; [`subvec` function](https://clojuredocs.org/clojure.core/subvec),
;; which claims to be very fast `O(1)` operation.
;;
;; We have two exit conditions: either we've found the problematic level
;; (and we return its index, we'll see why a bit later) or we've reached
;; the end of the line without finding it.
;;
;; And now the complete rule for part 2 is:
;;
(defn safe-2? [line]
  ((some-fn safe? single-bad-bit?) line))

;; Using the [`some-fn` function](https://clojuredocs.org/clojure.core/some-fn) is an
;; equivalent of using `or` on each function result (both will short-circuit, i.e.
;; if the first function returns `true`, the second one is not evaluated):
;;
^{:nextjournal.clerk/visibility {:result :hide}}
(fn [line]
  (or (safe? line)
      (single-bad-bit? line)))


;; Once again we need to count the lines that satisfy the rule and we're done:
;;
(aoc/count-if safe-2? example-reports)
(aoc/count-if safe-2? reports)






;; ## Single-pass solution
;;
;; Even though I said I won't be chasing performance this year, once again
;; I can't help myself.
;;
;; Now that we have all the functions we need, we can write a solution which
;; will give us an answer for both parts in one pass through the input data:
;;
(defn solve-both [reports]
  (reduce (fn [[pt1 pt2] line]
            (let [safe-pt1? (safe? line)
                  safe-pt2? (or safe-pt1?
                                (single-bad-bit? line))]
              [(+ pt1 (if safe-pt1? 1 0))
               (+ pt2 (if safe-pt2? 1 0))]))
          [0 0]
          reports))

(solve-both example-reports)
(solve-both reports)





;; ## Visualization
;;
;; We have `400 - 334 = 66` situations where we have a single problematic
;; level.
;; Let's find which indexes are most and least likely to have the problematic level.
;;
;; This is why earlier I've made the `single-bad-bit?` function to return the index:
;; any truthy value would do, so why not make it useful.
;;
(let [problematic-reports (filter (every-pred (complement safe?)
                                              single-bad-bit?)
                                  reports)
      problematic-indexes (mapv single-bad-bit? problematic-reports)]
  (clerk/plotly
   {:config {:displayModeBar false}
    :data [{:x problematic-indexes
            :type :histogram
            :opacity 0.7}]
    :layout {:margin {:l 20 :r 0 :t 0 :b 20}
             :bargap 0.05}}))

;; It seems like index 2 is a clear winner: it was problematic only once.

;; The [`every-pred` function](https://clojuredocs.org/clojure.core/every-pred)
;; is an `and`-version of the `some-fn` function used before
;; (This one short-circuits on the first `false` result).\
;; Alternatively, the multiple predicates for `filter` could have been written like this:
;;
^{:nextjournal.clerk/visibility {:result :hide}}
(filter (fn [x]
          (and (not (safe? x))
               (single-bad-bit? x)))
        reports)




;; ## Conclusion
;;
;; We're still in the easy land.
;; Let's hope we'll stay here few more days, giving me more time
;; to write all this.
;;
;; We had a nice reference to an earlier AoC task.
;; Will this be a recurring theme this year, especially since we're in the
;; company of Senior Historians?
;;
;; Today's highlights:
;; - `partition`: with `step = 1`, get a list of adjacent elements
;; - `subvec`: efficient splitting of a vector
;; - `some-fn`/`every-pred`: write `or`/`and` chains of functions





^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [reports (parse-data input)]
    (solve-both reports)))
