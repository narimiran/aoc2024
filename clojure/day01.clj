^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day01
  {:nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc :collapsed}
  (:require
   [aoc-utils.core :as aoc]
   [nextjournal.clerk :as clerk]))


;; # Day 1: Historian Hysteria
;;
;; Finally! AoC 2024 is here!
;;
;; Let's see what's the theme this year...
;;
;; It seems this year we'll have to answer one question:\
;; **_Where is ~~Jessica Hyde~~ Chief Historian?_**
;;
;; Ok, let's help Senior Historians with today's task.



;; ## Input parsing
;;
;; We are holding two lists side by side:
;;
(def example
  "3   4
   4   3
   2   5
   1   3
   3   9
   3   3")

;; Each line consists of two numbers, separated by multiple spaces.
;;
;; Fortunately, I have a helper function `aoc/integers` (if you haven't already,
;; take a look at my [aoc-utils library](https://narimiran.github.io/aoc-utils/),
;; lots of goodies there which will get used throughout these tasks), which extracts
;; all integers from a line, and I can invoke it when parsing the input
;; with `:ints`.\
;; Next, we have to transform a list of lines into a list of columns.
;; This is what `aoc/transpose` does.
;; Finally, for the first part, we need to `sort` both columns, so we'll
;; do that here too.
;;
(defn parse-data [input]
  (->> (aoc/parse-lines input :ints)
       aoc/transpose
       (mapv sort)))

(def example-columns (parse-data example))


;; The example looks correct, so we can do the same for the real input:
;;
^{:nextjournal.clerk/auto-expand-results? false}
(def columns (parse-data (aoc/read-input 1)))





;; ## Part 1
;;
;; Our task for the first part is:
;; > pair up the numbers and measure how far apart they are.
;;
;; We have sorted columns already, so for each pair of numbers we need to find
;; the difference between the numbers.\
;; What the example doesn't show is that a number on the left can
;; be larger than a number on the right, so we also need to take `abs` of
;; their difference.
;; We can do that by `comp`-osing those two functions.
;;
;; The `map` function also works with multiple collections, taking the first
;; element of each collection, then the second element of each, etc. — exactly
;; what we need.\
;; After we calculate all the differences, we need to take their sum.
;;
(defn part-1 [[left right]]
  (->> (map (comp abs -) right left)
       (reduce +)))

(part-1 example-columns)
(part-1 columns)







;; ## Part 2
;;
;; For part 2 we need to:
;; > figure out exactly how often each number from the left list appears in the right list.
;;
;; and then:
;; > Calculate a total similarity score by adding up each number in the left list
;; > after multiplying it by the number of times that number appears in the right list




;; ### Version 1: The naive way
;;
;; We'll go through every number in the left column and calculate its occurrences
;; in the right column.
;;
;; To count all values which satisfy a predicate, I use the `aoc/count-if`
;; function. It is, in essence, `(count (filter ...))`.\
;; Also, I have the `aoc/sum-map` helper function which will sum up all the values
;; after applying a `map` to a collection. Similar to `(reduce + (map ...))`.
;;
(defn part-2-v1 [[left right]]
  (aoc/sum-map
   (fn [nr] (* nr (aoc/count-if #{nr} right)))
   left))


(part-2-v1 example-columns)
(part-2-v1 columns)




;; ### Version 2: Frequencies
;;
;; You'll notice that in the version 1 of the code, for each number in the left
;; column we iterate through the whole right column each time.
;; Since both columns are of the same size (`n` elements), this means we're
;; doing `n*n` iterations.
;;
;; While this year my aim is not to chase every bit of performance, and
;; inefficiency won't cause us any trouble in this task, let's pick this
;; low-hanging fruit and improve our solution.
;;
;; The built-in [`frequencies function`](https://clojuredocs.org/clojure.core/frequencies)
;; will be of great help.
;; We'll use it on the right column to calculate how many times each
;; number appears.\
;; There might be numbers from the left column which don't appear at all in
;; the right column, so we pass the `0` as the default value for such cases.
;;
(defn part-2-v2 [[left right]]
  (let [right-freqs (frequencies right)]
    (aoc/sum-map
     (fn [nr] (* nr (right-freqs nr 0)))
     left)))

(part-2-v2 example-columns)
(part-2-v2 columns)


;; ### Performance comparison
;;
;; Let's see how both versions compare.
;; We'll use the [`criterium` library](https://github.com/hugoduncan/criterium)
;; for benchmarking.
;;
;; ```
;; (require '[criterium.core :as c])
;;
;;
;; (c/quick-bench (part-2-v1 columns))
;;
;; Evaluation count : 30 in 6 samples of 5 calls.
;;              Execution time mean : 22.253470 ms
;;     Execution time std-deviation : 50.561843 µs
;;    Execution time lower quantile : 22.206475 ms ( 2.5%)
;;    Execution time upper quantile : 22.327963 ms (97.5%)
;;                    Overhead used : 1.835833 ns)
;;
;;
;;
;; (c/quick-bench (part-2-v2 columns))
;;
;; Evaluation count : 4848 in 6 samples of 808 calls.
;;              Execution time mean : 132.993876 µs
;;     Execution time std-deviation : 12.297005 µs
;;    Execution time lower quantile : 124.482119 µs ( 2.5%)
;;    Execution time upper quantile : 151.412166 µs (97.5%)
;;                    Overhead used : 1.835833 ns)
;; ```

;; More than 150 times faster!!\
;; Not bad, not bad at all for such a small and simple change.





;; ## Visualization
;;
;; Let's see if we can plot the distribution of the values in the right column.
;;
(let [right-freqs (frequencies (second columns))]
  (clerk/plotly
   {:config {:displayModeBar false}
    :data [{:x (map key right-freqs)
            :y (map val right-freqs)
            :type :bar}]
    :layout {:margin {:l 20 :r 0 :t 0 :b 20}}}))

;; As expected, most of the elements have one occurrence, with few exceptions
;; easily visible in the graph.\
;; The winner is `77689` with 20 occurrences.






;; ## Conclusion
;;
;; Much easier Day 1 than we had last year.
;; And I like that.
;; These first days should be about having fun, not about chasing some edge
;; cases (_hello `oneight` my old friend..._).
;;
;; Faster tasks mean more time for writing this "blog", exploring
;; different solutions, making visualizations, etc.
;; I hope this will continue in the next days too.
;;
;; Today's highlights:
;; - `aoc/transpose`: get a list of columns from a list of rows
;; - `comp`: compose two functions
;; - `frequencies`: count the number of appearances of items







^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [data (parse-data input)]
    [(part-1 data)
     (part-2-v2 data)]))
