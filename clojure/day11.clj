^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day11
  {:nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc :collapsed}
  (:require [aoc-utils.core :as aoc]
            [clojure.math :as math]))



;; # Day 11: Plutonian Pebble
;;
;; We're visiting Pluto and there is _a strange set of physics-defying stones._
;; Each time we blink, each stone changes according to the following rules:
;; - `0` --> `1`
;; - even number of digits --> split in half
;; - else --> multiply the number by `2024`
;;
;; For example:
;;
(def example "125 17")

;; After one blink, `125` would become `253000`, and `17` would be split
;; into `1` and `7`.







;; ## Input parsing
;;
;; Since we can travel through time, I can already see what will happen in
;; Part 2, so let's immediately prepare for it.
;;
;; After several blinks, we will start to see familiar numbers.
;; There's no point in calculating the result of a blink if we already know
;; the result from earlier.
;;
;; There are several ways we could do that.
;; The obvious one is to use memoization.
;; But I had another idea: let's keep track how many times each number appears.
;; Enter the [`frequencies` function](https://clojuredocs.org/clojure.core/frequencies):
;;
(defn parse-data [input]
  (-> (aoc/parse-input input :ints)
      frequencies))


;; At the beginning, each number appears only once:
;;
(def example-data (parse-data example))
(def data (parse-data (aoc/read-input 11)))






;; ## Solution
;;
;; Let's start with the rules we're given.
;;
;; The only interesting rule is when we have a number with even number of
;; digits.\
;; Initially I've implemented counting the digits as `((comp count str) n)`,
;; i.e. converting a number to a string and then take a length of the string.
;; Simple, but since we'll be doing this many times (in this task, but we
;; might also need it in future tasks), I wrote a helper `aoc/count-digits`,
;; which does it via `math/log10` and is ~40% faster.
;;
(defn rules [stone]
  (let [len (aoc/count-digits stone)]
    (cond
      (zero? stone) 1
      (even? len) (let [half (long (math/pow 10 (/ len 2)))]
                    [(quot stone half)
                     (mod stone half)])
      :else (* 2024 stone))))



;; Every time we blink, the existing stones will transform into new ones.
;; We start with a map of quantities of each stone, and we'll also return such
;; a map of new stones and their quantities.
;; This will allow us to call repeatedly the same function for each blink.
;;
;; For each stone, we get the new stone (`stone'`), and we _add_ its amount
;; to the accumulation map (`acc`) we'll return.
;; Since the accumulation map might not contain the key for the stone we're adding,
;; we need to use the [`fnil` function](https://clojuredocs.org/clojure.core/fnil),
;; and pass it `0` as a default value when a key is not in the map.
;;
;; Out of three rules, two of them return a number, but one returns a vector with two
;; numbers, so we need to special-case it by checking if the return value is
;; a `vector?` and add frequencies for each number in it to the accumulator map.
;;
(defn blink [stones]
  (let [+= (fnil + 0)]
    (reduce
     (fn [acc [stone freq]]
       (let [stone' (rules stone)]
         (if (vector? stone')
           (-> acc
               (update (stone' 0) += freq)
               (update (stone' 1) += freq))
           (update acc stone' += freq))))
     {}
     stones)))


;; To blink multiple times, we need to repeatedly call the `blink` function.
;; We have the [`iterate` function](https://clojuredocs.org/clojure.core/iterate)
;; that gives us an infinite sequence of repeated calls to the same function,
;; i.e. `(x, (f x), (f (f x)), ...)`.
;; We are interested only in the [`nth` element](https://clojuredocs.org/clojure.core/nth)
;; of that sequence:
;;
(defn nth-blink [n stones]
  (-> (iterate blink stones)
      (nth n)))

;; The example after 6 blinks looks like this:
;;
(nth-blink 6 example-data)

;; Which looks correct.
;; We have two occurrences of `0`, `40` and `48`, and four occurrences of `2`.



;; We need to find how many stones we will have after 25 (Part 1) and 75 (Part 2)
;; blinks:
;;
(defn solve [stones n]
  (->> stones
       (nth-blink n)
       (aoc/sum-map val)))


(mapv #(solve example-data %) [25 75])
(mapv #(solve data %) [25 75])






;; ## Conclusion
;;
;; Beware of AoC tasks with small amount of text in Part 2! :)
;;
;; If you solved the [Lanternfish task (AoC 2021, Day 6)](https://adventofcode.com/2021/day/6),
;; then this one should be a very familiar theme and you'll know how to deal
;; with the exponential explosion.
;;
;; Today's highlights:
;; - `fnil`: provide a default value for `nil`
;; - `iterate`: call a function multiple times







^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [data (parse-data input)]
    (mapv #(solve data %) [25 75])))
