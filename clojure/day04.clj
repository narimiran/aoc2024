^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day04
  {:nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc :collapsed}
  (:require [aoc-utils.core :as aoc]))


;; # Day 4: Ceres Search
;;
;; We're not on Earth any more!
;; Now we're on the Ceres monitoring station and it xmas-time.
;;
;; An Elf wants us to help with her word search, so let's do that.
;; (The search for Chief Historian can wait a bit, I guess.)





;; ## Input parsing
;;
;; We are given a word search that looks like this:
(def example "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

;; We need to find how many times the word `XMAS` appears there, and we need
;; to search in 8 directions.
;;
;; We'll convert input into a 2D vector of characters:
;;
(def example-grid (aoc/parse-lines example :chars))
(def grid (aoc/parse-lines (aoc/read-input 4) :chars))


;; ## Part 1
;;
;; We will go through all positions in our input and check if, starting from
;; there, we can find the word `XMAS` in any of 8 directions.\
;; This we can do with a Cartesian product between three `dx` possibilities
;; and three `dy` possibilities.
;; (This also gives us 9th "direction", when both deltas are zero, which will
;; never match the word we're looking for, so we're just leave it in.)
;;
;; Once again, I'm using one of my helpers.
;; This time it is `do-count`, which is a simple macro allowing me to write
;; an equivalent of `(count (for [... ...] 1))`, without unnecessary creating
;; a list of ones, just to count the number of them.\
;; Anything you can write in a `seq-exprs` (the stuff inside of `[ ... ]`)
;; of a [`for`](https://clojuredocs.org/clojure.core/for) or a
;; [`do-seq`](https://clojuredocs.org/clojure.core/doseq) (see some examples
;; on those links), you can also write here; for example, use `:when`:
;;
(defn part-1 [grid]
  (let [letters (map-indexed vector "XMAS")
        height (count grid)
        width (count (first grid))]
    (aoc/do-count
     [y (range height)
      x (range width)
      :let [c (aoc/grid-get grid x y)]
      :when (= \X c)
      dx [-1 0 1]
      dy [-1 0 1]
      :when (every? (fn [[i l]]
                      (= l (aoc/grid-get grid (+ x (* i dx)) (+ y (* i dy)))))
                    letters)])))

;; If the position we're at isn't a letter `X`, we won't even bother to
;; check its neighbours: if the first `:when` is false,
;; the inner loops are skipped.\
;; Otherwise, we check all the directions for a potential `XMAS`.
;; Using the [`every?` function](https://clojuredocs.org/clojure.core/every_q)
;; means we will short-circuit on the first letter that doesn't satisfy the predicate.
;;
(part-1 example-grid)
(part-1 grid)






;; ## Part 2
;;
;; > The Elf looks quizzically at you. Did you misunderstand the assignment?
;;
;; Well, my dear Elf, to be honest, I initially misunderstood the first part too!
;; I didn't read the task description carefully and I was searching only in
;; four directions.
;; But ok, nobody will know about that.
;;
;; > this isn't actually an `XMAS` puzzle; it's an `X-MAS` puzzle
;;
;; There are four ways to have X-MAS:
;; ```
;; M.S   M.M   S.S   S.M
;; .A.   .A.   .A.   .A.
;; M.S   S.S   M.M   S.M
;; ```
;;
;; As we can see, each diagonal will contain either `MAS` or `SAM`.\
;; We will check all points to see if their diagonals satisfy this condition:
;;
(defn part-2 [grid]
  (let [diags #{(vec "MAS") (vec "SAM")}
        deltas [-1 0 1]
        height (count grid)
        width (count (first grid))]
    (aoc/do-count
     [y (range height)
      x (range width)
      :when (and (diags (for [d deltas]
                          (aoc/grid-get grid (+ x d) (+ y d))))
                 (diags (for [d deltas]
                          (aoc/grid-get grid (- x d) (+ y d)))))])))


;; The only difference between two diagonals is if the deltas for `x` and `y`
;; have the same or the opposite sign. (Notice the `(- x d)` in the second one.)
;;
(part-2 example-grid)
(part-2 grid)






;; ## Conclusion
;;
;; Today no visualizations, sorry.\
;; Hopefully, it's an exception and it doesn't mean that from now on
;; the visualizations are... wait for it... _no más_.
;;
;; Today's highlights:
;; - `:when`: write conditions inside of `for`/`doseq`
;; - `every?`: check if all elements of a collection satisfy a predicate







^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [grid (aoc/parse-lines input :chars)]
    [(part-1 grid)
     (part-2 grid)]))
