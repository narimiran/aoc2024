^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day03
  {:nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc :collapsed}
  (:require
   aoc
   [clojure.string :as str]
   [nextjournal.clerk :as clerk]))


;; # Day 3: Mull It Over
;;
;; It seems that my hunch from [yesterday](../day02) was correct:
;; this year (AoC's 10th anniversary) we'll be re-visiting some
;; old AoC locations. I like it!
;;
;; Today we're in the North Pole Toboggan Rental Shop and we need to help
;; them with their computer with corrupted memory.
;;
;; Today I'll do both parts at the same time, so if you haven't solved part 2
;; yet: beware, **here be ~~dragons~~ spoilers**!



;; ## Input parsing
;;
;; We need to help the computer with multiplying some numbers inside an
;; instruction that looks like this: `mul(a,b)`, where `a` and `b` are
;; multi-digit numbers.\
;; For part 2, we also need to consider two additional instructions:
;; > - The `do()` instruction enables future `mul` instructions.
;; > - The `don't()` instruction disables future `mul` instructions.
;;
;; In the task there are two slightly different examples for each part, but
;; we'll use the second example, as it is still valid for part 1:
;;
(def example "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

;; The main question from this task is a very simple one:
;; Do you know regex?
;;
;; Knowing how to write the regex to find the instructions is the majority of
;; the work needed to solve this task.\
;; The patterns for part 2 are easier to write: we only need to match the
;; full pattern.
;; For part 1, we also need to extract numbers inside of the `mul` instructions
;; so we enclose each in a set of parentheses:
;;
(def pattern
  (let [mul-patt  #"mul\((\d+),(\d+)\)"
        do-patt   #"do\(\)"
        dont-patt #"don't\(\)"]
    (re-pattern (str/join "|" [mul-patt do-patt dont-patt]))))


;; We are interested in all matches, so we use the
;; [`re-seq` function](https://clojuredocs.org/clojure.core/re-seq) to get a
;; list of them all:
;;
(def example-instructions (re-seq pattern example))

;; Our pattern works correctly for all three valid instructions.\
;; For each instruction the first element is the full pattern,
;; and for the `mul` instructions the 2nd and 3rd elements are the numbers
;; we need to mupliply.
;; We can work with that!
;; Let's proceed to the real input:
;;
(def instructions (re-seq pattern (aoc/read-input 3)))






;; ## Solution
;;
;; Now that we have a list of all valid instructions, we need to go through
;; it and calculate the sum of those multiplications.
;;
;; For part 1 we do that unconditionally, and for part 2 we add the result
;; of a multiplication only if we're in the `enabled` state.\
;; To do conditional updates of our state, we'll use
;; the [`cond->` macro](https://clojuredocs.org/clojure.core/cond-%3E).
;;
;; Once we went through all the instructions, we're only interested in the
;; sums for each part.
;; We can extract those keys from the final state using
;; the [`juxt` function](https://clojuredocs.org/clojure.core/juxt).\
;; Writing `((juxt f1 f2) xs)` is an equivalent of `[(f1 xs) (f2 xs)]`.
;; Exactly what we need here!
;;
(defn solve [instructions]
  (->> (reduce (fn [state [instr a b]]
                 (case instr
                   "don't()" (assoc state :enabled false)
                   "do()"    (assoc state :enabled true)
                   (let [mult (* (parse-long a) (parse-long b))]
                     (cond-> state
                       true             (update :p1-sum + mult)
                       (:enabled state) (update :p2-sum + mult)))))
               {:enabled true
                :p1-sum 0
                :p2-sum 0}
               instructions)
      ((juxt :p1-sum :p2-sum))))


(solve example-instructions)
(solve instructions)



;; ## Visualization
;;
;; There are lots of invalid instructions in our input.
;; Let's see how frequently they're used.
;;
(let [all-instructions     (re-seq #"[a-z][a-z'/]+" (aoc/read-input 3))
      valid-instructions   #{"mul" "do" "don't"}
      invalid-instructions (remove valid-instructions all-instructions)]
  (clerk/plotly
     {:config {:displayModeBar false}
      :data [{:y invalid-instructions
              :type :histogram
              :opacity 0.7}]
      :layout {:margin {:l 80 :r 0 :t 10 :b 30}
               :bargap 0.05}}))

;; 🥚 We found an easter egg!!\
;; Eric, the AoC creator, stated in the past that he's a big fan `perl`, and he's
;; using it to create these tasks.




;; ## Conclusion
;;
;; The difficulty for this task depends on how familiar/comfortable you are
;; with regex.\
;; Once you successfully parse the instructions, the rest of the task is a breeze.
;;
;; With these instructions I'm getting a slight IntCode flashback
;; (IntCode was the theme of [AoC 2019](https://adventofcode.com/2019)).\
;; Time for another prediction:
;; Will some of today's invalid instructions become valid in some future task?
;;
;; Today's highlights:
;; - `re-seq`: list of all regex matches
;; - `cond->`: conditional threading
;; - `juxt`: create a list of function applications to a single argument



^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [instructions (re-seq pattern input)]
    (solve instructions)))
