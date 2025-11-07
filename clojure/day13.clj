^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day13
  {:nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc :collapsed}
  (:require
   [aoc-utils.core :as aoc]
   [nextjournal.clerk :as clerk]))


;; # Day 13: Claw Contraption
;;
;; Today we're on a tropical island, and in the lobby of a resort there are
;; lots of claw machines.\
;; They have two buttons, `A` and `B`, which move the claw different amount
;; in the x- and y- directions.
;; We win a prize if, after N amount of presses, we are at the exact
;; x-y location of the prize.
;;
;; For example:
;;
(def example "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279")


;; Here we have four different machines.\
;; To press button `A` we need 3 tokens, to press button `B` we need 1 token.
;; Our task is to find the minimum amount of tokens needed to win all
;; possible prizes (on some machines it is impossible to win a prize).





;; ## Input parsing
;;
;; Each machine is in its own paragraph, so the `aoc/parse-paragraphs`
;; function will come handy.
;; The description format is always the same, and we're only interested
;; in the numbers for each machine:
;;
(defn parse-data [input]
  (aoc/parse-paragraphs input :ints))

(def example-data (parse-data example))
(def data (parse-data (aoc/read-input 13)))




;; ## Math
;;
;; The task for Part 1 says:
;; > You estimate that each button would need to be pressed no more
;; > than **100 times** to win a prize.
;;
;; Which, if you're familiar with AoC, looks like a bait.
;; You should immediately get a hint that for Part 2 the numbers will be
;; much higher and we won't be able to check every single combination
;; until we find a solution.
;;
;; For each machine, we need to solve a system of two equations with
;; two unknowns:
;; ```
;; ax*a + bx*b = px
;; ay*a + by*b = py
;; ```
;; where `a` and `b` are unknown amounts of button presses for button `A`
;; and button `B`, respectively.\
;; For each machine, `ax` and `ay` are the movements for each press of
;; button `A`, and `bx` and `by` are the movements for button `B`.
;; The prize is at `[px py]`.
;;
;; At first, I solved these equations on paper, manually doing the
;; subtitutions.
;;
;; There is another way (well, it is the same stuff, just written/calculated
;; a bit differently).
;; This system can also been written as:
;;
^{:nextjournal.clerk/visibility {:code :hide}}
(clerk/tex "
\\begin{bmatrix}
a_x & b_x \\\\
a_y & b_y
\\end{bmatrix}
\\begin{bmatrix}
a \\\\
b
\\end{bmatrix}
=
\\begin{bmatrix}
p_x \\\\
p_y
\\end{bmatrix}
")


;; We can solve this system using
;; [Cramer's rule](https://en.wikipedia.org/wiki/Cramer's_rule#Applications).
;; (Last time I've used it, I was in highschool, 20+ years ago!)
;;
;; We need to find three determinants:
;; - `det-M`, determinant of the original matrix
;; - `det-a`, we substitute `ax` and `ay` in the original matrix with
;;   `px` and `py` and find a determinant
;; - `det-b`, we substitute `bx` and `by` in the original matrix with
;;   `px` and `py` and find a determinant
;;
^{:nextjournal.clerk/visibility {:code :hide}}
(clerk/tex "
{\\det}_M
=
\\begin{vmatrix}
a_x & b_x \\\\
a_y & b_y
\\end{vmatrix}

\\hspace{1 cm}

{\\det}_a
=
\\begin{vmatrix}
p_x & b_x \\\\
p_y & b_y
\\end{vmatrix}

\\hspace{1 cm}

{\\det}_b
=
\\begin{vmatrix}
a_x & p_x \\\\
a_y & p_y
\\end{vmatrix}
")

;; To get the solution for `a` and `b`, we need to divide the determinants:
;; `det-a / det-M` and `det-b / det-M`, respectively.






;; ## Solution
;;
;; We have all we need to solve the equations.
;;
;; For our task, valid solutions are only those for which `a` and `b` are integers.
;; (We cannot press a button 0.1 times :P).\
;; If a solution is valid, we multiply the presses of button `A` by their
;; price of 3 tokens, and add presses of `B` (costing 1 token):
;;
(defn det [a1 a2 b1 b2]
  (- (* a1 b2) (* b1 a2)))

(defn solve-eq [pos-incr [[ax ay] [bx by] [px py]]]
  (let [px (+ pos-incr px)
        py (+ pos-incr py)
        det-M (det ax ay bx by)
        det-a (det px py bx by)
        det-b (det ax ay px py)
        a (/ det-a det-M)
        b (/ det-b det-M)]
    (if (or (ratio? a) (ratio? b))
      0
      (+ (* 3 a) b))))


;; The only difference between Part 1 and Part 2 is that for the second part
;; we need to increase the coordinates of a prize by `10000000000000` in
;; each direction:
;;
(defn solve [data]
  (vec
   (for [pos-incr [0 10000000000000]]
     (aoc/sum-map (partial solve-eq pos-incr) data))))


(solve example-data)
(solve data)





;; ## Conclusion
;;
;; Today was mainly a math task.
;;
;; If you know how to solve a system of two linear equations with two unknowns,
;; the hardest thing to code (if you don't have some helpers ready ;))
;; is to separate the input into different groups and parse integers in each.




^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [data (parse-data input)]
    (solve data)))
