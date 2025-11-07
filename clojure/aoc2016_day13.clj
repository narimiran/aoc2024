^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns aoc2016-day13
  {:nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc :collapsed}
  (:require [aoc-utils.core :as aoc]
            [nextjournal.clerk :as clerk]))

;; # Testing graph traversal helper before AoC 2024, pt. 1
;;
;; ## AoC 2016, Day 13
;;
;; > The cube maze starts at `0,0` and seems to extend infinitely
;; > toward positive `x` and `y`.
;; >
;; > You can determine whether a given x,y coordinate will be a wall
;; > or an open space using a simple system:
;; > - Find `x*x + 3*x + 2*x*y + y + y*y`.
;; > - Add the office designer's favorite number (your puzzle input).
;; > - Find the binary representation of that sum; count the number of bits that are 1.
;; >   - If the number of bits that are 1 is even, it's an open space.
;; >   - If the number of bits that are 1 is odd, it's a wall.
;; >
;; > &nbsp;
;; >
;; > - Part 1: What is the fewest number of steps required for you to reach `31,39`?
;; > - Part 2: How many locations can you reach in at most 50 steps?
;;
;; [Full task](https://adventofcode.com/2016/day/13)



;; ## Input and forumula
;;
(def input 1364)

(defn formula [x y]
  (+ (* x x)
     (* 3 x)
     (* 2 x y)
     y
     (* y y)
     input))


;; ## Open space
;;
;; To calculate if some coordinate if an open space, we need to convert
;; the number given by the `formula` to its binary representation and
;; see if there is an even number of `1` bits.
;;
(defn open-space? [[x y]]
  (->> (formula x y)
       Integer/toBinaryString
       (aoc/count-if #{\1})
       even?))


;; ## Graph properties
;;
;; The grid we're trying to navigate is an infinite one, and we're given
;; a starting and ending point.
;; We don't need to pre-calculate walls and open spaces for each coordinate,
;; we'll do it on the fly with the `open-space?` function for the points
;; we need.
;;
(def graph-properties
  {:start [1 1]
   :end [31 39]
   :size ##Inf
   :nb-cond open-space?})


;; ## Part 1
;;
;; Part 1 is simple: we need to find minimal number of steps between the
;; starting and the ending point.
;; Either `aoc/bfs` or `aoc/a-star` would do.
;; I chose the latter because the default heuristic I implemented is
;; to pick the points closer to the end, which allows us to explore the graph faster,
;; i.e. with less visits to the points further away from the end.
;;
(def pt1 (-> graph-properties
             aoc/a-star
             :steps))





;; ## Part 2
;;
;; For part 2, we can use same algorithm and same graph properties as for
;; part 1, with the difference that we don't have the ending point.
;; Instead, the ending condition is when we reach 50 steps from the start.
;; We're interested in a number of all locations visited along the way.
;;
(def pt2 (-> graph-properties
             (dissoc :end)
             (assoc :steps-limit 50)
             aoc/bfs
             :count))



;; ## Visualizing the path
;;
;; We'll use Plotly to visualize the path from the start to the end.
;; We need to separate x's and y's for Plotly to use them:
;;
(defn extract-coords [points]
  {:x (map first points)
   :y (map second points)})


;; We will plot all the walls in the area and the path we took:
;;
(def walls
  (extract-coords
   (for [x (range 35)
         y (range 45)
         :when (not (open-space? [x y]))]
     [x y])))

(def path (-> graph-properties
              aoc/a-star
              :path
              extract-coords))


;; We have the data we need, now we just need to tweak a bit
;; some plot parameters and that's it:
;;
(let [axes-common {:ticks ""
                   :showticklabels false
                   :showgrid false
                   :zeroline false}]
  (clerk/plotly
   {:config {:displayModeBar false
             :displayLogo false}
    :data [(merge walls
                  {:mode :markers
                   :marker {:symbol :square
                            :size 8}})
           (assoc path :line {:width 6})]
    :layout {:xaxis axes-common
             :yaxis (merge axes-common {:autorange :reversed})
             :margin {:l 0 :r 0 :t 0 :b 0}
             :showlegend false}}))





^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
[pt1 pt2]
