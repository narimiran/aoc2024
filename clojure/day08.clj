^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day08
  {:nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc :collapsed}
  (:require
   [aoc-utils.core :as aoc]
   [clojure.set :as set]
   [nextjournal.clerk :as clerk]))





;; # Day 8: Resonant Collinearity
;;
;; We are on the roof of Easter Bunny Headquarters, and we realize the antenna
;; there is just one of many across the city.
;;
;; We create a map of all the antennas which looks like this:
;;
(def example "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")



;; Each (non-dot) character represents a different frequency.\
;; Our task is to find "antinodes": positions on the grid that are on the same
;; line as two antennas of a same frequency, but twice as far from one
;; antenna than its pair.\
;; If this sounds confusing, it's because it is.
;; Fortunately, we are provided with an example:
;;
;; ```
;; ..........
;; ...#......
;; ..........
;; ....a.....
;; ..........
;; .....a....
;; ..........
;; ......#...
;; ..........
;; ..........
;; ```
;;
;; Here the positions marked with `a` are the original antennas,
;; and the positions marked with `#` are antinodes.\
;; Hopefully, this clears things up a bit.






;; ## Input parsing
;;
;; We need to extract the locations of all antennas.
;;
;; Once again my [aoc-utils library](https://narimiran.github.io/aoc-utils/) is useful.
;; The `aoc/grid->point-map` function takes a predicate as an optional second
;; argument, so it'll extract only the points which satisfy the predicate.
;; We're interested in all non-dot characters, and we can write that negation
;; using the [`complement` function](https://clojuredocs.org/clojure.core/complement).
;;
;; We need to limit our search to be inside the existing grid, so we'll also
;; extract its size.
;;
(defn parse-data [input]
  (let [lines (aoc/parse-lines input)
        size (count lines)
        antennas (aoc/grid->point-map lines (complement #{\.}))]
    [size antennas]))


(def example-data (parse-data example))
(def data (parse-data (aoc/read-input 8)))







;; ## Solution
;;
;; Here is the plan:\
;; We will iterate through all possible antenna pairs.
;; If the pair has the same frequency, we calculate the distance between them
;; and proceed with finding the antinodes for that pair.
;;
;; For Part 1, the only thing that remains is to see if — when we move
;; "one distance" away from the second point in our pair — we're still
;; inside of the grid.\
;; But, since the solution for Part 2 would share most of the code with Part 1,
;; we will write a version that works for both parts.
;;
;; This means that for each matching antenna pair, we will find a set of
;; _all_ antinodes that satisfy a given criteria.
;; For Part 1 we consider only one antinode on each side, and for Part 2 it is
;; a set of all points inside of the grid that lie on the same line as
;; the antenna pair with the same distance between them.
;; Here's the example from the task:
;;
;; ```
;; T....#....
;; ...T......
;; .T....#...
;; .........#
;; ..#.......
;; ..........
;; ...#......
;; ..........
;; ....#.....
;; ..........
;; ```

;; To find a set of all antinodes that satisfy the criteria, we are
;; collecting them with `reduce`, and we are immediately stoppng the
;; accumulation (by using the
;; [`reduced` function](https://clojuredocs.org/clojure.core/reduced))
;; when we find a point that is outside of the grid.
;;
(defn antinodes [[size antennas] multis]
  (for [[pt1 freq1] antennas
        [pt2 freq2] antennas
        :when (and (= freq1 freq2) (not= pt1 pt2))
        :let [dist (aoc/pt- pt2 pt1)]]
     (reduce (fn [acc n]
               (let [pt (aoc/pt+ pt2 (aoc/pt* n dist))]
                 (if (aoc/inside? size pt)
                   (conj acc pt)
                   (reduced acc))))
             #{}
             multis)))


;; The function above will give us a list of antinodes for each antenna pair.
;; Our tasks s to count the number of uniqe positions:
;;
(defn solve [data multis]
  (->> (antinodes data multis)
       (reduce set/union)
       count))



;; ### Part 1
;;
;; As previously said, we are only interested in the antinodes that are
;; "one distance" away from our pair:
;;
(solve example-data [1])
(solve data [1])




;; ### Part 2
;;
;; In Part 2 we want antinodes for _all_ multiples of the distance,
;; so we're passing an unlimited range as a multiplier:
;;
(solve example-data (range))
(solve data (range))







;; ## Visualization
;;
;; Let's see how all these antinodes from Part 2 look visually:
;;
(let [antis (antinodes data (range))
      axes-props {:showgrid false
                  :zeroline false}]
  (clerk/plotly
   {:config {:displayModeBar false}
    :data (for [anti antis]
            (let [xs (map first anti)
                  ys (map second anti)]
              {:x xs
               :y ys
               :type :scatter
               :mode :markers}))
    :layout {:margin {:l 0 :r 0 :t 0 :b 0}
             :xaxis axes-props
             :yaxis (merge axes-props {:autorange :reversed})
             :showlegend false}}))


;; It looks like the people who live in the south-eastern part of the city
;; are less likely to receive radio ads about the
;; _Easter Bunny brand Imitation Mediocre Chocolate_.






;; ## Conclusion
;;
;; Another easy day (the hardest part was understanding what _exactly_ we
;; are looking for), even though we're already on Day 8 and it is a weekend
;; (traditionally, AoC has harder tasks on weekends).\
;; I don't mind it at all, and I hope it'll continue like that for another week.
;;
;;
;;
;; Today's highlights:
;; - `complement`: create an "opposite" function
;; - `reduced`: immediately exit from a `reduce`
;; - `(range)`: unlimited range, from 0 to infinity










^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  [(solve (parse-data input) [1])
   (solve (parse-data input) (range))])
