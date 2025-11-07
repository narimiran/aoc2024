^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day16
  {:nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc :collapsed}
  (:require
   [aoc-utils.core :as aoc]
   [nextjournal.clerk :as clerk]))


;; # Day 16: Reindeer Maze
;;
;; Unlike human Olympic, Reindeer Olympics are happening every 9 years!?
;;
;; We are given a map like this:
;;
(def example "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############")

;; The Reindeer start at `S`, facing east, and need to come to end, marked `E`.
;; They can move forward, costing them `1` point, or they can stay where
;; they are and rotate 90 degrees to the left or to the right, costing
;; them `1000` points.
;;
;; A graph with unequal weights between nodes.
;; Time to bring out Dijkstra.


;; ## Input parsing
;;
;; We need to extract the positions of the walls, the start and the end.
;; There is more performant way than doing it like this (going through the
;; input multiple times), but — as I already said previously — this year
;; is not about the performance.
;;
(defn parse-data [input]
  (let [lines (aoc/parse-lines input)
        walls (aoc/grid->point-set lines #{\#})
        start (first (aoc/grid->point-set lines #{\S}))
        end (first (aoc/grid->point-set lines #{\E}))]
    [walls start end]))

(def example-data (parse-data example))
(def data (parse-data (aoc/read-input 16)))




;; ## Part 1
;;
;; For this part, we need to find the shortest path between the start and
;; the end.
;;
;; At each point we have three `neighbours`:
;; - we continue forward keeping the same direction
;; - we stay at the current point and turn left
;; - we stay at the current point and turn right
;;
(defn neighbours [[pt [dx dy :as delta]]]
  [[(aoc/pt+ pt delta) delta]
   [pt [dy (- dx)]]    ; left
   [pt [(- dy) dx]]])  ; right



;; Once again, I'm dogfeeding my graph traversal helper.
;;
;; Our starting point is the coordinate of `start` plus the direction
;; towards east (`[pt dir]`).
;; We are done when we reach the `end`, in any direction.
;;
;; Since the `walls` only have coordinates (and not directions), we need
;; to specify the `:nb-cond` function to check if a coordinate of a point is
;; (not) part of the walls.
;;
;; Making a turn costs more than continuing straight, so we need to
;; define the `:cost-fn` function for each movement we make:
;;
(defn traverse [walls start start-dir end]
  (aoc/dijkstra {:start [start start-dir]
                 :end-cond (fn [[pt _]] (= end pt))
                 :walls walls
                 :nb-cond (fn [[nb _]] (not (walls nb)))
                 :nb-func neighbours
                 :cost-fn (fn [[_ dir1] [_ dir2]]
                            (if (= dir1 dir2) 1 1000))}))



;; The Reindeer are now ready to make a run:
;;
(defn olympics [[walls start end]]
  (traverse walls start [1 0] end))


;; In Part 1 we're interested only in the minimum "score", i.e. the least
;; amount of points needed to run :
;;
(defn part-1 [data]
  (-> data
      olympics
      :steps))

(part-1 example-data)
(part-1 data)




;; ## Part 2
;;
;; In Part 1 we found _a_ best path, but it is not the only one.
;; Here we need to find _all_ points that are on _all_ best paths.
;;
;; There are several ways to do that.\
;; We could continue running the original algorithm even when we find the end
;; for the first time, and every time we reach the end again check if the
;; score is the same as the best one, and then add points in that path
;; to an accumulator.
;; But due to the large size of the grid, this is very slow.
;;
;; Another approach is to extract the costs for each point (more precisely:
;; the `[pt dir]` pair) we encountered initially.
;; Then we run backwards, from `end` to `start` and also extract the costs
;; of each point we visit.
;;
;; From Part 1 we know the `cost(start, end)` of the best run.
;; Now we check for each point visited if `cost(start, pt) + cost(pt, end) =
;; cost(start, end)`.\
;; (Since we're going backwards, the directions will be opposite, we need
;; to reverse them: `(mapv - d)`)
;;
(defn on-best-path? [best-score fwd-costs bkw-costs]
  (keep (fn [[[pt d] fwd-cost]]
          (when-let [bkw-cost (bkw-costs [pt (mapv - d)])]
            (when (= best-score (+ fwd-cost bkw-cost))
              pt)))
        fwd-costs))



;; Since the `end` is in the top-right corner, there's no need to check
;; east and north directions.
;; For other two directions, we `traverse` form `end` to `start`.
;; We need to take the `:costs` of all visited points, and we add all points
;; that are `on-best-path?` to the `best-spots`.
;; In the end, we're interested in the `count` of such points.
;;
(defn part-2 [[walls start end :as data]]
  (let [fwd-results (olympics data)
        best-result (:steps fwd-results)
        fwd-costs   (:costs fwd-results)]
    (-> (reduce (fn [best-spots init-dir]
                  (->> (traverse walls end init-dir start)
                       :costs
                       (on-best-path? best-result fwd-costs)
                       (into best-spots)))
                #{}
                [[0 1] [-1 0]]) ; south and west
        count)))


(part-2 example-data)
(part-2 data)



;; ## Visualization
;;
;; Let's see the best path!
;;
(let [walls (first data)
      path (map first (:path (olympics data)))
      axes-common {:ticks ""
                   :showticklabels false
                   :showgrid false
                   :zeroline false}]
  (clerk/plotly
   {:config {:displayModeBar false
             :displayLogo false}
    :data [{:x (map first walls)
            :y (map second walls)
            :mode :markers
            :marker {:symbol :square
                     :size 4
                     :color "777"}}
           {:x (map first path)
            :y (map second path)}]
    :layout {:xaxis axes-common
             :yaxis (merge axes-common {:autorange :reversed})
             :margin {:l 0 :r 0 :t 0 :b 0}
             :showlegend false}}))



;; ## Conclusion
;;
;; One of the rare times that Dijkstra's algorithm is needed.
;; Usually the BFS is enough.
;;
;; For Part 2 we could write a more performant solution where we immediately
;; check (when choosing valid neighbours) if a point is on the best path,
;; and discard it if it isn't.
;; But I'm sticking with my path traversal hepler :)
;;
;; Today's highlights:
;; - `(mapv - coll)`: change sign of all numbers in the `coll`
;; - `keep`: keep only non-nil results of a predicate function
;; - `when-let`: evaluate a test and, if true, bind the result








^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [data (parse-data input)]
    [(part-1 data)
     (part-2 data)]))
