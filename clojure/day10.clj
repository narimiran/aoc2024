^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day10
  {:nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc :collapsed}
  (:require [aoc-utils.core :as aoc]))



;; # Day 10: Hoof It
;;
;; We are at a Lava Production Facility and we discover a familiar reindeer
;; wearing (goggles and a loose-fitting) hard hat.
;;
;; The reindeer shows us a topographic map of the surrounding area that
;; looks like this:
;;
(def example "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")


;; We need to find hiking trails that:
;; - start at height `0`,
;; - each step increases the height by 1,
;; - end at height `9`.




;; ## Input parsing
;;
;; We have a 2D grid of digits.
;; If you've been reading [these notebooks](../index) regularly since Day 1,
;; you already know that I have [aoc-utils library](https://narimiran.github.io/aoc-utils/)
;; ready for this:
;;
(defn parse-data [input]
  (-> input
      (aoc/parse-lines :digits)))

(def example-data (parse-data example))
(def data (parse-data (aoc/read-input 10)))




;; ## Solution
;;
;; Finally a graph traversal problem where I can use my
;; [graph traversal helper](https://narimiran.github.io/aoc-utils/#graph-traversal)
;; which I've written in preparation for this year.
;;
;; This helper is a huge monstrosity, implementing four algorithms
;; (DFS, BFS, Dijkstra, A*) and trying to cover various details Eric might
;; throw at us.
;; And yet, it wasn't enough for this task: today I had to add two additional
;; parameters in the already huge map of options.
;;
;; Maybe it would have been easier to solve this task without this helper,
;; but I really wanted to use it :)
;;
;; ----
;;
;; Ok, back to the task:
;;
;; > A **trailhead** is any position that starts one or more hiking trails - here,
;; > these positions will always have height `0`.
;;
;; Let's find all positions in our grid with height `0`:
;;
(defn find-trailheads [grid]
  (for [[j row] (map-indexed vector grid)
        [i n] (map-indexed vector row)
        :when (zero? n)]
    [i j]))

(find-trailheads example-data)




;; Looks correct, moving on.
;;
;; > you establish that a trailhead's **score** is the number of `9`-height
;; > positions reachable from that trailhead via a hiking trail.
;;
;; > A trailhead's **rating** is the number of distinct hiking trails
;; > which begin at that trailhead.
;;
;; For each trailhead, we are interested in _all_ positions (not just the
;; one we encounter first) with height `9`.
;; This means we'll run the pathfinding algorithm until exhaustion
;; (no `:end` or `:end-cond` specified), and count the number of
;; times we've seen a `9`.
;; We'll use [an `atom`](https://clojuredocs.org/clojure.core/atom)
;; to keep track of all such points.
;;
;; Since there can be multiple paths passing through the same point(s), we
;; will not limit ourselves to just the nodes we've not yet seen, i.e. we need to
;; `:allow-revisits?`:
;;
(defn traverse [grid trailhead]
  (let [ends (atom [])]
    (aoc/bfs {:start trailhead
              :allow-revisits? true
              :side-effect (fn [{:keys [pt]}]
                             (when (= 9 (aoc/grid-get grid pt))
                               (swap! ends conj pt)))
              :nb-func (fn [pt]
                         (aoc/neighbours-4 pt (fn [nb]
                                                (= (aoc/grid-get grid nb)
                                                   (inc (aoc/grid-get grid pt))))))})
    {:score  (count (distinct @ends))
     :rating (count @ends)}))



;; In Part 1, we're only interested in number of distinct 9-height points we
;; can reach (`:score`).
;; For Part 2, we count all times we came to an end (`:rating`).
;;
;; To sum it all up:
;; - We find the starting points ("trailheads") on the grid.
;; - For each trailhead we find solutions for both parts.
;; - We extract the values for each part and calculate their sum.
;;
(defn solve [grid]
  (->> grid
       find-trailheads
       (pmap #(traverse grid %))
       ((juxt #(aoc/sum-map :score %)
              #(aoc/sum-map :rating %)))))


(solve example-data)
(solve data)





;; ## Conclusion
;;
;; Finally a task where I could use my pathfinding helper!
;;
;; Maybe it was an overkill for today, but it was a nice test to see what
;; else I need to add to the helper to make it more useful.
;; Hopefully we'll have more pathfinding tasks later.
;;
;;
;; Today's highlights:
;; - `atom`: a reference to a mutable value
;; - `distinct`: remove duplicates from a collection







^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [data (parse-data input)]
    (solve data)))
