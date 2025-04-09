^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day20
  {:nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc :collapsed}
  (:require aoc))


;; # Day 20: Race Condition
;;
;; Today we're "right outside the CPU" and it's time for some race condition
;; festival.
;;
;; We're given a map of the racetrack (another 2D grid task!).
;; This time, there's only one way from the start (`S`) to the end (`E`),
;; and our job is to... cheat.



;; ## Input parsing
;;
;; We already had the same parsing task in [Day 16 solution](../aoc),
;; but this time we'll create a hash map, as we can use it directly later.
;;
(defn parse-data [input]
  (let [lines (aoc/parse-lines input)]
    {:start (first (aoc/grid->point-set lines #{\S}))
     :end   (first (aoc/grid->point-set lines #{\E}))
     :walls (aoc/grid->point-set lines #{\#})}))


(def data (parse-data (aoc/read-input 20)))



;; ## Solution
;;
;; Once again, it is possible to write a single solution that works for
;; both parts.
;;
;; Assuming we have a list of all visited points on our `path` (and we'll
;; see a bit later how easy this can be done), we need to find all
;; "shortcuts" which would save us _at least_ 100 steps.
;;
;; We'll try every pair of points in our path that are at least 100
;; steps appart (`old-dist`).\
;; A `new-dist` between those two points (using cheats to walk through
;; the walls) is a direct path between them: a `manhattan` distance.
;;
;; We have a `limit` of the maximum length of a shortcut:
;; 2 steps in Part 1 and 20 steps in Part 2.
;;
;; Putting it all together, we're only interested in a total number of
;; such shortcuts:
;;
(defn count-cheats [indexed-path limit]
    (aoc/do-count [[i a] indexed-path
                   [j b] indexed-path
                   :let [old-dist (- i j)]
                   :while (> old-dist 100)
                   :let [new-dist (aoc/manhattan a b)]
                   :when (and (<= new-dist limit)
                              (<= new-dist (- old-dist 100)))]))


;; The only thing remaining is to find the original `path` between `start`
;; and `end`.\
;; We've parsed our data into a hashmap with `:start`, `:end` and `:walls`
;; keys.
;; These are exactly the keys that my `[pathfinding helper]`(../aoc#graph-traversal)
;; uses: we pass the hashmap to `aoc/dfs` and extract `:path` from the
;; results.
;; Done.
;;
;; We need `[index value]` pair for each point on the path, and we can get
;; it using the `(map-indexed vector ...)` idiom.
;;
;; Since the `count-cheats` function does a nested loop through the path,
;; and the path itself has almost 10.000 elements, it takes a significant
;; amount of time to run it.\
;; Instead of waiting for the calculation for Part 1 to finish, before running
;; the calculation for Part 2, we can run them concurrently.
;;
;; We use the [`future` macro](https://clojuredocs.org/clojure.core/future),
;; which invokes a function in a separate thread, not blocking the current
;; one:
;;
(defn solve [grid]
  (let [path (:path (aoc/dfs grid))
        indexed-path (vec (map-indexed vector path))
        p1 (future (count-cheats indexed-path 2))
        p2 (future (count-cheats indexed-path 20))]
    [@p1 @p2]))


(solve data)




;; ## Conclusion
;;
;; A surprisingly short one.
;; Basically, it is just one short function that does all the work needed.
;;
;; Btw, having a maze where you can destroy some walls to reach the end
;; quicker?
;; Now I'm even more sure than on Day 18: Eric must have played my
;; [aMaze game](https://narimiran.github.io/amaze/)! :)
;;
;; Today's highlights:
;; - `future`: invoke a function in another thread, concurrently






^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [data (parse-data input)]
    (solve data)))
