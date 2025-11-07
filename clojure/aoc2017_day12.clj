^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns aoc2017-day12
  {:nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc :collapsed}
  (:require [aoc-utils.core :as aoc]))

;; # Testing graph traversal helper before AoC 2024, pt. 2

;; ## AoC 2017, Day 12
;;
;; > For example, suppose you go door-to-door like
;; > a travelling salesman and record the following list:
;;
;; ```
;; 0 <-> 2
;; 1 <-> 1
;; 2 <-> 0, 3, 4
;; 3 <-> 2, 4
;; 4 <-> 2, 3, 6
;; 5 <-> 6
;; 6 <-> 4, 5
;; ```
;;
;; > - Part 1: "How many programs are in the group that contains program ID 0?"
;; > - Part 2: "How many groups are there in total?"
;;
;; [Full task](https://adventofcode.com/2017/day/12)



;; ## Parsing the data
;;
;; Nothing spectacular here, we read from the input file and
;; extract all integers from each line.
;;
(def data (-> (aoc/read-input "2017_12")
              (aoc/parse-lines :ints)))


;; ## Creating connections
;;
;; Each line consists of a pipe and one or more of its neighbours.
;; Since there is a symmetry, i.e. a neighbour of a point will have
;; that point as its own neighbour, we have an easy time with this.
;; Basically, the first integer is the pipe and the rest are put
;; in the list of neighbours.
;; The resulting list is converted into a map.
;;
(def graph
  (into {} (for [[pipe & neighbours] data]
             [pipe neighbours])))




;; ## Part 1
;;
;; For part 1, we need to start from pipe `0` and see how far
;; that takes us.
;; There is no `end`, so we let our graph traversal run until
;; it reaches all available points.
;; For each point, we pick its neighbours from the `graph` we
;; just created.
;; We count how many points we visited along the way.
;;
(def pt1 (-> {:start 0
              :nb-func graph}
             aoc/bfs
             :count))




;; ## Part 2
;;
;; In part 2, we need to find how many separate "islands" are there.
;; We start from every (currently unvisited!) point and run the
;; same algorithm as in part 1.
;; The only difference is that now we're interested in all pipes
;; seen along the way, not just their number.
;;
(def pt2
  (-> (reduce (fn [acc pt]
                (if ((:visited acc) pt)
                  acc
                  (let [seen (-> {:start pt
                                  :nb-func graph}
                                 aoc/bfs
                                 :seen)]
                    (-> acc
                        (update :visited into seen)
                        (update :groups inc)))))
              {:visited #{}
               :groups 0}
              (keys graph))
      :groups))



^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
[pt1 pt2]
