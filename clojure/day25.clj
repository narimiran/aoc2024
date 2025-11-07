^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day25
  {:nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc :collapsed}
  (:require [aoc-utils.core :as aoc]))


;; # Day 25: Code Chronicle
;;
;; We're back at the Chief Historian's office, but the door is locked.
;;
;; Fortunately, we have
;; > schematics of every lock and every key
;;
;; that looks like this:
;;
(def example "#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####")


;; ## Input parsing
;;
;; We differentiate between the locks and keys like this:
;; > The locks are schematics that have the top row filled (`#`)
;; > and the bottom row empty (`.`); the keys have the top row empty and
;; > the bottom row filled.
;;
;; We need to count number of `#` in each column of a schematic:
;;
(defn parse-schematic [group]
  (let [kl (if (= "#####" (first group))
             :locks
             :keys)
        columns (->> (aoc/transpose group)
                     (mapv #(aoc/count-if #{\#} %)))]
    [kl columns]))

;; We do that for every schematic, and we separate them into two groups,
;; either locks or keys:
;;
(defn parse-data [input]
  (let [schematics (->> (aoc/parse-paragraphs input)
                        (mapv parse-schematic)
                        (group-by first))]
    schematics))

(def example-data (parse-data example))
(def data (parse-data (aoc/read-input 25)))




;; ## Solution
;;
;; For every combination of a lock and a key, we need to check if they're
;; compatible, i.e. if `every?` column of a key-lock combination doesn't
;; overlap:
;;
(defn solve [{:keys [locks keys]}]
  (aoc/do-count [[_ l] locks
                 [_ k] keys
                 :let [sums (map + l k)]
                 :when (every? #(<= % 7) sums)]))

(solve example-data)
(solve data)




;; ## Conclusion
;;
;; Traditionally, we have an easier task for Christmas.\
;; Nothing special here to write about.








^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [data (parse-data input)]
    (solve data)))
