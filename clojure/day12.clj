^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day12
  {:nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc :collapsed}
  (:require [aoc-utils.core :as aoc]
            [clojure.set :as set]))


;; # Day 12: Garden Groups
;;
;; Today we're on a farm with many regions consisting of garden plots
;; that look like this:
;;
(def example "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE")

;; Each region has plots with the same type of plant (the same letter).





;; ## Input parsing
;;
;; It's a 2D grid once again.
;;
(def example-grid (aoc/parse-lines example :chars))
(def grid (aoc/parse-lines (aoc/read-input 12) :chars))



;; ## Creating regions
;;
;; How to create a region?
;; Pick a point and visit all connected plots (vertically and horizontally)
;; that have the same type of plant.
;;
;; A perfect job for my graph-traversal helper.
;; All we need to specify is a starting point and a condition for a valid
;; neighbouring point.
;; After there's no more places to visit, we return all points we've `:seen`:
;;
(defn traverse [grid start plant]
  (-> (aoc/bfs {:start start
                :nb-cond #(= plant (aoc/grid-get grid %))})
      :seen))

;; As an example, for the top-most point of the example, we would get this region:
;;
(traverse example-grid [0 0] \R)



;; Now we have to repeat this for all the points in the grid.
;; We will run the above function on every point we've not visited so far.
;; We keep track of all regions we discover:
;;
(defn create-regions [grid]
  (loop [unvisited (set (for [y (range (count grid))
                              x (range (count (first grid)))]
                          [x y]))
         regions []]
    (if-let [pt (first unvisited)]
      (let [plant (aoc/grid-get grid pt)
            region (traverse grid pt plant)]
        (recur (set/difference unvisited region)
               (conj regions region)))
      regions)))


;; Let's just check if we're getting the expected 11 regions for the example:
;;
(count (create-regions example-grid))







;; ## Part 1
;;
;; > Due to "modern" business practices, the price of fence required for
;; > a region is found by multiplying that region's **area** by its **perimeter**.
;;
;; The area is just a number of plots in the region, and the perimeter is:
;; > the number of sides of garden plots in the region that do not touch
;; > another garden plot in the same region.
;;
;; To calculate a perimeter around a point, it is enough to find how many of
;; its four neighbours don't belong to the same region.
;;
(defn perimeter [region pt]
   (count (aoc/neighbours-4 pt (complement region))))

(defn fence-price [region]
  (* (count region)
     (aoc/sum-map #(perimeter region %) region)))



;; Let's check if we're getting the correct price of `216` for the region
;; starting at the top-left corner of the example:
;;
(-> example-grid
    (traverse [0 0] \R)
    fence-price)


;; Everything looks fine.
;; We need to calculate the total price of all fences around all regions:
;;
(defn part-1 [grid]
  (let [regions (create-regions grid)]
    (aoc/sum-map fence-price regions)))

(part-1 example-grid)
(part-1 grid)







;; ## Counting sides
;;
;; > Under the bulk discount, instead of using the perimeter to calculate
;; > the price, you need to use the **number of sides** each region has.
;; > Each straight section of fence counts as a side, regardless of how long it is.
;;
;; My initial idea was to stand next to a fence, and then walk parallel to it
;; with my right hand always perpendicular to the fence.
;; - While I'm walking straight, I'm on a single side.
;; - When I have to make a turn, this means there is an another side.
;; Walk around a region and count how many turns there were.
;;
;; But I abandoned that idea because I wasn't sure if I'll be able to walk on
;; the inside of hollow shapes.
;; And it looked complex to implement, prone to subtle bugs.
;;
;; After few hours, I realized I don't have to walk around a region.
;; It is enough to check each garden plot if it is a corner and how many turns
;; do we make at that corner.
;; One turn = one side.
;;
;; Consider the following examples:
;;
;; ```
;; ......    A######B   ........    .....
;; .A##B.    #E####F#   .A##B...    ..#..
;; .####.    ##....##   .###I#J.    .#K#.
;; .C##D.    #G####H#   .C##D...    ..#..
;; ......    C######D   ........    .....
;; ```
;;
;; In the first example we have four _outside_ corners (A, B, C, D) and that
;; means we have four sides in total.\
;; The shape in the second example is hollow, and there are also four _inner_
;; corners (E, F, G, H), which brings total sides to 8.\
;; In the third example we have two corners which have two turns each.
;; Corner `J` is a `B + D` combination (it is both top-right and bottom-right
;; _outside_ corner), and corner `I` is `E + G` combination
;; (both top-left and bottom-left _inside_ corner).
;; Total amount of turns is 8.\
;; The `K` point in the fourth example is a combination of all four inner
;; corners!
;; And each of the outside corners is a combination of two outer corners,
;; bringing the total to 12.






;; ## Part 2
;;
;; For every garden plot in a region we will check how many corner types
;; (from `A` to `H`) it satisfies.
;; This can be a number from zero (not a corner) to four (a single-plot region
;; or the `K` point from the fourth example above).
;;
;; WARNING: What you're about to see is some ugly code with some ugly variable
;; names.\
;; I didn't want to type 20 times `(region [(inc x) (dec y)])` and similar,
;; so I shortened it to `r+-`. (If a coordinate stays the same, it is
;; denoted by `=`.)
;;
(defn turns [region [x y]]
  (let [x+ (inc x) , x- (dec x) , y+ (inc y) , y- (dec y)
        [r-= r+= r=- r=+] (mapv region [[x- y]  [x+ y]  [x y-]  [x y+]])
        [r-- r-+ r+- r++] (mapv region [[x- y-] [x- y+] [x+ y-] [x+ y+]])]
    (aoc/count-if identity
                  [(and (not r-=) (not r=-))   ; A
                   (and (not r+=) (not r=-))   ; B
                   (and (not r-=) (not r=+))   ; C
                   (and (not r+=) (not r=+))   ; D
                   (and r+= r=+ (not r++))     ; E
                   (and r-= r=+ (not r-+))     ; F
                   (and r+= r=- (not r+-))     ; G
                   (and r-= r=- (not r--))]))) ; H


;; To get total number of sides of a region, we need to sum up all turns
;; from all garden plots in a region.
;; The discount price of a region is that number times the region area:
;;
(defn discount-price [region]
  (* (count region)
     (aoc/sum-map #(turns region %) region)))



;; All it is left to do is to add all discount prices of all regions
;; and we're done:
;;
(defn part-2 [grid]
  (let [regions (create-regions grid)]
    (aoc/sum-map discount-price regions)))

(part-2 example-grid)
(part-2 grid)







;; ## Conclusion
;;
;; Oh my.\
;; In the end, it wasn't _that_ hard, but it took me some time to come up
;; with a solution that doesn't involve walking around each region.
;; (And then some more time to fix lots of small bugs in the `turns` function.)
;;
;; No new functions to highlight today.



^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [regions (-> input
                    (aoc/parse-lines :chars)
                    create-regions)]
    [(aoc/sum-map fence-price regions)
     (aoc/sum-map discount-price regions)]))
