^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day06
  {:nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc :collapsed}
  (:require [aoc-utils.core :as aoc]
            [quil.core :as q]
            [quil.middleware :as m]
            [nextjournal.clerk :as clerk]))



;; # Day 6: Guard Gallivant
;;
;; The Historians have a device which allows travelling through the space...
;; and time!
;;
;; > It turns out that having direct access to history is very convenient
;; > for a group of historians.
;;
;; We are currently in the year 1518!






;; ## Input parsing
;;
;; We are given a 2D map of a lab that looks like this:
;;
(def example "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

;; We'll have to extract several pieces of information from it:
;; - where are obstructions (`#`)
;; - the starting position of the guard (`^`)
;; - the size of the grid (it is the same in both directions)
;;
(defn parse-data [input]
  (let [lines (aoc/parse-lines input :chars)]
    {:walls (aoc/grid->point-set lines #{\#})
     :guard (first (aoc/grid->point-set lines #{\^}))
     :size (count lines)}))

(def example-data (parse-data example))
(def data (parse-data (aoc/read-input 6)))




;; ## Part 1
;;
;; When the guard comes next to an obstacle, he turns right.
;;
;; We need to be careful when writing the direction transformation for
;; the right turn, since we are in a left Cartesian coordinate system
;; (y-values increase to the bottom).\
;; (Guess who implemented a left turn initially.)
;;
(defn turn-right [[dx dy]]
  [(- dy) dx])


;; Now we can find the path the guard will take.\
;; We have three possible situations:
;; - if the next step is outside the grid, we have an exit condition
;; - the guard is going forwards
;; - if ahead of the guard is a `wall`, he'll turn right
;;
(defn guard-path [{:keys [walls guard size]}]
  (loop [pt guard
         dir [0 -1]
         path [pt]]
    (let [pt' (aoc/pt+ pt dir)]
      (cond
        (not (aoc/inside? size pt')) path
        (walls pt') (recur pt (turn-right dir) path)
        :else (recur pt' dir (conj path pt'))))))

^{:nextjournal.clerk/auto-expand-results? false}
(def example-path (guard-path example-data))

^{:nextjournal.clerk/auto-expand-results? false}
(def path (guard-path data))


;; The guard will visit some locations multiple times in different
;; directions, but our task is to find:
;;
;; > How many **distinct positions** will the guard visit
;; > before leaving the mapped area?
;;
(defn part-1 [path]
  (-> path set count))


(part-1 example-path)
(part-1 path)







;; ## Part 2
;;
;; The Historians are now asking us how many positions in the grid
;; can be used to put an additional obstacle which would cause the
;; guard to be stuck in a loop.
;;
;; We could try to put an obstacle to every empty space in our grid
;; to see if that causes a loop, but that would be very inefficient.
;; The guard will only visit those locations from his path in Part 1, and it
;; is enough to check if putting an obstacle there would create a loop.
;;
;; We've seen in Part 1 that a location can be visited multiple times
;; and that on its own doesn't mean we're in a loop.
;; We'll also have to track a direction.
;;
;; As a small optimization, we'll only track a position and a direction
;; when we come next to an obstacle and make a turn.
;; It'll take a bit more steps to find a loop, but the set of
;; candidates will be much smaller, giving us a faster lookup.
;;
;; The code looks very similar to the Part 1.
;; We have two exit conditions from the recursion:
;; - if we found the loop
;; - if we went outside of the grid
;;
(defn is-loop? [{:keys [guard walls size]} obstacle]
  (let [walls (conj walls obstacle)]
    (loop [pt guard
           dir [0 -1]
           turns #{}]
      (let [pt' (aoc/pt+ pt dir)]
        (cond
          (turns [pt dir]) 1
          (not (aoc/inside? size pt')) 0
          (walls pt') (recur pt (turn-right dir) (conj turns [pt dir]))
          :else (recur pt' dir turns))))))


;; Now what is left to do is to try to put an obstacle to each
;; unique location visited in Part 1 and count all the loops we find.\
;; Since the movement through the grid (the function above) is relatively
;; slow and we need to run it more than 5000 times, we'll take an
;; advantage of our modern hardware and, instead of a regular `map`,
;; we'll do it in parallel with the
;; [`pmap` function](https://clojuredocs.org/clojure.core/pmap).
;;
(defn part-2 [data path]
  (aoc/sum-pmap #(is-loop? data %) (set path)))


(part-2 example-data example-path)
(part-2 data path)





;; ## Animation
;;
;; Today, instead of a static image like in the previous days,
;; we can do an animation of the guard's path.
;;
;; For creating animations, I prefer to use the [Quil library](http://quil.info).
;; I've used it previously to create
;; [some visualizations for AoC 2016](https://github.com/narimiran/advent_of_code_2016?tab=readme-ov-file#visualizations)
;; and I've even [made a game](https://narimiran.github.io/amaze/) with Quil
;; (you have to play it, it is AMAZEing!).
;;
;; Here's the full code:
;;
{:nextjournal.clerk/visibility {:result :hide}}

(def initial-state (merge data {:path path}))
(def scale 8)
(def size (* scale (:size initial-state)))


(defn draw-walls [walls]
  (q/fill 15 15 33)
  (q/fill 150)
  (q/stroke-weight 0.2)
  (q/stroke 220)
  (q/scale scale)
  (doseq [[x y] walls]
    (q/rect x y 1 1)))


(defn setup []
  (q/frame-rate 240)
  (q/background 15 15 33)
  (draw-walls (:walls initial-state))
  (q/no-stroke)
  (q/fill 255 255 96)
  initial-state)

(defn update-state [state]
  (update state :path rest))

(defn draw-state [{:keys [path]}]
  (q/scale scale)
  (when (empty? path)
    (q/delay-frame 2000)
    (q/exit))
  (let [[x y] (first path)]
    (q/rect (+ 0.1 x) (+ 0.1 y) 0.8 0.8))
  #_(q/save-frame "/tmp/imgs/day06-####.jpg"))

(comment
  (q/sketch
   :size [size size]
   :setup #'setup
   :update #'update-state
   :draw #'draw-state
   :middleware [m/fun-mode]))


;; To make a video from the frames created above, I'm using the following command:\
;; `ffmpeg -framerate 240 -i /tmp/imgs/day06-%04d.jpg -c:v libx264 -pix_fmt yuv420p imgs/day06.mp4`
;;
;; And the result is:
;;
^{:nextjournal.clerk/visibility {:code :hide
                                 :result :show}}
(clerk/html
 [:video {:controls true}
  [:source {:src "https://imgur.com/4LoJf6l.mp4"
            :type "video/mp4"}]
  "Your browser does not support the video tag."])




;; ## Conclusion
;;
;; Maybe in the future (or: more recent past), the guard will learn to
;; also make a left turn? Could that be a later task for us to solve?
;;
;; Today's highlights:
;; - `pmap`: apply a function in parallel
;; - Quil library: create drawings and animations








^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [data (parse-data input)
        path (guard-path data)]
    [(part-1 path)
     (part-2 data path)]))
