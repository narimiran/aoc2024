^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day15
  {:nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc :collapsed}
  (:require
   [aoc-utils.core :as aoc]
   [clojure.string :as str]
   [quil.core :as q]
   [quil.middleware :as m]
   [nextjournal.clerk :as clerk]))


;; # Day 15: Warehouse Woes
;;
;; We're back inside of our mini submarine and we encounter... lanternfish!!
;;
;; The lanternfish have built a warehouse (using `#` for walls)
;; with boxes of food (`O`) and a single robot (`@`).
;; The moves of the robot are known in advance, and it can move the boxes
;; around.
;;
;; Our task is to find where all the boxes will end up after the robot
;; makes all of its moves.
;;
;; Here's an example of the warehouse and robot's moves:
;;
(def example "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<")







;; ## Input parsing
;;
;; Our task is to calculate the sum of a "GPS coordinate" of all boxes after
;; robot's movements.
;; The GPS coordinate is `x + 100*y`.
;;
;; While we could work with `x,y` coordinates, and then after all moves
;; calculate the GPS coordinates, I already have a helper function ready
;; to work with GPS: `aoc/create-hashed-grid`, where we can specify
;; the multiplier for the y-axis.
;; Exactly what we need.
;;
;; There are three different things we're interested from the map of the
;; warehouse:
;; - walls (`#`)
;; - boxes (`O`)
;; - robot (`@`)
;;
;; We will extract each in a separate key in a hashmap which will
;; track the `state` of things (the positions of the robot and of the boxes
;; will change over time):
;;
(defn extract-state [grid]
  (-> (aoc/create-hashed-grid grid
                              {\# :walls
                               \O :boxes
                               \@ :bot}
                              100)
      (update :bot first)))


;; We will convert a list of moves into directions (moving across y-axis
;; changes the GPS coordinate by 100):
;;
(def directions
  {\< -1
   \> 1
   \^ -100
   \v 100})


;; In the real input, the list of moves is given in multiple lines just for
;; convenience, but they should be considered as a single sequence.
;; We will use the [`str/join` function](https://clojuredocs.org/clojure.string/join)
;; to concatenate it.
;;
(defn parse-data [input]
  (let [[grid moves] (aoc/parse-paragraphs input)]
    [(extract-state grid)
     (mapv directions (str/join moves))]))



^{:nextjournal.clerk/visibility {:result :hide}}
(def example-data (parse-data example))

^{:nextjournal.clerk/visibility {:result :hide}}
(def data (parse-data (aoc/read-input 15)))

;; NOTE: Due to a bug in Clerk notebooks, the value of `example-data` and
;; `data` cannot be shown. Here is the value of `example-data`, manually
;; copy-pasted, to get an idea what they look like:
;; ```
;; [{:walls #{0 1 2 3 4 5 6 7
;;            100 107
;;            200 201 207
;;            300 307
;;            400 402 407
;;            500 507
;;            600 607
;;            700 701 702 703 704 705 706 707},
;;   :boxes #{103 105 204 304 404 504},
;;   :bot 202}
;;  [-1 -100 -100 1 1 1 100 100 -1 100 1 1 100 -1 -1]]
;; ```






;; ## Moving a robot
;;
;; Now that we have a state and a list of moves, we need to define how the
;; state is going to change based on the robot movement.
;;
;; There are three possible scenarios when a robot tries to move to some
;; location:
;; - there is a wall at that location --> stay at the current location
;; - there is a box at that location --> recursively try to find if the box
;;   can be moved in that direction to make a space for the robot to move
;; - the location is empty --> move the robot
;;
;; The first scenario is the easiest one: we don't have to do anything,
;; the state stays the same as it was.
;;
;; Consider these examples where the robot tries to move to the right:
;; ```
;; A:   ..@..OO#
;; B:   ..@OOO..
;; C:   ..@OO##.
;; ```
;;
;; In the example A, the updated state will have the robot moved
;; to the right, no other changes needed.
;;
;; In the examples B and C, there is a box in a location
;; where the bot would like to go.\
;; We need to recursively check to the right of that location to see if,
;; at some point, we will encounter a wall or an empty space.
;;
;; In the example B, we find a space after three boxes, and we need
;; to move them to the right.
;; We don't have to move all three boxes one by one, it is enough to move
;; the first one to the empty space.
;; If the boxes were numbered, initially we would have `..@123..`, and
;; after the move we'll have `...@231.`, only the box named `1` changes
;; its position.
;; This allows us a much simpler implementation of the box movement.
;;
;; In the example C, after two boxes we find a wall.
;; It means that the state won't change.
;; The same as if the wall were next to the robot.
;;
;; Interesting to notice is that having an empty space next to a robot,
;; or having it after one or more boxes in the direction of the robot's
;; movement doesn't make a difference in a programming logic:
;; 1. put a box in the first empty location (`pos`)
;; 2. remove a box from the location where the robot will move (`bot'`)
;; 3. move the robot to its new location (`bot'`)
;;
;; If there wasn't any box next to a robot, the first two points cancel each
;; other: we put a box there, and then remove it from the same place
;; (`pos = bot'`), and we just move the robot:
;;
(defn move [{:keys [walls boxes bot] :as state} dir]
  (let [bot' (+ bot dir)]
    (loop [pos bot']
      (cond
        (walls pos) state
        (boxes pos) (recur (+ pos dir))
        :else (-> state
                  (update :boxes conj pos)
                  (update :boxes disj bot')
                  (assoc  :bot bot'))))))





;; ## Part 1
;;
;; Having the `move` function that takes a current state, makes a move and returns
;; a new state, means that all we have to do for Part 1 is to use that
;; function inside of a `reduce` to continuously update the state for
;; all moves in the input.
;;
;; Once we're done, we take the GPS coordinates of all boxes in the final
;; state and sum them up:
;;
(defn part-1 [[state moves]]
  (->> (reduce move state moves)
       :boxes
       (reduce +)))


(part-1 example-data)
(part-1 data)





;; ## Alternate solution
;;
;; With the above approach my solution for part 2 was very long, complicated and ugly.
;; So, it is better to find a different approach.
;;
;; Here's a new idea:
;; - Instead of having separate sets for walls and boxes in the state,
;;   this time we'll have a map of all occupied coordinates.
;; - We will have a solution that works for both parts.
;; - For part 2, before parsing the input, we'll make the following replacements:
;;
(def replacements
  {"#" "##"
   "O" "[]"
   "." ".."
   "@" "@."})

(defn widen-row [row]
  (str/replace row #"." replacements))


;; ### Input parsing
;;
;; Parsing the `moves` is exactly the same as before.
;; The `state` map now consists of `:grid` with all occupied coordinates,
;; and we will need the `:allowed?` field to tell us if we're allowed to
;; make a move.
;;
(defn parse-data-2 [input widen?]
  (let [[grid moves] (aoc/parse-paragraphs input)
        grid (-> (if widen?
                   (mapv widen-row grid)
                   grid)
                 (aoc/create-hashed-grid {\@ :bot , (set "#O[]") :grid}
                                         100)
                 (update :bot first)
                 (assoc :allowed? false))
        moves (mapv directions (str/join moves))]
    [grid moves]))

(def example-data-1 (parse-data-2 example false))
(def example-data-2 (parse-data-2 example true))

(def data-1 (parse-data-2 (aoc/read-input 15) false))
(def data-2 (parse-data-2 (aoc/read-input 15) true))



;; ### Solution
;;
;; When we're allowed to make a move, we will switch a box and and empty
;; space next to it:
;;
(defn switch [{:keys [grid] :as state} pos pos']
  (-> state
      (update :grid assoc pos' (grid pos))
      (update :grid dissoc pos)))



;; The `move` function is now more convoluted because of the part 2, where
;; we need to check if we can move both left and right part of a box, before
;; making a move.
;;
;; We'll try to move one part of a box, and if that's successful then we'll
;; try to move the other part (the order of these two checks is important).
;; If both are successful, that's our new state.
;; If any of them is not allowed, we return the initial state.
;;
(defn move-2 [{:keys [grid] :as state} pos dir]
  (let [pos' (+ pos dir)
        move-box (fn [s] (switch s pos pos'))
        state (assoc state :allowed? false)]
    (case (grid pos')
      nil (-> state
              (assoc :allowed? true)
              move-box)
      \# state
      \O (let [state' (move-2 state pos' dir)]
           (if (:allowed? state')
             (move-box state')
             state))
      \[ (let [state' (move-2 state (inc pos') dir)]
           (if (:allowed? state')
             (let [state'' (move-2 state' pos' dir)]
               (if (:allowed? state'')
                 (move-box state'')
                 state))
             state))
      \] (let [state' (move-2 state (dec pos') dir)]
           (if (:allowed? state')
             (let [state'' (move-2 state' pos' dir)]
               (if (:allowed? state'')
                 (move-box state'')
                 state))
             state)))))


;; If we were successful in moving all the boxes in our way (or there
;; was an empty space), we need to move the `:bot`:
;;
(defn move-bot [{:keys [bot] :as state} dir]
  (let [state' (move-2 state bot dir)]
    (if (:allowed? state')
      (assoc state' :bot (+ bot dir))
      state)))



;; This is everything we need to apply the moves.
;; After all moves have been made, we need to calculate GPS coordinates
;; of the boxes:
;;
(defn boxes-gps [grid]
  (->> grid
       (keep (fn [[k v]]
               (when (#{\[ \O} v) k)))
       (reduce +)))


(defn solve [[state moves]]
  (->> (reduce move-bot state moves)
       :grid
       boxes-gps))


(solve example-data-1)
(solve example-data-2)

(solve data-1)
(solve data-2)






;; ## Animation
;;
;; The animation for the real input would be too long and too boring.
;; Or waaaay too fast.\
;; Let's do the animation for the first example from the task.
;;
{:nextjournal.clerk/visibility {:result :hide}}

(def states
  (let [ex (parse-data (aoc/read-input "15_test"))
        [state moves] ex]
    (reductions move state moves)))

(defn setup []
  (q/frame-rate 15)
  (q/no-stroke)
  (q/ellipse-mode :corner)
  states)

(defn update-state [states]
  (rest states))

(defn draw-walls [walls]
  (q/fill 12 87 11)
  (doseq [gps walls]
    (let [x (mod gps 100)
          y (quot gps 100)]
      (q/rect (+ 0.1 x) (+ 0.1 y) 0.8 0.8))))

(defn draw-boxes [boxes]
  (q/fill 255 255 102)
  (doseq [gps boxes]
    (let [x (mod gps 100)
          y (quot gps 100)]
      (q/ellipse (+ 0.1 x) (+ 0.1 y) 0.8 0.8))))

(defn draw-bot [bot]
  (q/fill 180 20 20)
  (let [x (mod bot 100)
        y (quot bot 100)]
    (q/ellipse x y 1 1)))

(defn draw-state [states]
  (q/scale 75)
  (q/background 15 15 35)
  (when (empty? states)
    (q/exit))
  (let [{:keys [walls boxes bot]} (first states)]
    (draw-walls walls)
    (draw-boxes boxes)
    (draw-bot bot))
  #_(q/save-frame "/tmp/imgs/day15-###.jpg"))

(comment
  (q/sketch
   :size [750 750]
   :setup #'setup
   :update #'update-state
   :draw #'draw-state
   :middleware [m/fun-mode]))


;; To make a video from the frames created above, I'm using the following command:\
;; `ffmpeg -framerate 15 -i /tmp/imgs/day15-%03d.jpg -c:v libx264 -pix_fmt yuv420p imgs/day15.mp4`
;;
;; And the result is:
;;
^{:nextjournal.clerk/visibility {:code :hide
                                 :result :show}}
(clerk/html
 [:video {:controls true}
  [:source {:src "https://i.imgur.com/fNdzHpx.mp4"
            :type "video/mp4"}]
  "Your browser does not support the video tag."])






;; ## Conclusion
;;
;; Sokoban, anybody?
;;
;; Today's highlights:
;; - `str/join`: concatenate a sequence









^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  [(-> input
       (parse-data-2 false)
       solve)
   (-> input
       (parse-data-2 true)
       solve)])
