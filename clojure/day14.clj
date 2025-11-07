^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day14
  {:nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc :collapsed}
  (:require
   [aoc-utils.core :as aoc]
   [quil.core :as q]
   [quil.middleware :as m]
   [nextjournal.clerk :as clerk]))



;; # Day 14: Restroom Redoubt
;;
;; We're at Easter Bunny Headquarters and one of The Historians needs
;; to use the bathroom.
;; But there are robots guarding the area outside the bathroom.
;; They are moving in predictable straight lines and our task is to find
;; out where will they be after 100 seconds.
;;
;; For each robot we have its current position (at time 0) and its
;; velocity, e.g. `p=2,4 v=2,-3`.
;;
;; NOTE: Since the task changes some parameters between the example and
;; the real input, we won't be solving it for the example.





;; ## Input parsing
;;
;; To get a position and a velocity of a robot, we need to extract all
;; integers from an input line.
;;
;; Since the positions will change every second, while the velocities stay
;; constant, we will split them in two separate lists:
;;
(defn parse-data [input]
  (let [robots (for [[px py vx vy] (aoc/parse-lines input :ints)]
                  [[px py] [vx vy]])
        positions (mapv first robots)
        velocities (mapv second robots)]
    [positions velocities]))

(def data (parse-data (aoc/read-input 14)))


;; The task defines the `width` and `height` of the floor as:
;;
(def width 101)
(def height 103)




;; ## Part 1
;;
;; Our task for Part 1 consists of two tasks:
;; - find the positions of robots after 100 seconds
;; - count the number of robots in each quadrant of the floor
;;
;; Moving a robot is easy.
;; We move it `n` times, making sure it is still inside of the floor
;; using `mod` (it works also for negative number):
;;
(defn move-robot
  ([pos vel] (move-robot 1 pos vel))
  ([n [px py] [vx vy]]
   (let [px' (mod (+ px (* n vx)) width)
         py' (mod (+ py (* n vy)) height)]
     [px' py'])))

;; The function has two arities.
;; We can call it with two arguments and get the next position (after 1 second)
;; of a robot, or if we call it with three arguments, we get the position
;; of a robot after `n` seconds.

(let [p [2 4]
      v [2 -3]]
  [(move-robot p v)
   (move-robot 100 p v)])






;; For the second task, we can divide the floor in four quadrants like this:
;; ```
;; ..... .....
;; .-.-. .+.-.
;; ..... .....
;;      C
;; ..... .....
;; .-.+. .+.+.
;; ..... .....
;; ```
;;
;; `C` is a point in the centre of the floor.
;; Points in the top-left quadrant have both x- and y- positions smaller than `C`.
;; Points in the top-right quadrant have x-position larger than x-position of `C`
;; and lower y-position. Etc.
;;
;; Our task is to count the number of robots in each quadrant.
;; To do that, we will check how many robots satisfy the conditions of
;; a quadrant:
;;
(defn count-in-quadrant [positions [cx cy] [x-cond y-cond]]
  (aoc/do-count [[x y] positions
                 :when (and (x-cond x cx)
                            (y-cond y cy))]))


;; Putting it all together, we get this:
;;
(defn part-1 [[positions velocities]]
  (let [positions' (mapv (partial move-robot 100)
                         positions
                         velocities)
        center     [(quot width 2) (quot height 2)]
        quad-count (partial count-in-quadrant positions' center)
        quad-conds [[< <]    ; top-left
                    [> <]    ; top-right
                    [< >]    ; bottom-left
                    [> >]]]  ; bottom-right
    (aoc/prod-map quad-count quad-conds)))


;; Several things to highlight:
;; - For each robot, we will move it 100 times, so we can partially apply
;;   the `move-robot` function by using
;;   [`partial`](https://clojuredocs.org/clojure.core/partial).
;; - We are taking the advantage of `map/mapv` taking multiple arguments which are
;;   passed in parallel to the function we're calling.
;;   Here, the function will receive two arguments: the first element of
;;   `positions` and the first element of `velocities`, then the second element
;;   of each, etc.
;; - One more usage of `partial`. The only thing that changes when calling
;;   the `count-in-quadrant` function are the conditions of each quadrant,
;;   so the `positions'` and `center` can be partially applied in advance.
;; - We are interested in a product of numbers in each quadrant, so I'm
;;   using the `aoc/prod-map` function, which is an equivalent of
;;   `(reduce * (map ...))`.
;;

(part-1 data)







;; ## Part 2
;;
;; We need to find an _Easter_ egg: a _Christmas_ tree.
;;
;; I've seen people use different approaches for this task.\
;; The one I used is kind of hinted in Part 1, where we're shown some
;; situations where two robots occupy the same space.
;; I've guessed that for an image to appear, every robot should be in its
;; own position, with no overlapping.
;;
;; I'm using the [`distinct?` function](https://clojuredocs.org/clojure.core/distinct_q)
;; to check if all positions are unique.
;; If they are, we've got the solution.
;; If not, we move robots for one second and check again.
;;
(defn part-2 [[positions velocities]]
  (reduce
   (fn [positions n]
     (if (apply distinct? positions)
       (reduced n)
       (mapv move-robot positions velocities)))
   positions
   (range)))


(part-2 data)








;; ## Visualization
;;
;; Let's see how that Christmas tree looks like:
;;
(let [[positions velocities] data
      positions' (mapv (partial move-robot 8168) positions velocities)
      axes-common {:showgrid false
                   :zeroline false}]
 (clerk/plotly
  {:config {:displayModeBar false
            :displayLogo false}
   :data [{:x (mapv first positions')
           :y (mapv second positions')
           :mode :markers
           :marker {:symbol :square
                    :size 5}}]
   :layout {:xaxis axes-common
            :yaxis (merge axes-common {:autorange :reversed})
            :height 700
            :width 700
            :margin {:l 0 :r 0 :t 0 :b 0}
            :showlegend false}}))






;; ## Animation
;;
;; Ok, we have a final image, but let's also see how we got there!
;;
;; Here are the last few seconds of robot movements in a slow-motion:
;;
{:nextjournal.clerk/visibility {:result :hide}}

(defn setup []
  (let [[pos vel] data
        step 1/180
        start (- 8168 (* 600 step))
        init-pos (mapv (partial move-robot start) pos vel)]
    (q/frame-rate 60)
    (q/no-stroke)
    {:pos init-pos
     :step-size step
     :vel vel
     :step start}))

(defn update-state [{:keys [pos vel step step-size] :as state}]
  (-> state
      (assoc :pos (mapv (partial move-robot step-size) pos vel))
      (update :step + step-size)))

(defn draw-state [{:keys [pos step]}]
  (q/scale 7)
  (q/background 15 15 35)
  (when (= step 8168)
    (q/exit))
  (doseq [[x y] pos]
    (q/fill 255 255 102)
    (q/rect x y 1 1))
  #_(q/save-frame "/tmp/imgs/day14-###.jpg"))

(comment
  (q/sketch
   :size [(* 7 (inc width))
          (* 7 (inc height))]
   :setup #'setup
   :update #'update-state
   :draw #'draw-state
   :middleware [m/fun-mode]))


;; To make a video from the frames created above, I'm using the following command:\
;; `ffmpeg -framerate 60 -i /tmp/imgs/day14-%03d.jpg -vf tpad=stop_mode=clone:stop_duration=2 -c:v libx264 -pix_fmt yuv420p imgs/day14.mp4`
;;
;; And the result is:
;;
^{:nextjournal.clerk/visibility {:code :hide
                                 :result :show}}
(clerk/html
 [:video {:controls true}
  [:source {:src "https://i.imgur.com/AnVFvb8.mp4"
            :type "video/mp4"}]
  "Your browser does not support the video tag."])







;; ## Conclusion
;;
;; The hardest part of today's task was to figure out when/how the image
;; of a Christmas tree would appear.\
;; The coding part of the task was easy.
;;
;; Today's highlights:
;; - multi-arity functions
;; - `partial`:  partially apply a function
;; - `distinct?`: check uniqueness of its arguments











^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [data (parse-data input)]
    [(part-1 data)
     (part-2 data)]))
