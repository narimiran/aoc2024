^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day18
  {:nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc :collapsed}
  (:require
   [aoc-utils.core :as aoc]
   [clojure.string :as str]
   [nextjournal.clerk :as clerk]))




;; # Day 18: RAM Run
;;
;; Today we're inside of a computer's memory space and we need to find
;; a way to navigate through some corrupted coordinates.
;;
;; In short, one more 2D grid pathfinding problem.
;;
;; When I saw the following sentence in the task text:
;; > As bytes fall into your memory space
;;
;; I was afraid that the bytes will start to move in Part 2, but fortunately
;; that was not the case.




;; ## Input parsing
;;
;; Nothing special here.
;; We read integers from each line, like many times before.
;;
(defn parse-data [input]
  (aoc/parse-lines input :ints))

(def data (parse-data (aoc/read-input 18)))




;; ## Part 1
;;
;; We need to take first kilobyte (1024 bytes) from our input and
;; find minimum number of steps from the start (`(0, 0)`) to the
;; end (`(70, 70)`).
;;
;; A perfect job for my pathfinding helper.
;; The first possibility for off-by-one error: the size of the grid is not
;; 70. It is 71.
;;
(defn traverse [data size]
  (let [walls (set (take size data))]
    (aoc/bfs {:start [0 0]
              :end [70 70]
              :walls walls
              :size 71})))


;; The above function will return a hashmap with various results of
;; graph traversal.
;; We'll need them for Part 2.
;;
;; For Part 1, we are only interested in number of steps:
;;
(defn part-1 [data]
  (:steps (traverse data 1024)))

(part-1 data)



;; ## Part 2
;;
;; In Part 2 we need to find:
;; > the first byte that will cut off the path to the exit.
;;
;; We already know from Part 1 that the first kilobyte is ok, so we'll start
;; from there.
;; After a successful run through the maze (using only the provided `size` of
;; the input), we will extract the `:path` we've traveled.
;;
;; Instead of increasing size one by one and traversing through the maze
;; each time, there is a better way (aside from a binary search):\
;; We can consider (multiple) next bytes of the input.
;; If a byte is not on our `path`, then it can't possibly cut the path to
;; the exit and there's no need for traversing.\
;; Once we find a byte that _is_ on our path, that means that we need to re-run
;; through the maze: there's a possibility that we'll find another path.
;;
;; Rinse and repeat, until there's no path to the end anymore.
;;
(defn blocking-size [data]
  (loop [size 1024]
    (if-let [path ((:path (traverse data size)))]
      (recur (->> data
                  (take-while (complement (set path)))
                  count
                  inc))
      size)))

;; When we find the blocking _size_, we need to extract the coordinate of the
;; blocking _byte_.
;; One more opportunity for off-by-one error (and this one has bit me):
;; if we have a size `n`, the last index is at `(dec n)`:
;;
(defn part-2 [data]
  (->> data
       blocking-size
       dec
       data
       (str/join ",")))


(part-2 data)





;; ## Visualization
;;
;; Let's see how our paths compare with 1024 obstacles and before our path
;; is blocked.
;;
(let [init-path ((:path (traverse data 1024)))
      init-walls (take 1024 data)
      size (dec (blocking-size data))
      last-path ((:path (traverse data size)))
      last-walls (take size data)
      axes-common {:ticks ""
                   :showticklabels false
                   :showgrid false
                   :zeroline false}]
  (for [[walls path] [[init-walls init-path]
                      [last-walls last-path]]]
    (clerk/plotly
     {:config {:displayModeBar false
               :displayLogo false}
      :data [{:x (map first walls)
              :y (map second walls)
              :mode :markers
              :marker {:symbol :square
                       :size 8
                       :color "999"}}
             {:x (map first path)
              :y (map second path)}]
      :layout {:xaxis axes-common
               :yaxis (merge axes-common {:autorange :reversed})
               :width 600
               :height 600
               :margin {:l 0 :r 0 :t 0 :b 0}
               :showlegend false}})))



;; This is starting to look like [aMaze](https://narimiran.github.io/amaze/)!
;; It is the game I made earlier this year where you decide how difficult
;; the maze will be before you go through it, trying to be as fast as you
;; can.\
;; And if you create a too difficult maze, you can use bombs to destroy
;; the walls around you.
;;





;; ## Conclusion
;;
;; Even if an input doesn't look like a 2D grid, that doesn't necessarily
;; mean that a task isn't about 2d grids.
;;
;; And it was an easy one. Especially if you consider that this is Day 18.
;;
;; Hopefully, this doesn't mean that tomorrow we'll have something brutal.
;;
;; P.S. Give [aMaze](https://narimiran.github.io/amaze/) a try! :)






^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [data (parse-data input)]
    [(part-1 data)
     (part-2 data)]))
