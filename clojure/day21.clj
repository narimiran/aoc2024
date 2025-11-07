^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day21
  {:nextjournal.clerk/auto-expand-results? true,
   :nextjournal.clerk/toc :collapsed}
  (:require [aoc-utils.core :as aoc]))

;; # Day 21: Keypad Conundrum
;;
;; We're at Santa's Reindeer-class starship and there's a historian in
;; another part of the ship we can't reach because of the locked doors.
;;
;; There is a numeric keypad:
;;
(def numeric-keypad
  {\7 [0 0] \8 [1 0] \9 [2 0]
   \4 [0 1] \5 [1 1] \6 [2 1]
   \1 [0 2] \2 [1 2] \3 [2 2]
            \0 [1 3] \A [2 3]})

;; We need to type some codes on it, for example:
;;
(def example "029A
980A
179A
456A
379A")


;; We can't reach it, but we can send a robot we can control with a
;; directional keypad:
;;
(def directional-keypad
  {         \^ [1 0] \A [2 0]
   \< [0 1] \v [1 1] \> [2 1]})


;; But wait, there are multiple directional keyboards!
;; We're getting ahead of ourselves, let's start from the beginning.





;; ## Input parsing
;;
;; Nothing special to do here, just read each line of the input:
;;
(def example-data (aoc/parse-lines example))
(def data (aoc/parse-lines (aoc/read-input 21)))



;; ## Robot movement
;;
;; To move a robot, for example, from `0` to `4` on the numeric keypad, we
;; need to move two spaces up and one space left.
;; In that order.
;; We cannot first move left, because there is no keypad on the left of `0`,
;; and the move wouldn't be `valid?`:
;;
(defn valid? [keypad pos]
  ((set (vals keypad)) pos))

;; Because we have multiple directional keypads and our aim is to have a
;; minimum amount of keypresses, we always move maximum amount in one direction
;; before moving in the other direction.
;; For example: moving `up, left, up` will never be better than `up, up, left`.
;;
(defn move [n neg-dir pos-dir]
  (vec (repeat (abs n) (if (pos? n) pos-dir neg-dir))))


;; To create an optimal path `from` one point `to` another for a given `keypad`,
;; we have several rules to follow.
;; If we're moving to the left (i.e. `(neg? x)`), we first move to the left
;; if the position would be `valid?` and then `vert`-ically; otherwise,
;; we'll first move vertically, then to the left.
;; If we're moving to the right, then we would want to move first
;; vertically then `hor`-izontally if possible; otherwise, the order is
;; reversed.
;; We finish our move sequence by pressing `A`:
;;
(defn path [keypad [from to]]
  (let [[dx dy] (aoc/pt- (keypad to) (keypad from))
        hor (move dx \< \>)
        vert (move dy \^ \v)]
    (-> (if (neg? dx)
          (if (valid? keypad (aoc/pt+ (keypad from) [dx 0]))
            (into hor vert)
            (into vert hor))
          (if (valid? keypad (aoc/pt+ (keypad from) [0 dy]))
            (into vert hor)
            (into hor vert)))
        (conj \A))))


;; Here are some examples of movement:
;;
(let [a \0
      b \4
      c \3]
  [{"from 0 to 4:" (path numeric-keypad [a b])}
   {"from 4 to 0:" (path numeric-keypad [b a])}
   {"from 3 to 4:" (path numeric-keypad [c b])}
   {"from 4 to 3:" (path numeric-keypad [b c])}])




;; ## Mapping all posible moves
;;
;; Since in part 2 we're dealing with 25 directional keypads, there will
;; be lots of "already seen" movements we shouldn't be calculating, but
;; recalling from `memo`-ry.
;;
;; Each movement starts from the `A` button, and we need to reach all
;; positions in a code one by one:
;;
(defn button-pairs [buttons]
  (map vector (into [\A] buttons) buttons))


;; To have a non-nested list of all movements needed for given `buttons`,
;; we'll use the [`mapcat` function](https://clojuredocs.org/clojure.core/mapcat),
;; which combines calling a `map` and then calling `concat` on its result:
;;
(defn movement [keypad buttons]
  (mapcat #(path keypad %) (button-pairs buttons)))


;; For example:
;;
(let [code "029A"]
  [(button-pairs code)
   (movement numeric-keypad code)])




;; ### Creating a memo
;;
;; For every directional keypad, depending on its `depth` and the `pair`
;; of keys we're interested in the amount of keypresses needed to come
;; `from` one point of the pair `to` the other:
;;
(def memo (atom {}))

(defn calc-from-memo [buttons depth]
  (aoc/sum-map #(@memo [(dec depth) %])
               (button-pairs buttons)))

(defn create-memo [max-depth]
  (doseq [depth (range max-depth)
          from "<^v>A"
          to "<^v>A"]
    (let [pair [from to]
          p (path directional-keypad pair)
          k [depth pair]]
      (swap! memo assoc k
             (if (zero? depth) (count p)
                 (calc-from-memo p depth))))))

(create-memo 25)


;; For example, on depth 0, going from `<` to `A` requires four moves,
;; two to the right, one up and then press `A` to finish the move.
;; But if we were one depth deeper, to instruct a robot (one level deeper)
;; to press `<` and `A`, we first need to instruct our robot to come
;; to `<` (down, left left) and press `A`, and then from there go to `A`
;; (right, right, up) and press `A`, 8 moves in total:
;;
(let [buttons [\< \A]]
  [(path directional-keypad buttons)
   (@memo [0 buttons])
   (movement directional-keypad buttons)
   (@memo [1 buttons])])




;; ## Solution
;;
;; For each line in our input, we need to calculate the complexity of a code
;; to enter.
;; The complexity is a multiplier of the numeric part of the code and the
;; length of the shortest move sequence:
;;
(defn complexity [line depth]
  (let [num (parse-long (subs line 0 3))
        directions (movement numeric-keypad line)
        score (calc-from-memo directions depth)]
    (* num score)))

(defn solve [lines depth]
  (aoc/sum-map #(complexity % depth) lines))


;; ### Part 1
;;
(solve example-data 2)
(solve data 2)

;; ### Part 2
;;
(solve example-data 25)
(solve data 25)



;; ## Conclusion
;;
;; This task was one of the two (the other was Day 24) that I solved in
;; March 2025.
;; It took me a while to come up with the solution which builds a
;; memoized dict of all solutions for all depths.
;; And with it, the task becomes much easier than hoping that I won't make
;; some hard-to-debug error in a recursion.
;;
;; Today's highlights:
;; - `mapcat`: `map` + `concat`






^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [data (aoc/parse-lines input)]
    [(solve data 2)
     (solve data 25)]))
