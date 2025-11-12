^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day22
  {:nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc :collapsed}
  (:require [aoc-utils.core :as aoc]))


;; # Day 22: Monkey Market
;;
;; Today we are in the jungle and we're trying to buy bananas on the Monkey
;; Exchange Market with pseudorandom prices.


;; ## Input parsing
;;
;; Nothing fancy needed here, just a list of numbers that looks like this:
;;
(def example "1
10
100
2024")

(defn parse-data [input]
  (aoc/parse-lines input :int))

(def example-data (parse-data example))
(def data (parse-data (aoc/read-input 22)))


;; ## Creating a secret number
;;
;; The process of creating a secret number consists of three similar steps, in
;; which we multiply (`bit-shift-left`) or divide (`bit-shift-right`) a number
;; and then we "mix" (`bit-xor`) and "prune" (`bit-and`) it:
;;
(defn mix-prune [n i]
  (-> n
      (bit-xor i)
      (bit-and 0xFFFFFF)))

(defn step-1 [n]
  (-> n
      (bit-shift-left 6)
      (mix-prune n)))

(defn step-2 [n]
  (-> n
      (bit-shift-right 5)
      (mix-prune n)))

(defn step-3 [n]
  (-> n
      (bit-shift-left 11)
      (mix-prune n)))

(defn calc-secret [n]
  (-> n
      step-1
      step-2
      step-3))


;; We need to repeat the process 2000 times, and the
;; [`iterate` function](https://clojuredocs.org/clojure.core/iterate) is
;; exactly what we need for that:
;;
(defn calc-2000 [n]
  (-> (iterate calc-secret n)
      (nth 2000)))


;; ## Part 1
;;
;; In the first part, all we need to do is calculate 2000th secret number for
;; each line of the input, and sum those numbers:
;;
(defn part-1 [data]
  (aoc/sum-pmap calc-2000 data))

(part-1 example-data)
(part-1 data)





;; ## Part 2
;;
;; In the second part we're interested in all 2000 iterations of secret
;; numbers. More precisely, we need only the last digit of every number:
;;
(defn get-2000-digits [n]
  (->> (iterate calc-secret n)
       (take 2000)
       (mapv #(mod % 10))))


;; We need to find price changes at which, if the monkeys sell the bananas,
;; we will gain the most money.
;;
;; Instead of using a vector of four numbers as a key in our map, we can
;; "hash" them in the following way:
;;
(defn change-hash [change]
  (reduce (fn [acc x]
            (+ (* 20 acc) x 10))
          0
          change))

;; Since we'll encounter some change sequences multiple times,
;; we can `memoize` this function to retrieve the result faster:
;;
(def change-hash-memo (memoize change-hash))

;; Now we can create a map from price changes to prices at that point.
;; If we see the same change multiple times, we're interested only in the
;; first time it happens: that's why we are using the
;; [`rseq` function](https://clojuredocs.org/clojure.core/rseq)
;; before creating a map with the
;; [`zipmap` function](https://clojuredocs.org/clojure.core/zipmap),
;; so that the earlier occurrence overwrites the later ones:
;;
(defn create-change-map [digits changes]
  (zipmap (rseq changes) (rseq digits)))

(defn price-changes [n]
  (let [digits (get-2000-digits n)]
    (->> digits
         (map - (rest digits))
         (partition 4 1)
         (mapv change-hash-memo)
         (create-change-map digits))))


;; We will do that for each number in our input. (We're using `pmap` to
;; speed things up.)
;; After we have the results, we need to combine them with the
;; [`merge-with` function](https://clojuredocs.org/clojure.core/merge-with)
;; to see at which price change sequence we will gain the most bananas overall:
;;
(defn part-2 [data]
  (->> data
       (pmap price-changes)
       (apply merge-with +)
       vals
       (reduce max)))


(part-2 example-data)
(part-2 data)




;; ## Faster solution
;;
;; While revisiting these notebooks in November 2025, I realized that this
;; task was the only one that takes more than one second on machine to get
;; the solution for both parts. Unacceptable!
;;
;; There are two major changes:
;; - We won't be calculating all 2000 price changes at once and keep them in
;;   memory.
;; - We'll use Java array instead of hasmaps to keep the scores.
;;
;; This allows us to write a solution that is ~3x faster than the one above,
;; even though that one runs in parallel for each number in our input.
;;
;; We will have a sliding window we can calculate like this:
;;
(defn calc-window [window n' n]
  (mod (- (+ (* 20 window)
             (mod n' 10))
          (mod n 10))
       160000)) ; 20^4


;; We will have a single array of 160,000 elements where we'll store
;; results for each "hash" of four consecutive changes.\
;; For each number in the input we'll have to keep track of already
;; `seen` windows, as only the first one should be added to the array.
;;
(defn part-2-array [data]
  (let [a (short-array 160000 (short 0))]
    (run! (fn [n]
            (loop [window 0
                   n n
                   cnt 2001
                   seen #{}]
              (when-not (zero? cnt)
                (let [n' (calc-secret n)
                      w' (calc-window window n' n)]
                  (if (> cnt 1997)
                    (recur w' n' (dec cnt) seen)
                    (do
                      (when-not (seen window)
                        (aset-short a window (+ (mod n 10) (aget a window))))
                      (recur w' n' (dec cnt) (conj seen window))))))))
          data)
    (reduce max a)))


(part-2-array data)




;; ## Conclusion
;;
;; Part 1 is very straight-forward, especially for a task this late.
;; The second part takes a bit more coding to get what we need, but still doable.
;;
;; Today's hightlights:
;;
;; - `iterate`: call a function multiple times, each time on a result of a
;;   previous iteration, i.e. `(f (f (f x)))`.
;; - `zipmap`: create a map from sequences of keys and vals.
;; - `merge-with`: combine multiple maps by applying a function to the
;;   keys that appear in more than one map.






^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [data (parse-data input)]
    [(part-1 data)
     (part-2-array data)]))
