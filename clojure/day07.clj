^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day07
  {:nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc :collapsed}
  (:require aoc))


;; # Day 7: Bridge Repair
;;
;; We're standing next to a bridge and we need to help a group of engineers
;; with their calibrations: some young elephants stole their operators, so
;; they are left with a list of numbers that looks like this:
;;
(def example "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")




;; ## Input parsing
;;
;; We'll extract all numbers from each line, knowing that the first value is
;; a result of an equation, and the remaining numbers are operands:
;;
(defn parse-data [input]
  (aoc/parse-lines input :ints))

(def example-equations (parse-data example))
(def equations (parse-data (aoc/read-input 7)))




;; ## Solution
;;
;; Since both parts are basically the same, we will have one set of functions
;; to solve both.
;;
;; There are three possible operations:
;; - addition (`2 + 3 = 5`)
;; - multiplication (`2 * 3 = 6`)
;; - concatenation (`2 || 3 = 23`) (only for part 2)
;;
;; The first two are, of course, built-ins. We can define the third as:
;;
(def || (comp parse-long str))



;; For each equation we will recursively check if it is valid:
;; - If the equation contains just one operand, the equation is valid if
;;   that operand is equal to the result.
;; - If the first operand is already higher than the result, the equation
;;   can never be valid (each operation only increases the final value).
;; - Otherwise, we need to try all operations on the first two operands
;;   and recurse with the result of that operation as a first operand.\
;;   e.g. `[a b c d e] --> [(operation a b) c d e]`
;;
(defn valid-equation? [res [a b & tail] concat-op?]
  (let [valid-op? (fn [op]
                    (valid-equation? res (conj tail (op a b)) concat-op?))]
    (cond
      (nil? b)  (when (= res a) res)
      (> a res) nil
      :else (or (valid-op? +)
                (valid-op? *)
                (when concat-op? (valid-op? ||))))))


;; The only difference between part 1 and part 2 is if we use the concatenation
;; operator or not:
;;
(defn valid-1? [[res & ops]]
  (valid-equation? res ops false))

(defn valid-2? [[res & ops]]
  (valid-equation? res ops true))


;; Now we're ready to solve both parts.
;;
;; One thing to notice is that if an equation is valid in the first part, then
;; it will be also valid in the second part (we can use exactly the same operands
;; as in part 1) and there's no need to check it again,
;; especially since finding suitable operators in part 2 is significantly slower,
;; e.g. the longest equations have 11 operations:
;; - part 1: `2^11 = 2048` potential operations to check
;; - part 2: `3^11 = 177,147` potential operations, 86 times more
;;
;; The [`group-by` function](https://clojuredocs.org/clojure.core/group-by)
;; will be useful once again: we'll split the equations in those that
;; satisfy the `valid-1?` functions and those that don't.\
;; To calculate the result for part 1, it is enough to sum all the results
;; (the first element of the list) from the first group.
;;
;; For the second part, we need to check the failing equations if they
;; satisfy the `valid-2?` function (once again we'll use the
;; [`pmap` function](https://clojuredocs.org/clojure.core/pmap) to do that
;; in parallel and gain some performance)
;; and calculate the sum of the results of those which do.
;; In the end, we need to add the result from part 1.
;;
(defn valid-counts [pt1-check pt2-check equations]
  (let [{pt1-eqs true
         pt2-eqs false} (group-by (comp some? pt1-check) equations)
        pt1 (aoc/sum-map first pt1-eqs)
        pt2 (->> pt2-eqs
                 (pmap pt2-check)
                 (filter some?)
                 (reduce +)
                 (+ pt1))]
    [pt1 pt2]))


(defn solve [equations]
  (valid-counts valid-1? valid-2? equations))

(solve example-equations)
(solve equations)







;; ## Optimized solution
;;
;; Although I'm repeatedly saying I'm not chasing performance this year,
;; sometimes I cannot help myself.
;; Especially when the potential benefits are huge.
;;
;; I've written above that for some equations we will have to do almost
;; 200,000 combinations to see if one of them is valid for part 2.
;; We could have a significant speedup if we could prune that search space.
;; And there is a way to do that: start from the end and go backwards.
;;
;; Consider a case where an equation has two elements, `[a b]`.
;; The multiplication is valid, obviously, only if `a * b = result`,
;; i.e. we can _divide_ the `result` by `b` to get `a`.\
;; But this also means in a larger equation that we can check if the result
;; is divisible by the last element, and if it isn't: we don't consider
;; multiplication as a valid operation.
;;
;; A similar check can be done for concatenation.
;; The `a || b = result` operation is valid only if the `result` is `ab`,
;; i.e. the `result` can be _split_ into `a` and `b`.
;;
;; To have an easier time going backwards, we will reverse our equations in
;; the following way: `[res a b c d] --> [res d c b a]`, keeping the result
;; in front and reversing the operations:
;;
(defn reverse-eq [[res & ops]]
  (conj (rseq (vec ops)) res))


;; To check if a multiplication operation is valid, we use `mod` to check if two
;; numbers are divisible.
;; But to check a concatenation operation, we need to write our own function:
;;
(defn concateable? [z y]
  (let [d (loop [d 1]
            (if (> d y) d
                (recur (* 10 d))))]
    (when (= y (mod z d))
      (quot z d))))


;; We're ready to write a function to check if an equation is valid.
;; It looks similar to the original `valid-equation?` function, with the
;; differences discussed above because we're going backwards:
;;
(defn backwards-valid? [res [z y & tail] concat-op?]
  (let [valid-op (fn [x]
                   (backwards-valid? res (conj tail x) concat-op?))]
    (cond
      (empty? tail) (when (= z y) res)
      (> y z) nil
      :else
        (or (valid-op (- z y))
            (and (zero? (mod z y))
                 (valid-op (quot z y)))
            (when-let [x (and concat-op? (concateable? z y))]
              (valid-op x))))))


(defn backwards-valid-1? [eq]
  (backwards-valid? (first eq) eq false))

(defn backwards-valid-2? [eq]
  (backwards-valid? (first eq) eq true))


;; For the optimized solution, we need to first prepare the equations by
;; reversing them, and then we need to pass these new validation functions to
;; the xisting `valid-counts` function we've originally used:
;;
(defn optimized-solve [equations]
  (->> equations
       (map reverse-eq)
       (valid-counts backwards-valid-1? backwards-valid-2?)))

(optimized-solve example-equations)
(optimized-solve equations)




;; ## Benchmarks
;;
;; It's not enough just to claim this new way is faster.
;; Let's back it up with some data.
;;
;; [Again](../day01), we'll use the
;; [`criterium` library](https://github.com/hugoduncan/criterium) for benchmarking.
;;
;; ```
;; (require '[criterium.core :as c])
;;
;;
;; (c/quick-bench (solve equations))
;;
;; Evaluation count : 6 in 6 samples of 1 calls.
;;             Execution time mean : 220.572772 ms
;;    Execution time std-deviation : 14.902858 ms
;;   Execution time lower quantile : 204.924300 ms ( 2.5%)
;;   Execution time upper quantile : 241.599678 ms (97.5%)
;;                   Overhead used : 1.876035 ns)
;;
;;
;; (c/quick-bench (optimized-solve equations))
;;
;; Evaluation count : 354 in 6 samples of 59 calls.
;;             Execution time mean : 1.849690 ms
;;    Execution time std-deviation : 159.477099 µs
;;   Execution time lower quantile : 1.694980 ms ( 2.5%)
;;   Execution time upper quantile : 2.027018 ms (97.5%)
;;                   Overhead used : 1.876035 ns)
;; ```

;; More than 100 times faster!!





;; ## Conclusion
;;
;; Today felt like a natural task to use recursion:
;; we have two simple exit conditions and we know how to get there (by trying
;; all possible combinations of operations).
;; A perfect job for the _Recursion Fairy_:
;;
;; > Your only task is to simplify the original problem, or to solve it directly
;; > when simplification is either unnecessary or impossible;
;; > the Recursion Fairy will magically take care of all the simpler
;; > subproblems for you.
;; >
;; > — Jeff Erickson, Algorithms
;;
;; The key to a good performance is to prune the search space.
;; We achieve this by solving the equations backwards and immediately rejecting
;; the operations which can't produce a valid solution.
;;
;; Today's highlights:
;; - `[a b & tail]`: unpack a seqence into its first two elements and all the rest
;; - `rseq`: a faster way to reverse a vector than using `reverse`









^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (optimized-solve (parse-data input)))
