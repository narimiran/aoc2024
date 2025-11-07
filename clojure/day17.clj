^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day17
  {:nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc :collapsed}
  (:require
   [aoc-utils.core :as aoc]
   [clojure.string :as str]))




;; # Day 17: Chronospatial Computer
;;
;; Today we are in possesion of a 3-bit computer.
;; It has 3 registers and 8 known instructions.
;;
;; This reminds of some previous "assembunny" tasks where the key to
;; a successful Part 2 is to disassemble the instructions, trying to figure
;; out what's really going on.
;;


;; ## Input parsing
;;
;; The input has two paragraphs, but we're interested only in the first
;; line of each.
;;
(defn parse-data [input]
  (let [[[[reg-a]] [program]] (aoc/parse-paragraphs input :ints)]
    [reg-a program]))

(def data (parse-data (aoc/read-input 17)))







;; ## Running the computer
;;
;; We can run the computer as it has been written in the task or we can try
;; to "decompile" the program.
;; We will write both versions and compare their performance.
;;
;;
;;
;; ### Naive way
;;
(defn combo [{:keys [a b c]} operand]
  (case operand
    4 a
    5 b
    6 c
    operand))

(defn run-program [reg-a program]
  (loop [{:keys [a c ip] :as state} {:a reg-a :b 0 :c 0 :out "" :ip 0}]
    (if (>= ip (count program))
      (:out state)
      (let [instr (program ip)
            operand (program (inc ip))
            comb-op (mod (combo state operand) 8)
            bsl (bit-shift-left 1 comb-op)
            state' (case instr
                     0 (update state :a quot bsl)
                     1 (update state :b bit-xor operand)
                     2 (assoc state :b comb-op)
                     3 (cond-> state
                         (pos? a) (assoc :ip (- operand 2)))
                     4 (update state :b bit-xor c)
                     5 (update state :out str comb-op)
                     6 (assoc state :b (quot a bsl))
                     7 (assoc state :c (quot a bsl)))]
        (recur (update state' :ip + 2))))))





;; ### Decompiling
;;
;; Instead of executing instructions one by one, following the input program,
;; I manually "translated" each input instruction.\
;; The program is very simple and depends only on the register `A`.
;;
;; We have a multi-arity function, as we keep the output as the second
;; argument to allow the tail-recursion with `recur`:
;;
(defn decompile
  ([a] (decompile a ""))
  ([a out]
   (if (zero? a)                           ; JNZ 0
     out
     (recur (quot a 8)
            (-> a
                (mod 8)                    ; BST 4
                (bit-xor 1)                ; BXL 1
                ((fn [b]                   ; CDV 5
                   (->> b
                        (bit-shift-left 1)
                        (quot a)
                        (bit-xor b))))     ; BXC 4
                (bit-xor 4)                ; BXL 4
                (mod 8)
                (->> (str out)))))))       ; OUT 5





;; ## Part 1
;;
;; The task is asking us to return a comma-separated output of the program:
;;
(defn part-1 [reg-a solve-fn]
  (str/join "," (solve-fn reg-a)))


;; Testing both versions, we get the same results:
;;
(let [[reg-a program] data]
  [(part-1 reg-a #(run-program % program))
   (part-1 reg-a decompile)])






;; ## Part 2
;;
;; In this part we need to find the initial value of the register `A` that
;; produces an output which is the same as the program input.
;;
;; We can do that going backwards, from the last digit going forward when
;; we find the suitable value.
;; Due to the instructions of the program (easily visible in the `decompile`
;; function), when running a program, the next digit in the output is
;; `a % 8`, and the next iteration starts with `a // 8`.\
;; This means when going backwards, the next value is always in the
;; following set of candidates: `8*current + {0..7}`.
;;
;; For each valid candidate so far, we check the set of potential candidates
;; and keep the ones for which running the program would output the expected
;; digits:
;;
(defn part-2 [program solve-fn]
  (let [output (str/join program)]
    (loop [candidates [0]
           idx (dec (count output))]
      (let [wnt (subs output idx)
            cands' (for [n candidates
                         m (range 8)
                         :let [n' (+ (* 8 n) m)
                               res (solve-fn n')]
                         :when (= wnt res)]
                     n')]
        (if (zero? idx)
          (first cands')
          (recur cands' (dec idx)))))))


;; Once again we're running both versions:

(let [[_ program] data]
  [(part-2 program #(run-program % program))
   (part-2 program decompile)])



;; ## Benchmarking
;;
;; How does the decompiled version compare to the naive version?
;; Let's find out:
;;
;; ```
;; (require '[criterium.core :as c])
;;
;;
;; (let [[_ program] data]
;;   (c/quick-bench (part-2 program #(run-program % program))))])
;;
;; Evaluation count : 258 in 6 samples of 43 calls.
;;              Execution time mean : 2.472719 ms
;;     Execution time std-deviation : 87.113609 µs
;;    Execution time lower quantile : 2.398153 ms ( 2.5%)
;;    Execution time upper quantile : 2.596704 ms (97.5%)
;;                    Overhead used : 1.794997 ns
;;
;;
;; (let [[_ program] data
;;   (c/quick-bench (part-2 program decompile)))])
;;
;; Evaluation count : 3072 in 6 samples of 512 calls.
;;              Execution time mean : 217.584345 µs
;;     Execution time std-deviation : 25.630465 µs)
;;    Execution time lower quantile : 196.510324 µs ( 2.5%)
;;    Execution time upper quantile : 250.980019 µs (97.5%)
;;                    Overhead used : 1.794997 ns])
;; ```
;;
;;
;; The decompiled version is around 10 times faster!






;; ## Conclusion
;;
;; Today was mostly about figuring out what the program does, and then
;; to come up with a way to build the output we want.
;;
;; Will we have another task with this computer, like it was in 2019
;; with the Intcode computer?







^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [[reg-a program] (parse-data input)]
    [(part-1 reg-a decompile)
     (part-2 program decompile)]))
