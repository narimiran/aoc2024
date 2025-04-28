^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day24
  {:nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc :collapsed}
  (:require
   aoc
   [clojure.string :as str]))


;; # Day 24: Crossed Wires
;;
;; We're in the jungle and there are some wires we need to untangle that
;; look like this:
;;
(def example "x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02")

;; Where the first paragraph are the initial values for each wire,
;; and the second paragraph shows how they are connected.



;; ## Input parsing
;;
;; We will `parse-paragraphs` and extract all `:words` from each line.
;; The words are separated either by `: ` or ` -> ` or plain old space (` `).
;;
;; For the first paragraph, we extract the name and the value:
;;
(defn parse-inits [[n v]]
  [n (parse-long v)])


;; For the second paragrah, we immediately convert the connections to
;; appropriate functions:
;;
(defn parse-conns [[a op b c]]
  (let [ops {"AND" bit-and
             "OR" bit-or
             "XOR" bit-xor}]
    [(ops op) a b c]))

(defn parse-data [input]
  (let [[inits conns] (aoc/parse-paragraphs input :words #" -> |: | ")
        inits (into {} (mapv parse-inits inits))
        conns (mapv parse-conns conns)]
    [inits conns]))

(def example-data (parse-data example))
(def data (parse-data (aoc/read-input 24)))




;; ## Part 1
;;
;; In the first part, our task is to run the device, based on the initial
;; values in the system.
;;
;; A gate will produce a value only if both inputs have a value.\
;; We will repeatedly go through all connections, trying to produce a value
;; where inputs are available, until we have a value in all `z` wires.
;;
;; Not very efficient, but it does the job:
;;
(defn run-device [[inits conns]]
  (let [count-zs (fn [coll] (aoc/count-if #(str/starts-with? % "z") coll))
        all-zs (count-zs (map last conns))]
    (loop [wires inits]
      (let [known-zs (count-zs (keys wires))]
        (if (= known-zs all-zs)
          wires
          (recur
           (reduce (fn [acc [op a b c]]
                     (cond-> acc
                       (and (acc a)
                            (acc b)
                            (not (acc c)))
                       (assoc c (op (acc a) (acc b)))))
                   wires
                   conns)))))))


;; Once the device has run, we need to extract the decimal value from a
;; binary number in all `z` wires:
;;
(defn extract-value [wires]
  (let [zs (->> wires
                (filter (fn [[k _]] (str/starts-with? k "z")))
                sort
                reverse
                vals
                str/join)]
    (Long/parseLong zs 2)))


(defn part-1 [data]
  (-> data
      run-device
      extract-value))


(part-1 example-data)
(part-1 data)




;; ## Part 2
;;
;; Here we need to find pairs of wires that have been swapped.
;;
;; To be honest, I have no idea what's going on here and why this
;; works — I've seeked for help on Reddit, and this is how people
;; solved this part; I just translated it to Clojure.
;;
(defn wire-exists? [wires wire op']
  (some (fn [[op a b _]]
          (and (= op' op)
               (#{a b} wire)))
        wires))

(defn out-is-in [wires [op a b c] op' op'']
  (and (= op' op)
       (not (#{a b} "x00"))
       (wire-exists? wires c op'')))

(defn find-wrong [wires]
  (->> (for [[op a b c :as wire] wires
             :when (or (and (not= bit-xor op)
                            (= \z (first c))
                            (not= "z45" c))
                       (and (= bit-xor op)
                            (not-any? #{\x \y \z} (map first [a b c])))
                       (out-is-in wires wire bit-xor bit-or)
                       (out-is-in wires wire bit-and bit-xor))]
         c)
       sort
       (str/join ",")))

(defn part-2 [[_ wires]]
  (find-wrong wires))

(part-2 example-data)
(part-2 data)




;; ## Conclusion
;;
;; My Part 1 solution is ugly and inefficient, and Part 2
;; solution isn't even mine.





^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [data (parse-data input)]
    [(part-1 data)
     (part-2 data)]))
