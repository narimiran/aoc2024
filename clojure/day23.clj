^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day23
  {:nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc :collapsed}
  (:require
   aoc
   [clojure.string :as str]))



;; # Day 23: LAN Party
;;
;; Let's see if we can get invited to a LAN party (what year is it?).
;;
;; We have a list of connections between computers that looks like this:
;;
(def example "kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn")



;; ## Input parsing
;;
;; Each line in the input is a connection, and they are symmetrical,
;; i.e. `a-b` is also `b-a`.
;;
;; My parsing helper can also have a custom word separator, which we
;; need here to split the lines on `-`.
;;
;; We will create a hashmap where each computer will have a set of its
;; connections:
;;
(defn parse-data [input]
  (let [lines (aoc/parse-lines input :words {:word-sep #"-"})
        conj' (fnil conj #{})]
    (reduce
     (fn [acc [a b]]
       (-> acc
           (update a conj' b)
           (update b conj' a)))
     {}
     lines)))


(def example-data (parse-data example))
(def data (parse-data (aoc/read-input 23)))




;; ## Part 1
;;
;; In Part 1 we're asked to find
;; > sets of three computers where each computer in the set is connected
;;   to the other two computers.
;;
;; and to
;; > consider only sets of three computers where at least one computer's
;;   name starts with `t`.
;;
;; For each computer `k` whose name starts with `t` we will check each pair
;; of its connections (`v1` and `v2`) if they are interconnected.
;; We already know that they are connected with `k`, so we only need to check
;; if `v1` and `v2` have a connection between them.
;;
(defn find-triplets [connections]
  (into #{} (for [[k vs] connections
                  :when (str/starts-with? k "t")
                  v1 vs
                  v2 vs
                  :while (not= v1 v2)
                  :when ((connections v1) v2)]
              #{k v1 v2})))


;; We need to `count` how many of those interconnected triplets exists:
;;
(defn part-1 [connections]
  (count (find-triplets connections)))


(part-1 example-data)
(part-1 data)






;; ## Part 2
;;
;; The LAN party will be held only between the largest group of computers
;; that are all interconnected.
;;
;; For each computer, we will find such a group.
;; We start by having only that computer in a `group`, and then for every
;; computer `k` in the input, if it is connected with `every?`
;; computer already in the `group`, we add it to the `group`:
;;
(defn clique [connections pt]
  (reduce-kv
   (fn [group k vs]
     (if (every? vs group)
       (conj group k)
       group))
   #{pt}
   connections))


;; Now we repeat that for every computer.
;; We're only interested in the largest group.
;;
(defn part-2 [connections]
  (->> (reduce (fn [acc pt]
                 (let [clq (clique connections pt)]
                   (if (> (count clq) (count acc))
                     clq
                     acc)))
               #{}
               (keys connections))
       sort
       (str/join #",")))


(part-2 example-data)
(part-2 data)



;; ## Conclusion
;;
;; Basically, every function in this task uses sets.
;; They're nice to quickly test membership and to keep only unique values,
;; without duplications.\
;; Other than that, nothing to highlight here.






^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [data (parse-data input)]
    [(part-1 data)
     (part-2 data)]))
