^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day05
  {:nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc :collapsed}
  (:require
   aoc))


;; # Day 5: Print Queue
;;
;; Were at the _North Pole printing department_ and there's one
;; _very familiar printer_!
;; We haven't seen it in 7 years, and now we need to fix it again.
;;
;; > The Elf has for you both the page ordering rules and
;; > the pages to produce in each update
;;
;; It looks like this:
;;
(def example "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")





;; ## Input parsing
;;
;; We have two separate groups of numbers, separated by a blank line.
;; Another [AoC helper](../aoc) to the rescue: `aoc/parse-paragraphs`,
;; which separates each group in it's own list.\
;; For each group, we extract integers from each line.
;;
(defn parse-data [input]
  (aoc/parse-paragraphs input :ints))

(def example-data (parse-data example))
(def data (parse-data (aoc/read-input 5)))





;; ## Dynamic rules
;;
;; We will have to check if the numbers follow the rules in multiple functions.
;; This means we'll have to either pass the rules as an argument to each
;; function, or have the rules defined as a global variable.
;;
;; The former idea will work fine, but it makes some function invocations
;; uglier than they would be if they only had a single argument.
;; (And we're not here to be content with ugly code!)\
;; The latter idea, with a global variable, will not work because
;; we're always solving both for the example input and the real one.
;; We cannot have a single global variable that works in both cases.
;;
;; Enter dynamic vars!
;;
;; This will allow us to use specific rules, depending on the input we're
;; using.\
;; We will `bind` the rules in our main functions and that value will be
;; "seen" by every function we call from there.\
;; See the documentation of the [`binding` macro](https://clojuredocs.org/clojure.core/binding)
;; to see some examples how this is used.
;;
;; We will mark `rules` as a `^:dynamic` variable, and by convention we need
;; to put "earmuffs" around it:
;;
(def ^:dynamic *rules* #{})





;; ## Part 1
;;
;; It seems that inputs are created in such a way that for every pair of
;; numbers in the updates list, there is a rule which says which one goes
;; before the other.
;; (More about that in a later section.)
;;
;; This means it is enough to check if all adjacent numbers
;; (we'll use `(partition 2 1 ...)` for that, like we did in [Day 2](../day02))
;; in a line follow the rules:
;;
(defn valid-line? [line]
  (every? *rules* (partition 2 1 line)))


;; > Determine which updates are already in the correct order.
;; > What do you get if you add up the middle page number from those
;; > correctly-ordered updates?
;;
;; From the list of updates, we will `filter` the lines which are in the
;; correct order, and for each of those we need to `sum` the numbers that are
;; in the middle of each line:
;;
(defn middle-page [line]
  (line (quot (count line) 2)))

(defn part-1 [[rules updates]]
  (binding [*rules* (set rules)]
    (->> updates
         (filter valid-line?)
         (aoc/sum-map middle-page))))

(part-1 example-data)
(part-1 data)


;; As you can see in the code above, each time we call the `part-1` function,
;; we bind the `*rules*` variable to have a new value, depending on the
;; argument we're passing in.\
;; When we call the `valid-line?` function, it will see this new value for
;; `*rules*` and we'll successfully `filter` the correctly-ordered updates.






;; ## Part 2
;;
;; Ok, we've successfully discovered the correctly-ordered updates, but now:
;;
;; > For each of the incorrectly-ordered updates,
;; > use the page ordering rules to put the page numbers in the right order.
;;
;; We can still use the `valid-line?` function as a predicate, but this time
;; we will [`remove`](https://clojuredocs.org/clojure.core/remove) the lines
;; which are in the correct order.
;;
;; For the lines that remain, we need to
;; [`sort`](https://clojuredocs.org/clojure.core/sort) them so they follow
;; the rules, and then take the middle number.\
;; We will write a custom function to use with `sort`: if two numbers are
;; in the `*rules*`, they are in the correct order (the first one is "lower"
;; than the second one):
;;
(defn sort-line [line]
  (->> line
       (sort (fn [a b]
               (if (*rules* [a b]) -1 1)))
       vec))


;; The result of a `sort` is a sequence, and we need to convert it into
;; a vector to be able to take the middle element with the
;; `middle-page` function.
;;
;; The complete part 2 is now:
;;
(defn part-2 [[rules updates]]
  (binding [*rules* (set rules)]
    (->> updates
         (remove valid-line?)
         (map sort-line)
         (aoc/sum-map middle-page))))

(part-2 example-data)
(part-2 data)






;; ## Both parts at once
;;
;; We can see that the `part-1` and `part-2` functions are very similar.
;; No need to repeat ourselves, we can have a single function which solves
;; both parts.
;;
;; First we'll split the updates in valid and invalid ones.
;; We will use the [`group-by` function](https://clojuredocs.org/clojure.core/group-by).
;; This will give us a hashmap with two keys: `true` (for the updates which
;; satisfy the `valid-line?` predicate) and `false` (for the invalid updates),
;; so we'll immediately unpack those, since we're only interested in their values.
;;
;; After that, we'll have to sort the invalid ones, and then for each group
;; we will calculate the sum of middle pages.
;;
(defn solve [[rules updates]]
  (binding [*rules* (set rules)]
    (let [{valid   true
           invalid false} (group-by valid-line? updates)
          sorted-invalid  (map sort-line invalid)]
      (mapv #(aoc/sum-map middle-page %) [valid sorted-invalid]))))

(solve example-data)
(solve data)




;; ## Nice inputs
;;
;; I've claimed in the Part 1 section that we have nice inputs which simplify
;; sorting.
;; Let's briefly see what's going on.
;;
;; For each input, we have some number of rules.\
;; How many unique numbers are there in the updates?
;; Is there any connection between the two?
;;
;; Let's explore that with our real input:
;;
(defn find-unique-numbers [[_ updates]]
  (-> updates
      flatten
      set
      count))

(def unique-numbers (find-unique-numbers data))


;; We can have `n * (n - 1) / 2` different combinations of `n` different numbers.
;; For our case that is:
(/ (* unique-numbers (dec unique-numbers)) 2)

;; And how many rules do we have?
(count (first data))

;; There you go!
;; We have a rule for every possible combination of two numbers, that's why
;; we could solve it this way.







;; ## Conclusion
;;
;; Initially this task looked way harder than it really was in the end.
;; (Well, most of them do, when you open them at 6 a.m.)
;;
;; Fortunately, the inputs are created in such way that makes sorting the
;; updates easy.
;;
;; Today's highlights:
;; - `^:dynamic`: declare a dynamic variable
;; - `binding`: bind a value for a dynamic variable
;; - `group-by`: split a collection into groups, based on a predicate








^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (solve (parse-data input)))
