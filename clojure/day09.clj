^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day09
  {:nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc :collapsed}
  (:require
   aoc))


;; # Day 9: Disk Fragmenter
;;
;; Computer problems. Again.
;; This time it is with a hard drive, not having enough of contiguous space.
;;
;; Currently the disk map looks like this:
;;
(def example "2333133121414131402")



;; ## Input parsing
;;
;; > The disk map uses a dense format to represent the layout of files and
;; > free space on the disk.
;; > The digits alternate between indicating the length of a file and the
;; > length of free space.
;;
;; So we need to "convert" the disk map into a representation with files
;; and free space.\
;; We'll go digit by digit and "unzip" it based on the rules we're given:
;;
(defn parse-data [input]
  (let [digits (aoc/parse-line input :digits)]
    (loop [[hd & tl] digits
           id 0
           file? true
           disk []]
      (cond
        (nil? hd) disk
        file? (recur tl (inc id) false (into disk (repeat hd id)))
        :else (recur tl      id  true  (into disk (repeat hd nil)))))))


(def example-data (parse-data example))
(def data (parse-data (aoc/read-input 9)))




;; ## Part 1
;;
;; Our task is to:
;; > move file blocks one at a time from the end of the disk to the leftmost
;; > free space block
;;
;; We need two "pointers", one starting at the front of the disk
;; (front index, `fi`) and one starting at the back (back index, `bi`).\
;; If there's a file at the front index, we add it to the result.
;; Otherwise, it is an empty space and we add a file which is at the back index
;; (unless that's not an empty space too).
;; We recursively do that, until the front index "overtakes" the back index.
;;
(defn defragment [disk]
  (loop [fi 0
         bi (dec (count disk))
         res []]
    (cond
      (> fi bi) res
      (disk fi) (recur (inc fi)      bi  (conj res (disk fi)))
      (disk bi) (recur (inc fi) (dec bi) (conj res (disk bi)))
      :else     (recur      fi  (dec bi) res))))


;; After we do the (de)fragmentation, we need to calculate the filesystem
;; checksum:
;; > To calculate the checksum, add up the result of multiplying each of
;; > these blocks' position with the file ID number it contains.
;;
;; One useful trick is to use the [`reduce-kv` function](https://clojuredocs.org/clojure.core/reduce-kv),
;; even though we're not having a hashmap, but a vector.
;; This way we automatically extract the index of each element:
;;
(defn calc-checksum [disk]
  (reduce-kv
   (fn [acc idx id]
     (+ acc (* idx id)))
   0
   disk))


;; Putting it all together, we need to (de)fragment the data and then
;; calculate its checksum:
;;
(defn part-1 [data]
  (-> data
      defragment
      calc-checksum))

(part-1 example-data)
(part-1 data)







;; ## Part 2
;;
;; We have a different task now:
;; > rather than move individual blocks, he'd like to try compacting
;; > the files on his disk by moving whole files instead.
;;
;; The disk representation we've used for Part 1 won't be suitable for this.
;; We need to move whole files, which means we need to know not only
;; the (starting) index and the file ID, but also a size of each file.
;; And for empty spaces we also need to know the contiguous space available.
;;
;; We can divide our disk into blocks of same data (either a file or a space)
;; with the [`partition-by` function](https://clojuredocs.org/clojure.core/partition-by),
;; and then for each block we can calculate its starting index and its size.
;;
(partition-by identity example-data)


;; Since we need to keep the blocks in the sorted order, we'll not use a
;; regular hashmap, but a [`sorted-map`](https://clojuredocs.org/clojure.core/sorted-map):
;;
(defn block-sizes [disk]
  (let [blocks (partition-by identity disk)]
    (reduce
     (fn [{:keys [idx] :as acc} block]
       (let [id   (first block)
             size (count block)]
         (if id
           (-> acc
               (assoc-in [:files idx] [id size])
               (update :idx + size))
           (-> acc
               (assoc-in [:spaces idx] size)
               (update :idx + size)))))
     {:files (sorted-map)
      :spaces (sorted-map)
      :idx 0}
     blocks)))


;; This creates two sorted maps we're interested in, one with files and one
;; with spaces.
;; The format for each element is `{index [file-id size]}` for files, and
;; `{index size}` for spaces.
;;
(block-sizes example-data)



;; Before we start moving files, we need a way to find a suitable space for a
;; file to move.
;; Files are always moving to the front, so a suitable space is one with
;; a lower index than the index of a file and with enough space to fit
;; a whole file:
;;
(defn find-space [spaces file-idx file-size]
  (->> spaces
       (take-while (fn [[space-idx _]] (< space-idx file-idx)))
       (aoc/find-first (fn [[_ space-size]] (>= space-size file-size)))))



;; The rules say:
;; > Attempt to move each file exactly once in order of decreasing
;; > file ID number starting with the file with the highest file ID number.
;;
;; We will iterate through the file list in the reverse order (`rseq`).
;; For each file we'll try to find if there is a suitable space in front of
;; it to put it there.\
;; If the free space is the same as the file size, we put the file there
;; and just remove that space.
;; But if the space is larger than the file size, we need to "update" the
;; space: move it after the file we just added and reduce the size of
;; available space.\
;; If there's not enough space to move a file, we leave it where it is and
;; move to the next (well, previous) file:
;;
(defn defragment-2 [disk]
  (let [{:keys [files spaces]} (block-sizes disk)]
    (loop [[[idx [id size]] & tl] (rseq files)
           files files
           spaces spaces]
      (if (nil? idx)
        files
        (if-let [[space-idx space-size] (find-space spaces idx size)]
          (let [spaces' (cond-> (dissoc spaces space-idx)
                          (> space-size size) (assoc (+ space-idx size)
                                                     (- space-size size)))
                files' (-> files
                           (dissoc idx)
                           (assoc space-idx [id size]))]
            (recur tl files' spaces'))
          (recur tl files spaces))))))



;; Our new disk representation needs a new function to calculate the
;; checksum.
;; Since each file is represented as `{index [file-id size]}`, we need to
;; "unpack" it and calculate the checksum for each file block inside the file:
;;
(defn calc-checksum-2 [disk]
  (reduce-kv
   (fn [acc idx [id size]]
     (reduce (fn [acc i]
               (+ acc (* id (+ idx i))))
             acc
             (range size)))
   0
   disk))



;; Part 2 is analogous to Part 1, we just use new functions for defragmentation
;; and calculating the checksum:
;;
(defn part-2 [data]
  (-> data
      defragment-2
      calc-checksum-2))

(part-2 example-data)
(part-2 data)





;; ## Conclusion
;;
;; Today's Part 2 was the hardest task for me this year so far.
;; It took me a very long time until I found a suitable data structure to
;; represent the disk which will allow for easy "updates" as we move
;; the files around.
;;
;; Today's highlights:
;; - `partition-by`: split a collection, based on a function
;; - `reduce-kv`: use on vectors to have indexes
;; - `sorted-map`: a hashmap with sorted keys








^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [data (parse-data input)]
    ((juxt part-1 part-2) data)))
