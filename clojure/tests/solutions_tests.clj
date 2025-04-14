(ns solutions-tests
  (:require
   day01 day02 day03 day04 day05
   day06 day07 day08 day09 day10
   day11 day12 day13 day14 day15
   day16 day17 day18 day19 day20
   day21 day22 day23 ;day24 day25
   [clojure.test :refer [deftest is run-tests successful?]]))



(defmacro check-day [day test-results real-results]
  (let [day        (format "%02d" day)
        full-day   (str "day" day)
        main-func  (symbol full-day "-main")
        test-name  (symbol (str full-day "-test"))
        test-input (str day "_test")]
    `(deftest ~test-name
       (when ~test-results
         (is (= ~test-results (~main-func (aoc/read-input ~test-input)))))
       (is (= ~real-results (~main-func (aoc/read-input ~day)))))))




(check-day 1 [11 31] [1222801 22545250])
(check-day 2 [2 4] [334 400])
(check-day 3 [161 48] [167090022 89823704])
(check-day 4 [18 9] [2534 1866])
(check-day 5 [143 123] [5091 4681])
(check-day 6 [41 6] [5131 1784])
(check-day 7 [3749 11387] [1985268524462 150077710195188])
(check-day 8 [14 34] [259 927])
(check-day 9 [1928 2858] [6279058075753 6301361958738])
(check-day 10 [36 81] [820 1786])
(check-day 11 [55312 65601038650482] [183435 218279375708592])
(check-day 12 [1930 1206] [1464678 877492])
(check-day 13 [480 875318608908] [27105 101726882250942])
(check-day 14 nil [226236192 8168])
(check-day 15 10092 1509863)
(check-day 16 [7036 45] [79404 451])
(check-day 17 nil ["7,4,2,0,5,0,5,3,7" 202991746427434])
(check-day 18 nil [384 "36,10"])
(check-day 19 [6 16] [240 848076019766013])
(check-day 20 nil [1445 1008040])
(check-day 21 [126384 154115708116294] [222670 271397390297138])
(check-day 22 [37327623 24] [18694566361 2100])
(check-day 23 [7 "co,de,ka,ta"] [1110 "ej,hm,ks,ms,ns,rb,rq,sc,so,un,vb,vd,wd"])


(let [summary (run-tests)]
  (when-not (successful? summary)
    (throw (Exception. "tests failed"))))
