(ns repertoire-maker.core-test
  (:require [repertoire-maker.core :as sut]
            [clojure.test :as t :refer [deftest is]]))

(defn close-to
  [a b]
  (< (Math/abs (- a b)) 1e-5))

(deftest process-option-test
  (let [prior-total   10000
        total-moves   1000
        white-win-pct 0.55
        black-win-pct 0.4
        processed     (sut/process-option
                       prior-total
                       {:uci   "e2e4"
                        :white (* white-win-pct total-moves)
                        :draws (-> 1 (- white-win-pct) (- black-win-pct) (* total-moves))
                        :black (* black-win-pct total-moves)})]
    (is (= "e2e4" (:uci processed)))
    (is (close-to white-win-pct (:white processed)))
    (is (close-to black-win-pct (:black processed)))
    (is (close-to total-moves (:play-count processed)))
    (is (close-to (double (/ total-moves prior-total)) (:play-pct processed)))))