(ns repertoire-maker.history-test
  (:require
   [repertoire-maker.history :as sut]
   [repertoire-maker.util.test :refer [close-to]]
   [clojure.test :as t :refer [deftest is]]))

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

;; TODO: Add API fixtures to test this
;; (deftest initialize-moveset-test

;; (sut/initialize-moveset
;;  {:moves ["e2e4"]
;;   :color :white
;;   :local? true})
;;   )
