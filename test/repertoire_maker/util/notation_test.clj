(ns repertoire-maker.util.notation-test
  (:require [repertoire-maker.util.notation :as sut]
            [clojure.test :as t :refer [deftest is]]))

(deftest sans->ucis-test
  (is (= ["e2e4" "e7e5" "g1f3"] (sut/sans->ucis ["e4" "e5" "Nf3"]))))

(deftest sans->fen-test
  (is
   (= "rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2"
      (sut/sans->fen ["e4" "e5" "Nf3"]))))
