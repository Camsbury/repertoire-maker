(ns repertoire-maker.strategy-test
  (:require [repertoire-maker.strategy :as sut]
            [clojure.test :refer [is deftest]]))

(deftest choose-move-test
  (is (=
       {:uci "d2d4",
        :score 0.6295343100667656,
        :chosen? true,
        :pct 0.0011231319091159076,
        :moves ["e2e4" "e7e5" "g1f3" "b8c6" "f1b5" "g8e7" "b1c3" "g7g6" "h2h4" "h7h6" "d2d4"]}
       (sut/choose-move
        {:use-engine? true,
         :move-choice-pct 0.01,
         :overrides {},
         :color :white,
         :uci "h7h6",
         :white 0.55851066,
         :moves ["e2e4" "e7e5" "g1f3" "b8c6" "f1b5" "g8e7" "b1c3" "g7g6" "h2h4" "h7h6"],
         :play-pct 0.3983051,
         :pct 0.0011231319091159076,
         :lichess [{:uci "d2d4",
                    :white 0.6375,
                    :black 0.3,
                    :play-count 80,
                    :play-pct 0.41666666}
                   {:uci "h4h5",
                    :white 0.54901963,
                    :black 0.39215687,
                    :play-count 51,
                    :play-pct 0.265625}
                   {:uci "d2d3",
                    :white 0.5365854,
                    :black 0.41463414,
                    :play-count 41,
                    :play-pct 0.21354167}
                   {:uci "c3d5",
                    :white 0.3,
                    :black 0.7,
                    :play-count 10,
                    :play-pct 0.052083332}
                   {:uci "b5c4",
                    :white 0.25,
                    :black 0.75,
                    :play-count 4,
                    :play-pct 0.020833334}
                   {:uci "a2a3",
                    :white 0.5,
                    :black 0.5,
                    :play-count 4,
                    :play-pct 0.020833334}
                   {:uci "b2b3",
                    :white 0.0,
                    :black 1.0,
                    :play-count 1,
                    :play-pct 0.0052083335}
                   {:uci "g2g4",
                    :white 1.0,
                    :black 0.0,
                    :play-count 1,
                    :play-pct 0.0052083335}],
         :filter-pct 0.001,
         :allowable-loss 0.1,
         :engine [{:uci "d2d4", :score 0.6295343100667656}
                  {:uci "h4h5", :score 0.5604570759825421}
                  {:uci "b5c6", :score 0.5036820134406647}
                  {:uci "d2d3", :score 0.5}
                  {:uci "b5a4", :score 0.4659933421436654}
                  {:uci "a2a4", :score 0.454103562397001}
                  {:uci "b5c4", :score 0.44954372531791664}
                  {:uci "c3d5", :score 0.44499235158754635}
                  {:uci "a2a3", :score 0.4322998523826144}
                  {:uci "f3e5", :score 0.4232868990440539}],
         :play-count 188,
         :black 0.3882979}))))
