(ns repertoire-maker.export-test
  (:require
   [flatland.ordered.map  :refer [ordered-map]]
   [repertoire-maker.export :as sut]
   [repertoire-maker.test :refer [close-to]]
   [clojure.test :as t :refer [is deftest]]))


(deftest weighted-stat-test
  (let [move-tree-1
        #ordered/map
        (["e2e4"
          {:uci "e2e4",
           :chosen? true
           :white 0.48112983,
           :black 0.46624005,
           :play-count 13731626,
           :play-pct 0.46319923,
           :pct 1.0,
           :responses
           #ordered/map
           (["c7c5"
             {:uci "c7c5",
              :white 0.47199076,
              :black 0.47401774,
              :play-count 3971904,
              :play-pct 0.28970253,
              :pct 0.28970253467559814,
              :responses #ordered/map
              (["b1c3"
                {:uci "b1c3",
                 :chosen? true
                 :white 0.5065461,
                 :black 0.44328055,
                 :play-count 571638,
                 :play-pct 0.14411178,
                 :pct 0.28970253467559814,
                 :score nil,
                 :responses
                 #ordered/map
                 (["b8c6"
                   {:uci "b8c6",
                    :white 0.50729805,
                    :black 0.44163746,
                    :play-count 228280,
                    :play-pct 0.39018154,
                    :pct 0.1130365815396317,
                    :responses
                    #ordered/map
                    (["f1b5"
                      {:uci "f1b5",
                       :chosen? true
                       :white 0.52684957,
                       :black 0.41693756,
                       :play-count 23073,
                       :play-pct 0.101094946,
                       :pct 0.1130365815396317,
                       :score nil}])}])}])}]
            ["e7e5"
             {:uci "e7e5",
              :white 0.496853,
              :black 0.44970325,
              :play-count 2530809,
              :play-pct 0.18459202,
              :pct 0.18459202349185944,
              :responses
              #ordered/map
              (["b1c3"
                {:uci "b1c3",
                 :chosen? true
                 :white 0.53959924,
                 :black 0.41176262,
                 :play-count 227517,
                 :play-pct 0.09004198,
                 :pct 0.18459202349185944,
                 :score nil}])}]
            ["e7e6"
             {:uci "e7e6",
              :white 0.48720765,
              :black 0.4590694,
              :play-count 1926700,
              :play-pct 0.14052954,
              :pct 0.1405295431613922,
              :responses
              #ordered/map
              (["d1e2"
                {:uci "d1e2",
                 :chosen? true
                 :white 0.53444946,
                 :black 0.4139658,
                 :play-count 27954,
                 :play-pct 0.014529083,
                 :pct 0.1405295431613922,
                 :score nil}])}]
            ["c7c6"
             {:uci "c7c6",
              :white 0.47020537,
              :black 0.47346783,
              :play-count 1419786,
              :play-pct 0.10355628,
              :pct 0.10355628281831741,
              :responses
              #ordered/map
              (["c2c4"
                {:uci "c2c4",
                 :chosen? true
                 :white 0.49715963,
                 :black 0.44314045,
                 :play-count 48057,
                 :play-pct 0.033890262,
                 :pct 0.10355628281831741,
                 :score nil}])}])}])
        move-tree-2
        #ordered/map
        (["e2e4"
          {:responses
           #ordered/map
           (["e7e5"
             {:uci "e7e5",
              :white 0.49684057,
              :play-pct 0.1844795,
              :pct 0.4630254805088043,
              :responses
              #ordered/map
              (["g1f3"
                {:uci "g1f3",
                 :white 0.4958433,
                 :play-pct 0.6675728,
                 :play-count 1696657,
                 :black 0.44761965,
                 :pct 0.3091032148021373,
                 :responses
                 #ordered/map
                 (["d7d5"
                   {:uci "d7d5",
                    :white 0.46160054,
                    :play-pct 0.03427448,
                    :pct 0.3091032148021373,
                    :score nil,
                    :responses
                    #ordered/map
                    (["e4d5"
                      {:uci "e4d5",
                       :white 0.46910626,
                       :play-pct 0.45032343,
                       :play-count 26316,
                       :black 0.48350814,
                       :pct 0.13919642074334118,
                       :responses
                       #ordered/map
                       (["e5e4"
                         {:uci "e5e4",
                          :white 0.46149877,
                          :play-pct 0.80595267,
                          :pct 0.13919642074334118,
                          :score nil,
                          :chosen? true,
                          :play-count 21311,
                          :black 0.48984092}])}]),
                    :chosen? true,
                    :play-count 58243,
                    :black 0.4892777}])}]),
              :stack [e2e4 e7e5],
              :chosen? true,
              :play-count 2545563,
              :black 0.449697}])}])]
    (is (close-to
         (+
          ; c5, Nc3
          (+ (* 0.113 0.5269)
             (* 0.289
                ;; parent component
                (- 0.5065 (* 0.39 0.5073))))
          ; e5
          (* 0.1846 0.5396)

          ; e6
          (* 0.1405 0.5344)

          ; c6
          (* 0.1036 0.4972)

          ; overall remainder
          (- 0.4811
             ; c5
             (* 0.289 0.472)
             ; e5
             (* 0.1846 0.4969)
             ; e6
             (* 0.1405 0.4872)
             ; c6
             (* 0.1036 0.4702)))
         (sut/weighted-stat move-tree-1 :white)
         1e-4))
    (is (close-to
         (+
          ; c5, Nc3
          (+ (* 0.113 0.4169)
             (* 0.289
                ;; parent component
                (- 0.4433 (* 0.39 0.4416))))
          ; e5
          (* 0.1846 0.4118)

          ; e6
          (* 0.1405 0.414)

          ; c6
          (* 0.1036 0.4431)

          ; overall remainder
          (- 0.4662
             ; c5
             (* 0.289 0.474)
             ; e5
             (* 0.1846 0.4497)
             ; e6
             (* 0.1405 0.4591)
             ; c6
             (* 0.1036 0.4735)))
         (sut/weighted-stat move-tree-1 :black)
         1e-3))
    (is (close-to
         (/
          (+
           ; Nf3, d5
           (+ (* 0.13919642074334118 0.48984092)
              (* 0.3091032148021373
                 ; parent component
                 (- 0.4892777 (* 0.45032343 0.48350814))))
            ; overall remainder
           (* 0.4630254805088043
              (- 0.449697
                 ; Nf3, d5
                 (* 0.6675728 0.44761965))))
          0.4630254805088043)
         (sut/weighted-stat move-tree-2 :black)
         1e-4))
    (is (close-to
         (/
          (+
           ; Nf3, d5
           (+ (* 0.13919642074334118 0.46149877)
              (* 0.3091032148021373
                 ; parent component
                 (- 0.46160054 (* 0.45032343 0.46910626))))
            ; overall remainder
           (* 0.4630254805088043
              (- 0.49684057
                 ; Nf3, d5
                 (* 0.6675728 0.4958433))))
          0.4630254805088043)
         (sut/weighted-stat move-tree-2 :white)
         1e-4))))



