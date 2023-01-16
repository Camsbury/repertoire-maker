(ns repertoire-maker.export-test
  (:require
   [flatland.ordered.map  :refer [ordered-map]]
   [repertoire-maker.export :as sut]
   [repertoire-maker.test :refer [close-to]]
   [clojure.test :as t :refer [is deftest]]))


(deftest weighted-stat-test
  (let [move-tree-1
        (ordered-map
         "e4"
         {:responses
          (ordered-map
           "e5"
           {:white 0.75 :black 0.2 :pct 0.24})})
        move-tree-2
        #ordered/map
        (["e2e4"
          {:uci "e2e4",
           :pct 1.0,
           :responses
           #ordered/map
           (["c7c5"
             {:uci "c7c5",
              :white 0.47198713,
              :black 0.4740327,
              :play-count 3960233,
              :play-pct 0.28969485,
              :pct 0.2896948456764221,
              :responses
              #ordered/map
              (["b1c3"
                {:uci "b1c3",
                 :white 0.5065455,
                 :black 0.4432782,
                 :play-count 570010,
                 :play-pct 0.14412516,
                 :pct 0.2896948456764221,
                 :score nil,
                 :responses
                 #ordered/map
                 (["b8c6"
                   {:uci "b8c6",
                    :white 0.5073518,
                    :black 0.44155878,
                    :play-count 227562,
                    :play-pct 0.3900751,
                    :pct 0.11300274228923257,
                    :responses
                    #ordered/map
                    (["f1b5"
                      {:uci "f1b5",
                       :white 0.52691954,
                       :black 0.416721,
                       :play-count 23013,
                       :play-pct 0.10115027,
                       :pct 0.11300274228923257,
                       :score nil}])}])}])}]
            ["e7e5"
             {:uci "e7e5",
              :white 0.49687266,
              :black 0.4496986,
              :play-count 2524184,
              :play-pct 0.18464647,
              :pct 0.18464647233486176,
              :responses
              #ordered/map
              ([b1c3
                {:uci b1c3,
                 :white 0.5397585,
                 :black 0.4115988,
                 :play-count 226920,
                 :play-pct 0.09004155,
                 :pct 0.18464647233486176,
                 :score nil}])}]
            ["e7e6"
             {:uci "e7e6",
              :white 0.487213,
              :black 0.4590678,
              :play-count 1920542,
              :play-pct 0.14048949,
              :pct 0.14048948884010315,
              :responses
              #ordered/map
              ([d1e2
                {:uci d1e2,
                 :white 0.53428,
                 :black 0.41412076,
                 :play-count 27888,
                 :play-pct 0.014541314,
                 :pct 0.14048948884010315,
                 :score nil}])}]
            ["c7c6"
             {:uci "c7c6",
              :white 0.4702794,
              :black 0.47340757,
              :play-count 1415516,
              :play-pct 0.10354635,
              :pct 0.10354635119438171,
              :responses
              #ordered/map
              ([c2c4
                {:uci c2c4,
                 :white 0.49741235,
                 :black 0.44290483,
                 :play-count 47920,
                 :play-pct 0.033895597,
                 :pct 0.10354635119438171,
                 :score nil}])}])}])]
    (is (= 0.75 (sut/weighted-stat move-tree-1 :white)))
    (is (= 0.2 (sut/weighted-stat move-tree-1 :black)))
    (is (close-to
         (;f1b5
          (+ (* 0.113 0.5269)
             (* 0.289
                ;; parent component
                (- 0.5065 (* 0.39 00.5073)))))
         (sut/weighted-stat move-tree-2 :white)))))



