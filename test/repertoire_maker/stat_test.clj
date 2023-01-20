(ns repertoire-maker.stat-test
  (:require
   [flatland.ordered.map]
   [repertoire-maker.stat :as sut]
   [repertoire-maker.util.test :refer [close-to load-test-data]]
   [clojure.test :as t :refer [is deftest]]))

(deftest weighted-stat-test
  (let [{:keys
         [move-tree-1
          move-tree-2
          move-tree-score
          move-tree-score-black
          large-spanish-tree]}
        (load-test-data
         "move-tree-1"
         "move-tree-2"
         "move-tree-score"
         "move-tree-score-black"
         "large-spanish-tree")]
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
         1e-4))

    (is (close-to
         0.5057526961439385
         (sut/weighted-stat move-tree-score :score)
         1e-4))

    (is (close-to
         (/
          (+
           ; Nf3, d5
           (+ (* 0.13919642074334118 0.3225384269344715)
              (* 0.3091032148021373
                 ; parent component
                 (* 0.3175308490476826 (- 1 0.4502498))))
            ; overall remainder
           (* 0.46298056840896606
              (* 0.4441428603339783 (- 1 0.6676149))))
          0.46298056840896606)
         (sut/weighted-stat move-tree-score-black :score)
         1e-4))

    (is (close-to
         0.512729463
         (sut/weighted-stat large-spanish-tree :white)))))
