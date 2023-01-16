(ns repertoire-maker.export-test
  (:require
   [flatland.ordered.map  :refer [ordered-map]]
   [repertoire-maker.export :as sut]
   [clojure.test :as t :refer [is deftest]]))

(deftest weighted-stat-test
  (let [move-tree-1 (ordered-map "e4" {:responses (ordered-map "e5" {:white 0.75 :black 0.2 :pct 0.24})})]
    (is (= 0.75 (sut/weighted-stat move-tree-1 :white)))
    (is (= 0.2 (sut/weighted-stat move-tree-1 :black)))))

