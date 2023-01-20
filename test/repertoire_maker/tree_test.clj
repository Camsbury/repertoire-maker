(ns repertoire-maker.tree-test
  (:require [repertoire-maker.tree :as sut]
            [repertoire-maker.util.test :refer [close-to load-test-data]]
            [flatland.ordered.map :refer [ordered-map]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(deftest starting-state-test
  (let [opts
        (-> "test/starting-state-opts.edn"
            str
            io/resource
            slurp
            edn/read-string)]

    (is (= {:stack
            (list
             {:action :candidates
              :ucis   []
              :depth  0}
             {:action :prune
              :ucis   []}
             {:action :trans-stats
              :ucis   []})
            :tree
            (assoc sut/base-node :responses (ordered-map))}
           (-> opts
               :ss-opts-white-empty
               sut/starting-state)))

    (is (= {:stack
            (list
             {:action :init-responses
              :ucis   []}
             {:action :calc-stats
              :ucis   []})
            :tree
            (assoc sut/base-node :responses (ordered-map))}
           (-> opts
               :ss-opts-black-empty
               sut/starting-state)))

    ;; NOTE: stats should be wrong for these base nodes, but should be set by
    ;; fixtures that I create for initialization

    (is (= {:stack (list
                    {:action :candidates
                     :ucis   ["e4" "c5"]
                     :depth  0}
                    {:action :prune
                     :ucis   ["e4" "c5"]}
                    {:action :trans-stats
                     :ucis   ["e4" "c5"]})
            :tree  (assoc
                    sut/base-node
                    :responses
                    (ordered-map
                     "e4"
                     {:ucis      ["e4"]
                      :responses (ordered-map
                                  "c5"
                                  {:white-m  0.3195719
                                   :black-m  0.2608705
                                   :white    0.48083812
                                   :black    0.4650707
                                   :score    0.53
                                   :prob     1.0
                                   :prob-agg 1.0
                                   :ucis     ["e4" "c5"]})}))}
           (-> opts
               :ss-opts-white-white
               sut/starting-state)))

    (is (= {:stack (list
                    {:action :init-responses
                     :ucis   ["e4" "c5"]}
                    {:action :calc-stats
                     :ucis   ["e4" "c5"]})
            :tree  (assoc
                    sut/base-node
                    :responses
                    (ordered-map
                     "e4"
                     {:ucis      ["e4"]
                      :responses (ordered-map
                                  "c5"
                                  {:white-m  0.0
                                   :black-m  0.0
                                   :white    0.0
                                   :black    0.0
                                   :score    0.53
                                   :prob     1.0
                                   :prob-agg 1.0
                                   :ucis     ["e4" "c5"]})}))}
           (-> opts
               :ss-opts-black-white
               sut/starting-state)))

    (is (= {:stack (list
                    {:action :init-responses
                     :ucis   ["e4"]}
                    {:action :calc-stats
                     :ucis   ["e4"]})
            :tree  (assoc
                    sut/base-node
                    :responses
                    (ordered-map
                     "e4"
                     {:white-m  0.0
                      :black-m  0.0
                      :white    0.0
                      :black    0.0
                      :score    0.53
                      :prob     1.0
                      :prob-agg 1.0
                      :ucis      ["e4"]}))}
           (-> opts
               :ss-opts-white-black
               sut/starting-state)))

    (is (= {:stack (list
                    {:action :candidates
                     :ucis   ["e4"]
                     :depth  0}
                    {:action :prune
                     :ucis   ["e4"]}
                    {:action :trans-stats
                     :ucis   ["e4"]})
            :tree  (assoc
                    sut/base-node
                    :responses
                    (ordered-map
                     "e4"
                     {:white-m  0.0
                      :black-m  0.0
                      :white    0.0
                      :black    0.0
                      :score    0.53
                      :prob     1.0
                      :prob-agg 1.0
                      :ucis      ["e4"]}))}
           (-> opts
               :ss-opts-black-black
               sut/starting-state)))

    ))

#_#_#_#_#_
(deftest do-trans-stats-test
  (is false))

(deftest transfer-stats-test
  (is false))

(deftest do-calc-stats-test
  (is false))

(deftest calculate-stats-test
  (is false))

(deftest build-tree-test
  (is false))
