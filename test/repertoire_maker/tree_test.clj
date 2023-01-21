(ns repertoire-maker.tree-test
  (:require
   [taoensso.timbre :as log]
   [repertoire-maker.tree :as sut]
   [repertoire-maker.util.test :refer [close-to]]
   [flatland.ordered.map :refer [ordered-map]]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is]]))

(deftest starting-state-test
  (with-redefs-fn
    {#'repertoire-maker.tree/init-move-eval
     (fn [{:keys [color ucis node]}]
       (case (conj ucis node)
         [] {:white-m  0.33
             :black-m  0.24
             :white    0.49
             :black    0.45
             :score    (if (= :white color)
                         0.531
                         0.484)
             :prob     1.0
             :prob-agg 1.0
             :ucis     []}

         ["e2e4"]
         {:white-m  0.319
          :black-m  0.261
          :white    0.481
          :black    0.465
          :score    (if (= :white color)
                      0.531
                      0.487)
          :prob     1.0
          :prob-agg 1.0
          :ucis     ["e2e4"]}

         ["e2e4" "c7c5"]
         {:white-m  0.319
          :black-m  0.261
          :white    0.481
          :black    0.465
          :score    (if (= :white color)
                      0.527
                      0.487)
          :prob     1.0
          :prob-agg 1.0
          :ucis     ["e2e4" "c7c5"]}))}

    #(let [opts
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
               (assoc (sut/base-node :white) :responses (ordered-map))}
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
               (assoc (sut/base-node :black) :responses (ordered-map))}
             (-> opts
                 :ss-opts-black-empty
                 sut/starting-state)))

      ;; NOTE: stats should be wrong for these base nodes, but should be set by
      ;; fixtures that I create for initialization

       (let [stack
             (list
              {:action :candidates
               :ucis   ["e2e4" "c7c5"]
               :depth  0}
              {:action :prune
               :ucis   ["e2e4" "c7c5"]}
              {:action :trans-stats
               :ucis   ["e2e4" "c7c5"]})
             node
             {:white-m   0.319
              :black-m   0.261
              :white     0.481
              :black     0.465
              :score     0.527
              :prob      1.0
              :prob-agg  1.0
              :responses (ordered-map)
              :ucis      ["e2e4" "c7c5"]}
             res (-> opts
                     :ss-opts-white-white
                     sut/starting-state)]

         (is (= stack (:stack res)))
         (is (= node (sut/get-in-tree (:tree res) ["e2e4" "c7c5"]))))

       (let [stack
             (list
              {:action :init-responses
               :ucis   ["e2e4" "c7c5"]}
              {:action :calc-stats
               :ucis   ["e2e4" "c7c5"]})
             node
             {:white-m   0.319
              :black-m   0.261
              :white     0.481
              :black     0.465
              :score     0.487
              :prob      1.0
              :prob-agg  1.0
              :responses (ordered-map)
              :ucis      ["e2e4" "c7c5"]}
             res (-> opts
                     :ss-opts-black-white
                     sut/starting-state)]

         (is (= stack (:stack res)))
         (is (= node (sut/get-in-tree (:tree res) ["e2e4" "c7c5"]))))

       (let [stack
             (list
              {:action :init-responses
               :ucis   ["e2e4"]}
              {:action :calc-stats
               :ucis   ["e2e4"]})
             node
             {:white-m   0.319
              :black-m   0.261
              :white     0.481
              :black     0.465
              :score     0.531
              :prob      1.0
              :prob-agg  1.0
              :responses (ordered-map)
              :ucis      ["e2e4"]}
             res (-> opts
                     :ss-opts-white-black
                     sut/starting-state)]

         (is (= stack (:stack res)))
         (is (= node (sut/get-in-tree (:tree res) ["e2e4"]))))

       (let [stack
             (list
              {:action :candidates
               :ucis   ["e2e4"]
               :depth  0}
              {:action :prune
               :ucis   ["e2e4"]}
              {:action :trans-stats
               :ucis   ["e2e4"]})
             node
             {:white-m   0.319
              :black-m   0.261
              :white     0.481
              :black     0.465
              :score     0.487
              :prob      1.0
              :prob-agg  1.0
              :responses (ordered-map)
              :ucis      ["e2e4"]}
             res (-> opts
                     :ss-opts-black-black
                     sut/starting-state)]

         (is (= stack (:stack res)))
         (is (= node (sut/get-in-tree (:tree res) ["e2e4"])))))))

#_#_#_#_#_#_#_#_#_

(deftest enumerate-candidates-test
  (is false))

(deftest enumerate-responses-test
  (is false))

(deftest prune-tree-test
  (is false))

(deftest initialize-responses-test
  (is false))

(deftest do-trans-stats-test
  (is false))

(deftest transfer-stats-test
  (is false))

(deftest do-calc-stats-test
  (is false))

(deftest calc-stats-test
  (is false))

(deftest build-tree-test
  (is false))
