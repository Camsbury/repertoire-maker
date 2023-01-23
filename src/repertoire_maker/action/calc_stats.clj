(ns repertoire-maker.action.calc-stats
  (:require
   [repertoire-maker.stat :refer [agg-stat]]
   [repertoire-maker.action.multi :refer [run-action]]
   [repertoire-maker.tree :as t]))

(defn- prob-attr
  [stat]
  (if (#{:white-m :black-m} stat)
    :prob-m
    :prob))

(defn- children-total
  [stat children]
  (->> children
       (map second)
       (map #(* (or (get % stat) 0.0)
                (or ((prob-attr stat) %) 0.0)))
       (reduce +)))

(defn do-calc-stats
  [tree ucis]
  (let [children (t/resp-in-tree tree ucis)]
    (fn [node stat]
      ;; keep stats the same if no children
      (if (seq children)
        (let [nominal (get node stat)
              prob-non-child
              (->> children
                   (map #(or (get-in % [1 (prob-attr stat)]) 0.0))
                   (reduce +)
                   (- 1))
              children-nominal (when (not= stat :score)
                                 (children-total stat children))
              children-aggregate (children-total (agg-stat stat) children)

              calced
              (when (and (some? nominal)
                         (some? prob-non-child)
                         (some? children-nominal)
                         (some? children-aggregate))
                (+
                 (if (= stat :score)
                   (* prob-non-child nominal)
                   (- nominal children-nominal))
                 children-aggregate))]

          (assoc node (agg-stat stat) calced))
        node))))

;; weight parent stats based on children
(defmethod run-action :calc-stats
  [{:keys [step tree] :as opts}]
  (let [{:keys [ucis]} step

        node
        (reduce
         (do-calc-stats tree ucis)
         (t/get-in-tree tree ucis)
         [:white :black :white-m :black-m :score])]

    (update opts :tree t/assoc-tree-branch node)))
