(ns repertoire-maker.action.calc-stats
  (:require
   [malli.core :as m]
   [malli.clj-kondo :as mc]
   [repertoire-maker.action.multi :refer [run-action]]
   [repertoire-maker.schema :as schema]
   [repertoire-maker.stat :refer [agg-stat]]
   [repertoire-maker.tree :as t]))

(defn- prob-attr
  [stat]
  (if (#{:white-m :black-m} stat)
    :prob-m
    :prob))
(m/=> prob-attr [:=> [:cat schema/non-agg-stat] schema/prob])

(defn- children-total
  [stat children]
  (->> children
       (map second)
       (map #(* (or (get % stat) 0.0)
                (or ((prob-attr stat) %) 0.0)))
       (reduce +)))
(m/=> children-total [:=> [:cat schema/stat schema/responses] :double])

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
(m/=>
 do-calc-stats
 [:=>
  [:cat schema/move-tree [:sequential schema/uci]]
  [:=> [:cat schema/move-tree schema/stat] schema/move-tree]])

(defn calc-stats
  "Weight parent stats based on children"
  [{:keys [step tree] :as opts}]
  (let [{:keys [ucis]} step

        node
        (reduce
         (do-calc-stats tree ucis)
         (t/get-in-tree tree ucis)
         [:white :black :white-m :black-m :score])]
    (update opts :tree t/assoc-tree-branch node)))
(m/=>
 calc-stats
 [:=>
  [:cat
   schema/build-tree-opts]
  schema/build-tree-opts])

(defmethod run-action :calc-stats
  [opts]
  (calc-stats opts))

;; NOTE: needed?
(-> (mc/collect *ns*) (mc/linter-config))
