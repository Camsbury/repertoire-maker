(ns repertoire-maker.action.trans-stats
  (:require
   [repertoire-maker.strategy :refer [apply-strategy]]
   [repertoire-maker.action.multi :refer [run-action]]
   [repertoire-maker.util.tree :as t]))

(defn- agg-stat
  [stat]
  (keyword (str (name stat) "-agg")))

;; bubble best child stat up to parent
(defmethod run-action :trans-stats
  [{:keys [step tree] :as opts}]
  (let [{:keys [ucis]} step
        node (t/get-in-tree tree ucis)
        children (:responses node)
        choice-uci (apply-strategy (assoc opts :children children))]
    (->> [:white :black :score :white-m :black-m]
         (map agg-stat)
         (select-keys (get children choice-uci))
         (merge node)
         (update opts :tree t/assoc-tree-branch))))
