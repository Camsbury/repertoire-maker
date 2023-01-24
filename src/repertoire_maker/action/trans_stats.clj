(ns repertoire-maker.action.trans-stats
  (:require
   [malli.core :as m]
   [repertoire-maker.action.multi :refer [run-action]]
   [repertoire-maker.schema :as schema]
   [repertoire-maker.stat :refer [agg-stat]]
   [repertoire-maker.strategy :refer [apply-strategy]]
   [repertoire-maker.tree :as t]))

(defn transfer-stats
  "Bubble the best child stat up to the parent"
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
(m/=>
 transfer-stats
 [:=>
  [:cat
   [:and
    schema/build-tree-opts
    schema/config-opts]]
  [:and
   schema/build-tree-opts
   schema/config-opts]])

(defmethod run-action :trans-stats
  [opts]
  (transfer-stats opts))
