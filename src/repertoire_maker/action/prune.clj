(ns repertoire-maker.action.prune
  (:require
   [malli.core :as m]
   [taoensso.timbre :as log]
   [repertoire-maker.action.multi :refer [run-action]]
   [repertoire-maker.strategy :refer [apply-strategy]]
   [repertoire-maker.schema :as schema]
   [repertoire-maker.tree :as t]))


(defn prune-tree
  [{:keys [step tree stack export-tree-path] :as opts}]
  (let [{:keys [ucis]} step

        node       (t/get-in-tree tree ucis)
        children   (:responses node)

        choice-uci (-> opts
                       (assoc :children children)
                       apply-strategy)]
    (if (some? choice-uci)
      (let [tree (->> children
                      (remove #(= (first %) choice-uci))
                      (map #(:ucis (second %)))
                      (reduce t/dissoc-in-tree tree))

            ucis (conj ucis choice-uci)

            stack
            (-> stack
                (conj {:action :calc-stats
                       :ucis   ucis})
                (conj {:action :create-prune-hooks
                       :ucis   ucis})
                (conj {:action :responses
                       :ucis   ucis
                       :cons-prob 1.0
                       :pruned? true
                       :depth   0}))]

        (when (some? export-tree-path)
          (spit export-tree-path tree))

        (log/info "Pruned tree to " ucis)

        (-> opts
            (assoc :tree tree)
            (assoc :stack stack)))
      opts)))
(m/=>
 prune-tree
 [:=>
  [:cat
   [:and
    schema/build-tree-opts
    schema/config-opts
    [:map [:step schema/build-step]]]]
  schema/build-tree-opts])

(defmethod run-action :prune
  [opts]
  (prune-tree opts))
