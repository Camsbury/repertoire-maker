(ns repertoire-maker.action.prune
  (:require
   [taoensso.timbre :as log]
   [repertoire-maker.action.multi :refer [run-action]]
   [repertoire-maker.strategy :refer [apply-strategy]]
   [repertoire-maker.util.tree :as t]))

(defmethod run-action :prune
  [{:keys [step tree stack] :as opts}]
  (let [{:keys [ucis]} step

        node       (t/get-in-tree tree ucis)
        children   (:responses node)

        #_#_
        _ (when (empty? ucis)
            (log/info children))

        choice-uci (-> opts
                       (assoc :children children)
                       apply-strategy)

        tree (->> children
                  (remove #(= (first %) choice-uci))
                  (map #(:ucis (second %)))
                  (reduce t/dissoc-in-tree tree))

        ucis (conj ucis choice-uci)

        stack (-> stack
                  (conj {:action :calc-stats
                         :ucis   ucis})
                  (conj {:action :create-prune-hooks
                         :ucis   ucis})
                  (conj {:action :responses
                         :ucis   ucis
                         :depth  0}))]

    (log/info "Pruned tree to " ucis)

    (-> opts
        (assoc :tree tree)
        (assoc :stack stack))))
