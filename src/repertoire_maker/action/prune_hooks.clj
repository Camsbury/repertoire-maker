(ns repertoire-maker.action.prune-hooks
  (:require
   [taoensso.timbre :as log]
   [repertoire-maker.action.multi :refer [run-action]]
   [repertoire-maker.default :refer [defaults]]
   [repertoire-maker.tree :as t]))

(def alternate-stats
  {:calc-stats  :trans-stats
   :trans-stats :calc-stats})

(defn- do-prune-hooks
  [action]
  (fn [[_ {:keys [ucis responses]}]]
    ;; alternate stat pushes to stack
    (into
     [{:action action
       :ucis   ucis}]
     (mapcat (do-prune-hooks (alternate-stats action)) responses))))

(defn prune-hooks
  [[_ {:keys [ucis responses]}]]
  (into
   [{:action :prune
     :ucis   ucis}
    {:action :trans-stats
     :ucis   ucis}]
   (mapcat (do-prune-hooks :calc-stats) responses)))

(defmethod run-action :create-prune-hooks
  [{:keys [min-prob-agg step tree stack]
    :or   {min-prob-agg (get-in defaults [:algo :min-prob-agg])}
    :as   opts}]
  (let   [{:keys [ucis]} step

          #_#_
          _ (log/info "creating prune hooks for " ucis)
          viable-responses
          (->> ucis
               (t/get-in-tree tree)
               :responses
               (filter #(< min-prob-agg (:prob-agg (second %))))
               ;; push the most common last
               reverse)

          nonviable-responses
          (->> ucis
               (t/get-in-tree tree)
               :responses
               (remove #(< min-prob-agg (:prob-agg (second %)))))

          tree (reduce
                (fn [t [_ n]] (t/assoc-tree-branch t (assoc n :trim? true)))
                tree
                nonviable-responses)]
    (merge
     opts
     {:tree  tree
      :stack (into stack (mapcat prune-hooks viable-responses))})))
