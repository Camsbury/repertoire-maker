(ns repertoire-maker.candidate
  (:require
   [malli.core :as m]
   [clojure.set :as set]
   [repertoire-maker.default :refer [defaults]]
   [repertoire-maker.history :as h]
   [repertoire-maker.schema :as schema]))

(defn prepare-masters-candidates
  [{:keys [masters? min-total-masters]
    :or   {min-total-masters (get-in defaults [:algo :min-total-masters])}
    :as opts}]
  (when masters?
    (let [candidates
          (h/historic-moves
           (assoc opts :group :masters))

          total-plays (->> candidates
                           (map :play-count)
                           (reduce +))]
      (when (> total-plays min-total-masters)
        (map
         #(set/rename-keys % {:white :white-m :black :black-m})
         candidates)))))
(m/=>
 prepare-masters-candidates
 [:=>
  [:cat
   [:and
    schema/build-tree-opts
    schema/config-opts]]
  [:sequential schema/move-node]])

(defn get-candidate
  [candidates move]
  (->> candidates
       (filter #(= (:uci move) (:uci %)))
       first))
(m/=>
 get-candidate
 [:=>
  [:cat
   [:sequential schema/move-node] schema/uci]
  schema/move-node])
