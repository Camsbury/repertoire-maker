(ns repertoire-maker.action.init-responses
  (:require
   [malli.core :as m]
   [repertoire-maker.candidate :refer [get-candidate prepare-masters-candidates]]
   [repertoire-maker.action.multi :refer [run-action]]
   [repertoire-maker.default :refer [defaults]]
   [repertoire-maker.history :as h]
   [repertoire-maker.schema :as schema]
   [repertoire-maker.tree :as t]))

(defn init-responses
  "Kicks off the tree builder by grabbing initial opponent responses"
  [{:keys [min-resp-prob step tree stack]
    :or   {min-resp-prob (get-in defaults [:algo :min-resp-prob])}
    :as opts}]
  (let [{:keys [ucis]} step
        {:keys [prob-agg]} (t/get-in-tree tree ucis)

        opts (assoc opts :ucis ucis)

        masters-responses
        (prepare-masters-candidates opts)

        responses
        (->> (assoc opts :group :lichess)
             h/historic-moves
             (map (fn [move]
                    (merge move
                           {:ucis (conj ucis (:uci move))
                            :cons-prob (:prob move)
                            :prob-m (:prob (get-candidate masters-responses move))
                            :prob-agg (* prob-agg (:prob move))}))))

        tree (reduce t/assoc-tree-branch tree responses)

        stack (->> responses
                   reverse ; prioritize the most common responses
                   (filter
                    (fn [r] (> (:cons-prob r) min-resp-prob)))
                   (reduce
                    (fn [s r]
                      (-> s
                          (conj {:action :trans-stats
                                 :ucis   (:ucis r)})
                          (conj {:action :prune
                                 :ucis   (:ucis r)})
                          (conj {:action :candidates
                                 :ucis   (:ucis r)
                                 :cons-prob (:cons-prob r)
                                 :depth     0})))
                    stack))]

    (-> opts
        (assoc :tree tree)
        (assoc :stack stack))))
(m/=>
 enumerate-candidates
 [:=>
  [:cat
   [:and
    schema/build-tree-opts
    schema/config-opts
    [:map [:step schema/build-step]]]]
  schema/build-tree-opts])

(defmethod run-action :init-responses
  [opts]
  (init-responses opts))
