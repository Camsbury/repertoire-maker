(ns repertoire-maker.action.init-responses
  (:require
   [repertoire-maker.candidate :refer [get-candidate prepare-masters-candidates]]
   [repertoire-maker.action.multi :refer [run-action]]
   [repertoire-maker.default :refer [defaults]]
   [repertoire-maker.history :as h]
   [repertoire-maker.tree :as t]))

(defmethod run-action :init-responses
  [{:keys [min-resp-prob step tree stack]
    :or   {min-resp-prob (get-in defaults [:algo :min-resp-prob])}
    :as opts}]
  (let [{:keys [ucis]} step
        {:keys [prob-agg]} (t/get-in-tree tree ucis)

        masters-responses
        (prepare-masters-candidates opts)

        responses
        (->> (-> opts
                 (assoc :group :lichess)
                 (assoc :moves ucis))
             h/moves->candidates
             (filter #(< min-resp-prob (:prob %)))
             (map (fn [move]
                    (merge move
                           {:ucis (conj ucis (:uci move))
                            :prob-m (:prob (get-candidate masters-responses move))
                            :prob-agg (* prob-agg (:prob move))}))))

        tree (reduce t/assoc-tree-branch tree responses)

        stack (->> responses
                   reverse ; prioritize the most common responses
                   (reduce
                    (fn [s r]
                      (-> s
                          (conj {:action :trans-stats
                                 :ucis   (:ucis r)})
                          (conj {:action :prune
                                 :ucis   (:ucis r)})
                          (conj {:action :candidates
                                 :ucis   (:ucis r)
                                 :depth  0})))
                    stack))]

    (-> opts
        (assoc :tree tree)
        (assoc :stack stack))))
