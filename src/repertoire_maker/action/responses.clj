(ns repertoire-maker.action.responses
  (:require
   [malli.core :as m]
   [repertoire-maker.action.multi :refer [run-action]]
   [repertoire-maker.candidate :refer [prepare-masters-candidates get-candidate]]
   [repertoire-maker.default :refer [defaults]]
   [repertoire-maker.history :as h]
   [repertoire-maker.schema :as schema]
   [repertoire-maker.tree :as t]))

(defn enumerate-responses
  [{:keys [min-plays
           min-prob-agg
           min-resp-pct
           min-resp-prob
           step
           tree
           stack]
    :or   {min-plays     (get-in defaults [:algo :min-plays])
           min-prob-agg  (get-in defaults [:algo :min-prob-agg])
           min-resp-pct  (get-in defaults [:algo :min-resp-pct])
           min-resp-prob (get-in defaults [:algo :min-resp-prob])}
    :as   opts}]
  (let [{:keys [ucis cons-prob pruned?]} step
        {:keys [prob-agg]}               (t/get-in-tree tree ucis)

        opts (assoc opts :ucis ucis)

        masters-responses
        (prepare-masters-candidates opts)

        responses
        (->> (assoc opts :group :lichess)
             h/historic-moves
             (filter #(or
                       (not pruned?)
                       (< min-prob-agg (* prob-agg (:prob %)))))

             (filter #(< min-plays (:play-count %)))
             (map (fn [move]
                    (merge move
                           {:ucis      (conj ucis (:uci move))
                            :cons-prob (* cons-prob (:prob move))
                            :prob-m    (:prob (get-candidate masters-responses move))
                            :prob-agg  (* prob-agg (:prob move))}))))


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
                          (conj {:action    :candidates
                                 :ucis      (:ucis r)
                                 :cons-prob (:cons-prob r)
                                 :pruned?   pruned?})))
                    stack))]
    (-> opts
        (assoc :tree tree)
        (assoc :stack stack))))
(m/=>
 enumerate-responses
 [:=>
  [:cat
   [:and
    schema/build-tree-opts
    schema/config-opts
    [:map [:step schema/build-step]]]]
  schema/build-tree-opts])

;; Enumerate responses to a move
(defmethod run-action :responses
  [opts]
  (enumerate-responses opts))
