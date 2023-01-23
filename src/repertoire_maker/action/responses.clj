(ns repertoire-maker.action.responses
  (:require
   [clojure.set :as set]
   [repertoire-maker.action.multi :refer [run-action]]
   [repertoire-maker.default :refer [defaults]]
   [repertoire-maker.history :as h]
   [repertoire-maker.util.tree :as t]))

(defn- prepare-masters-candidates
  [{:keys [masters? min-total-masters]
    :or   {min-total-masters (get-in defaults [:algo :min-total-masters])}
    :as opts}]
  (when masters?
    (let [candidates
          (h/moves->candidates
           (assoc opts :group :masters))

          total-plays (->> candidates
                           (map :play-count)
                           (reduce +))]
      (when (> total-plays min-total-masters)
        (map
         #(set/rename-keys % {:white :white-m :black :black-m})
         candidates)))))

(defn- get-candidate
  [candidates move]
  (->> candidates
       (filter #(= (:uci move) (:uci %)))
       first))

;; Enumerate responses to a move
(defmethod run-action :responses
  [{:keys [min-resp-prob min-prob-agg step tree stack]
    :or   {min-resp-prob (get-in defaults [:algo :min-resp-prob])
           min-prob-agg (get-in defaults [:algo :min-prob-agg])}
    :as opts}]
  (let [{:keys [ucis depth]} step
        {:keys [prob-agg]} (t/get-in-tree tree ucis)

        masters-responses
        (prepare-masters-candidates opts)

        responses
        (->> (-> opts
                 (assoc :group :lichess)
                 (assoc :moves ucis))
             h/moves->candidates
             (filter #(< min-resp-prob (:prob %)))
             (filter #(or
                       (not (zero? depth))
                       (< min-prob-agg (* prob-agg (:prob %)))))
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
                          (conj {:action :candidates
                                 :ucis   (:ucis r)
                                 :depth  depth})))
                    stack))]
    (-> opts
        (assoc :tree tree)
        (assoc :stack stack))))
