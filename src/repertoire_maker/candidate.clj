(ns repertoire-maker.candidate
  (:require
   [clojure.set :as set]
   [repertoire-maker.default :refer [defaults]]
   [repertoire-maker.history :as h]))

(defn prepare-masters-candidates
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

(defn get-candidate
  [candidates move]
  (->> candidates
       (filter #(= (:uci move) (:uci %)))
       first))
