(ns repertoire-maker.build
  (:require
   [taoensso.timbre :as log]
   [repertoire-maker.default :refer [defaults]]
   [repertoire-maker.history :as h]
   [repertoire-maker.strategy :as strategy]
   [repertoire-maker.util.tree :as tree]))

(defn expand-moves
  [{:keys [filter-pct moves parent-pct]
    :or   {filter-pct (get-in defaults [:algo :filter-pct])}
    :as opts}]
  (->> (assoc opts :group :lichess)
       h/moves->candidates
       (filter #(< filter-pct (* parent-pct (:play-pct %))))
       (map (fn [move]
              (merge move
                     {:moves (conj moves (:uci move))
                      :pct   (* parent-pct (:play-pct move))})))))

(defn expand-movesets
  [{:keys [filter-pct local? movesets]
    :as   opts}]
  (reduce
   (fn [acc {:keys [moves pct]}]
     (if-let [moveset
              (seq
               (expand-moves
                {:moves      moves
                 :parent-pct pct
                 :filter-pct filter-pct
                 :local?     local?}))]
       (-> acc
           (update :tree #(reduce tree/add-tree-branch % moveset))
           (update :movesets into moveset))
       acc))
   (assoc opts :movesets [])
   movesets))

(defn traverse-chosen-move
  [{:keys [movesets]
    :as   opts}]
  (reduce
   (fn [acc moveset]
     (let [new-move
           (strategy/choose-move
            (merge opts moveset))]
       (if (:uci new-move)
         (-> acc
             (update :tree tree/add-tree-branch new-move)
             (update :movesets conj (assoc moveset :moves (:moves new-move))))
         acc)))
   (assoc opts :movesets [])
   movesets))
