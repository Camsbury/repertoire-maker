(ns repertoire-maker.action.candidates
  (:require
   [repertoire-maker.candidate :refer [get-candidate prepare-masters-candidates]]
   [repertoire-maker.stat :refer [agg-stat]]
   [repertoire-maker.action.multi :refer [run-action]]
   [repertoire-maker.default :refer [defaults]]
   [repertoire-maker.engine :as ngn]
   [repertoire-maker.history :as h]
   [repertoire-maker.tree :as t]))

(defn- extract-filtered-moves
  [opts candidates]
  (let [best-score (->> candidates first :score)
        allowable-loss (or (:allowable-loss opts)
                           (get-in defaults [:engine :allowable-loss]))]
    (->> candidates
         (filter #(< allowable-loss (/ (:score %) best-score)))
         (mapv :uci))))

(defn- filter-engine
  [opts engine-candidates]
  (fn [move-candidates]
    (if-let
        [engine-candidates
         (->> engine-candidates
              (extract-filtered-moves opts)
              seq)]
      (filter
       #(contains? (set engine-candidates) (:uci %))
       move-candidates)
      move-candidates)))

(defn- prepare-player-move
  [{:keys [player] :as opts} engine-filter]
  (when player
    (some->> (assoc opts :group :player)
             h/moves->candidates
             engine-filter
             first
             :uci)))

(defn- init-agg-stats
  [node]
  (let [stats [:white :black :score :white-m :black-m]]
    (reduce
     (fn [node stat]
       (assoc node (agg-stat stat) (get node stat)))
     node
     stats)))

;; Enumerate all move candidates
(defmethod run-action :candidates
  [{:keys [min-plays
           min-cand-prob
           overrides
           search-depth
           stack
           step
           tree]
    :or   {min-plays       (get-in defaults [:algo :min-plays])
           min-cand-prob (get-in defaults [:algo :min-cand-prob])
           search-depth    (get-in defaults [:algo :search-depth])}
    :as   opts}]

  (let [{:keys [ucis depth]} step
        depth                (inc depth)
        {:keys [prob-agg]}   (t/get-in-tree tree ucis)
        opts                 (assoc opts :moves ucis)
        engine-candidates    (ngn/prepare-engine-candidates opts)
        engine-filter        (filter-engine opts engine-candidates)
        player-move          (prepare-player-move opts engine-filter)
        overridden-move      (get overrides ucis)
        lichess-candidates   (delay (-> opts
                                        (assoc :group :lichess)
                                        h/moves->candidates))
        masters-candidates   (prepare-masters-candidates opts)
        candidates           (or masters-candidates
                                 @lichess-candidates)
        candidates
        (cond
          (some? overridden-move)
          (->> candidates
               (filter #(= overridden-move (:uci %))))
          (some? player-move)
          (->> candidates
               engine-filter
               (filter #(= player-move (:uci %))))
          :else
          (->> candidates
               (filter #(< min-plays (:play-count %)))
               (filter #(< min-cand-prob (:prob %)))
               engine-filter))

        candidates
        (map
         #(merge
           %
           {:prob-agg prob-agg
            :ucis     (conj ucis (:uci %))
            :white    (->> %
                           (get-candidate @lichess-candidates)
                           :white)
            :black    (->> %
                           (get-candidate @lichess-candidates)
                           :black)
            :prob     (->> %
                           (get-candidate @lichess-candidates)
                           :prob)
            :white-m  (->> %
                           (get-candidate masters-candidates)
                           :white-m)
            :black-m  (->> %
                           (get-candidate masters-candidates)
                           :black-m)
            :prob-m   (->> %
                           (get-candidate masters-candidates)
                           :prob)
            :score    (->> %
                           (get-candidate engine-candidates)
                           :score)})
         candidates)

        tree (->> candidates
                  ;; init agg stats to the same as the nominal stats
                  (map init-agg-stats)
                  (reduce t/assoc-tree-branch tree))

        stack (if (< depth search-depth)
                (->> candidates
                     (reduce
                      (fn [s c]
                        (-> s
                            (conj {:action :calc-stats
                                   :ucis   (:ucis c)})
                            (conj {:action :responses
                                   :ucis   (:ucis c)
                                   :depth  depth})))
                      stack))
                stack)]

    (-> opts
        (assoc :tree tree)
        (assoc :stack stack))))
