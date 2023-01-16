(ns repertoire-maker.strategy
  (:require
   [repertoire-maker.default :refer [defaults]]))

(defn extract-filtered-moves
  [allowable-loss options]
  (let [best-score (->> options first :score)]
    (->> options
         (filter #(> allowable-loss (/ (:score %) best-score)))
         (mapv :uci))))

(defn filter-engine
  [allowable-loss engine-options move-options]
  (if (->> engine-options
           (extract-filtered-moves allowable-loss)
           seq?)
    (filter #(contains? (set engine-options) (:uci %)) move-options)
    move-options))

;; NOTE - could do some kind of statistical significance thing where we are
;; comparing distributions based on sample size to see the probability that
;; one is better than the other.
(defn choose-move
  [{:keys [allowable-loss
           color
           engine
           lichess
           masters
           min-plays
           move-choice-pct
           moves
           overrides
           pct
           player]
    :or   {min-plays       (get-in defaults [:algo :min-plays])
           move-choice-pct (get-in defaults [:algo :move-choice-pct])}}]
  (let [player-move
        (->> player
             (filter-engine allowable-loss engine)
             first
             :uci)

        overridden-move (get overrides moves)

        chosen-move
        (cond
          (some? overridden-move)
          (->> lichess
               (filter #(= overridden-move (:uci %)))
               first)

          (some? player-move)
          (->> lichess
               (filter-engine allowable-loss engine)
               (filter #(= player-move (:uci %)))
               first)

          :else
          (->> lichess
               (filter #(< min-plays (:play-count %)))
               (filter #(< move-choice-pct (:play-pct %)))
               (filter-engine allowable-loss engine)
               (sort-by (get {:black :white :white :black} color))
               first))]
    (-> (or chosen-move (first engine))
        (assoc :chosen? true)
        (assoc :pct pct)
        (assoc :moves (conj moves (:uci chosen-move)))
        (assoc :score (->> engine
                           (filter
                            #(= (:uci chosen-move)
                                (:uci %)))
                           first
                           :score)))))
