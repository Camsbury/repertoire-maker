(ns repertoire-maker.strategy
  (:require
   [repertoire-maker.default :refer [defaults]]))

(defn filter-engine
  [engine-options move-options]
  (if (seq? engine-options)
    (filter #(contains? (set engine-options) (:uci %)) move-options)
    move-options))

;; NOTE - could do some kind of statistical significance thing where we are
;; comparing distributions based on sample size to see the probability that
;; one is better than the other.
(defn choose-move
  [{:keys [color
           engine
           lichess
           masters
           min-plays
           move-choice-pct
           moves
           overrides
           player]
    :or   {min-plays       (get-in defaults [:algo :min-plays])
           move-choice-pct (get-in defaults [:algo :move-choice-pct])}}]
  (let [player-move
        (->> player
             (filter-engine engine)
             first
             :uci)
        overridden-move (get overrides moves)
        chosen-move
        (cond
          (some? overridden-move)
          (->> lichess (filter #(= overridden-move (:uci %))) first)

          (some? player-move)
          (->> lichess (filter #(= player-move (:uci %))) first)

          :else
          (->> lichess
               (filter #(< min-plays (:play-count %)))
               (filter #(< move-choice-pct (:play-pct %)))
               (filter-engine engine)
               (sort-by (get {:black :white :white :black} color))
               first))]
    (assoc chosen-move :moves (conj moves (:uci chosen-move)))))
