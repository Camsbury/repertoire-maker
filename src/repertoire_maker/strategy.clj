(ns repertoire-maker.strategy
  (:require
   [repertoire-maker.engine :as ngn]
   [repertoire-maker.util :as util]))

;; NOTE - could do some kind of statistical significance thing where we are
;; comparing distributions based on sample size to see the probability that
;; one is better than the other.
(defn select-option
  [{:keys [moves move-choice-pct masters lichess player engine color overrides]}]
  (let [min-plays 100
        player-move
        (->> player
             (filter #(contains? (set engine) (:uci %)))
             first
             :uci)
        lc-move
        (->> lichess
             (filter #(< min-plays (:play-count %)))
             (filter #(< move-choice-pct (:play-pct %)))
             (filter #(contains? (set engine) (:uci %)))
             (sort-by (get {:black :white :white :black} color))
             first
             :uci)]
    (conj moves (or (get overrides moves) player-move lc-move (first engine)))))
