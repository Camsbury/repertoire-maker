(ns repertoire-maker.strategy
  (:require
   [repertoire-maker.engine :as ngn]
   [repertoire-maker.util :as util]))

;; NOTE - could do some kind of statistical significance thing where we are
;; comparing distributions based on sample size to see the probability that
;; one is better than the other.
(defn select-option
  [{:keys [moves move-choice-pct masters lichess engine color overrides]}]
  (let [min-plays 100
        next-move
        (->> lichess
             (filter #(< min-plays (:play-count %)))
             (filter #(< move-choice-pct (:play-pct %)))
             (filter #(contains? (set engine) (:uci %)))
             (sort-by (get {:black :white :white :black} color))
             first
             :uci)]
    (conj moves (or (get overrides moves) next-move (first engine)))))
