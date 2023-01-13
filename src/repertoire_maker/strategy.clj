(ns repertoire-maker.strategy
  (:require
   [repertoire-maker.engine :as ngn]
   [repertoire-maker.util :as util]))

(defn filter-engine
  [engine-options move-options]
  (if (seq? engine-options)
    (filter #(contains? (set engine-options) (:uci %)) move-options)
    move-options))

;; NOTE - could do some kind of statistical significance thing where we are
;; comparing distributions based on sample size to see the probability that
;; one is better than the other.
(defn select-option
  [{:keys [moves move-choice-pct masters lichess player engine color overrides]}]
  (let [min-plays 100
        player-move
        (->> player
             (filter-engine engine)
             first
             :uci)
        lc-move
        (->> lichess
             (filter #(< min-plays (:play-count %)))
             (filter #(< move-choice-pct (:play-pct %)))
             (filter-engine engine)
             (sort-by (get {:black :white :white :black} color))
             first
             :uci)]
    (some->>
     (or
      (get overrides moves)
      player-move
      lc-move
      (first engine))
     (conj moves))))
