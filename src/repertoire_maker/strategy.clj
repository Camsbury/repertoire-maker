(ns repertoire-maker.strategy
  (:require
   [repertoire-maker.util :as util]))

(defn select-option
  [{:keys [moves masters lichess engine color]}]
  (let [min-pct   0.01 ;; NOTE - could do some kind of statistical significance thing where we are comparing distributions based on sample size to see the probability that one is better than the other.
        min-plays 100]
    (some->> (->> lichess
                  (filter #(< min-plays (:play-count %)))
                  (filter #(< min-pct (:play-pct %)))
                  (sort-by (get {:black :white :white :black} color))
                  first
                  :uci)
             (conj moves))))
