(ns repertoire-maker.strategy
  (:require
   [repertoire-maker.util :as util]))

(defn select-option
  [{:keys [moves master lichess engine color]}]
  (some->> (->> lichess
                (filter #(< 0.01 (:play-pct %)))
                (sort-by (get {:black :white :white :black} color))
                first
                :uci)
           (conj moves)))
