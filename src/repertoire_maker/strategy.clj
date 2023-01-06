(ns repertoire-maker.strategy
  (:require
   [repertoire-maker.util :as util]))

(defn select-option
  [{:keys [moves master lichess engine color]}]
  (->> lichess
       first
       :uci
       (conj moves)))
