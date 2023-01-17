(ns repertoire-maker.score)

;; NOTE: want to experiment with wdl (avail in python-chess)
;; stockfish wdl is calculated by the following:
;; https://github.com/official-stockfish/Stockfish/blob/master/src/uci.cpp#L201

;; NOTE: helpful links
;; https://lichess.org/page/accuracy
;; https://github.com/lichess-org/lila/blob/master/modules/analyse/src/main/WinPercent.scala#L23

(def multiplier
  -0.00368208)

(defn standardize-score
  [{:keys [cp mate]}]
  (if (some? cp)
    (-> (/ 2 (+ 1 (Math/exp (* multiplier cp))))
        (- 1)
        (* 0.5)
        (+ 0.5))
    (->> mate (/ 1) (+ 1))))
