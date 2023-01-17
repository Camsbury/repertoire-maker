(ns repertoire-maker.score)

;; Note: helpful links
;; https://lichess.org/page/accuracy
;; https://github.com/lichess-org/lila/blob/master/modules/analyse/src/main/WinPercent.scala#L23

(def multiplier
  -0.00368208)

(defn standardize-score
  [{:keys [cp mate-count]}]
  (if (some? cp)
    (-> (/ 2 (+ 1 (Math/exp (* multiplier cp))))
        (- 1)
        (* 0.5)
        (+ 0.5))
    (->> mate-count (/ 1) (+ 1))))
