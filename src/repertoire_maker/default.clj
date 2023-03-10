(ns repertoire-maker.default)

(def defaults
  {:algo
   {:min-prob-agg      0.01
    :min-resp-prob     0.05
    :min-cand-prob     0.01
    :min-plays         100
    :min-total-masters 500
    :max-cand-breadth  5
    :search-depth      5
    :strategy          :max-win-over-loss}
   :engine
   {:allowable-loss 0.9
    :move-count     10
    :depth          20
    :hash           2048
    :threads        7}
   :history
   {:ratings      [2000 2200 2500]
    :speeds       ["bullet" "blitz" "rapid"]
    :moves        30
    :top-games    0
    :recent-games 0
    :since        "1952-01"}})
