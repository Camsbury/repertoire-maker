(ns repertoire-maker.default)

(def defaults
  {:algo
   {:filter-pct        0.01
    :min-plays         100
    :min-total-masters 500
    :move-choice-pct   0.01}
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
