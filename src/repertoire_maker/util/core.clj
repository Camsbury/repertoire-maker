(ns repertoire-maker.util.core)

(defn whose-turn? [moves]
  (if (= 0 (mod (count moves) 2))
    :white
    :black))

