(ns repertoire-maker.util.core
  (:require [clojure.string :as str]))

(defn whose-turn? [moves]
  (if (= 0 (mod (count moves) 2))
    :white
    :black))

(defn fen->turn [fen]
  (-> fen
      (str/split #" ")
      (nth 1)
      {"w" :white "b" :black}))

(defn my-turn? [color moves]
  (= color (whose-turn? moves)))
