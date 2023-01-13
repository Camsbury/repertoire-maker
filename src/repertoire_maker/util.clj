(ns repertoire-maker.util
  (:require
   [python-base]
   [libpython-clj2.python :refer [py. py.-]]
   [libpython-clj2.require :refer [require-python]]
   [camel-snake-kebab.core :as csk]
   [clojure.data.json :as json]))

(require-python
 '[chess     :as chess]
 '[chess.pgn :as pgn])

(defn from-json [raw]
  (json/read-str raw :key-fn (comp csk/->kebab-case keyword)))

(defn whose-turn? [moves]
  (if (= 0 (mod (count moves) 2))
    :white
    :black))

(defn sans->ucis
  "Convert a list of SAN notation moves to UCI notation moves"
  [sans]
  (let [board (chess/Board)]
    (reduce
     (fn [b m] (py. b "push_san" m) b)
     board
     sans)
    (->> "move_stack"
         (py.- board)
         (mapv #(py. % "uci")))))

(defn sans->fen
  "Convert a list of SAN notation moves to a FEN board state"
  [sans]
  (let [board (chess/Board)]
    (reduce
     (fn [b m] (py. b "push_san" m) b)
     board
     sans)
    (py. board "fen")))

(comment
  (sans->fen ["e4" "e5" "Nf3"])
  )
