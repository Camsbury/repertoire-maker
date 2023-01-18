(ns repertoire-maker.util.notation
  (:require
   [python-base]
   [libpython-clj2.python :refer [py. py.-]]
   [libpython-clj2.require :refer [require-python]]))

(require-python
 '[chess :as chess])

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
  ([sans]
   (sans->fen sans nil))
  ([sans fen]
   (let [board
         (if (some? fen)
           (chess/Board fen)
           (chess/Board))]
     (reduce
      (fn [b m] (py. b "push_san" m) b)
      board
      sans)
     (py. board "fen"))))

(defn ucis->fen
  "Convert a list of UCI notation moves to a FEN board state"
  ([ucis]
   (ucis->fen ucis nil))
  ([ucis fen]
   (let [board
         (if (some? fen)
           (chess/Board fen)
           (chess/Board))]
     (reduce
      (fn [b m] (py. b "push_uci" m) b)
      board
      ucis)
     (py. board "fen"))))
