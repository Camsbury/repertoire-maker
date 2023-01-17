(ns repertoire-maker.util
  (:require
   [python-base]
   [libpython-clj2.python :refer [py. py.-]]
   [libpython-clj2.require :refer [require-python]]
   [flatland.ordered.map  :refer [ordered-map]]
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

(defn ucis->fen
  "Convert a list of UCI notation moves to a FEN board state"
  [sans]
  (let [board (chess/Board)]
    (reduce
     (fn [b m] (py. b "push_uci" m) b)
     board
     sans)
    (py. board "fen")))

(defn add-tree-branch
  [tree {:keys [moves] :as move}]
  (let [tree (or tree (ordered-map))]
    (cond
      (= 1 (count moves))
      (assoc tree (first moves) (dissoc move :moves))

      (seq moves)
      (update-in
       tree
       [(first moves) :responses]
       #(add-tree-branch % (update move :moves rest)))

      :else
      tree)))

(defn get-in-tree
  [tree moves]
  (get-in tree (-> :responses
                   (interpose moves)
                   vec
                   (conj :responses))))

(comment
  )
