(ns repertoire-maker.export
  (:require
   [python-base]
   [libpython-clj2.python :refer [py.]]
   [libpython-clj2.require :refer [require-python]]))

(require-python
 '[chess     :as chess]
 '[chess.pgn :as pgn])

(defn export-repertoire
  [movesets]
  (let [move-tree (reduce #(assoc-in %1 %2 nil) {} movesets)
        game      (pgn/Game)]
    ;;  traversal writing the game tree
    (loop [stack (mapv (fn [move] [[move] [game]]) (keys move-tree))]
      (when (seq stack)
        (let [[moves nodes] (last stack)
              nodes         (conj nodes
                                  (py. (last nodes)
                                       "add_variation"
                                       (py. chess/Move "from_uci" (last moves))))
              next          (keys (get-in move-tree moves))]
          (if (seq next)
            (recur
             (reduce
              (fn [acc move]
                (conj acc [(conj moves move) nodes]))
              (drop-last stack)
              next))
            (recur (drop-last stack))))))
    (println game)))
