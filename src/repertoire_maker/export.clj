(ns repertoire-maker.export
  (:require
   [python-base]
   [repertoire-maker.util :as util]
   [flatland.ordered.map  :as ordered]
   [libpython-clj2.python :refer [py.]]
   [libpython-clj2.require :refer [require-python]]))

(require-python
 '[chess     :as chess]
 '[chess.pgn :as pgn])

(defn calc-depths
  [depth move-tree]
  (->> move-tree
       (mapcat
        (fn [[_ v]]
          (if (seq v)
            (calc-depths (inc depth) v)
            [depth])))))

(defn tree-width
  [move-tree]
  (->> move-tree
       (map
        (fn [[_ v]]
          (if (seq v)
            (tree-width v)
            1)))
       (reduce +)))

(defn average-depth
  [move-tree]
  (double
   (/
    (reduce + (calc-depths 1 move-tree))
    (tree-width move-tree))))

(defn export-repertoire
  [move-tree]
  (let [game (pgn/Game)]
    (println move-tree)
    (println "average depth: " (average-depth move-tree))
    (println "tree width: " (tree-width move-tree))
    ;;  traversal writing the game tree
    (loop [stack (mapv (fn [move] [[move] [game]]) (keys move-tree))]
      (when (seq stack)
        (let [[moves nodes] (last stack)
              nodes         (conj nodes
                                  (py. (last nodes)
                                       "add_variation"
                                       (py. chess/Move "from_uci" (last moves))))
              next          (keys (util/get-in-tree move-tree moves))]
          (if (seq next)
            (recur
             (reduce
              (fn [acc move]
                (conj acc [(conj moves move) nodes]))
              (drop-last stack)
              next))
            (recur (drop-last stack))))))
    (println game)))
