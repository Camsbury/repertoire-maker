(ns repertoire-maker.export
  (:require
   [python-base]
   [taoensso.timbre :as log]
   [repertoire-maker.util :as util]
   [repertoire-maker.stat :refer [weighted-stat]]
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
        (fn [[_ {v :responses}]]
          (if (seq v)
            (calc-depths (inc depth) v)
            [depth])))))

(defn tree-width
  [move-tree]
  (->> move-tree
       (map
        (fn [[_ {v :responses}]]
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

(defn- get-leaves
  [move-tree]
  (->> move-tree
       (mapcat
        (fn [[_ {:keys [responses] :as move}]]
          (if (seq responses)
            (get-leaves responses)
            [move])))))

(defn- total-play-pct
  [move-tree]
  (->> move-tree
       get-leaves
       (map :pct)
       (reduce +)))

(defn export-repertoire
  [move-tree]
  (let [game (pgn/Game)]
    (println move-tree)
    #_
    (println "average depth: " (average-depth move-tree))
    #_
    (println "tree width: " (tree-width move-tree))
    #_
    (println "leaves: " (get-leaves move-tree))
    #_
    (println "total play pct: " (total-play-pct move-tree))
    (println "total win%: " (weighted-stat move-tree :white))
    (println "total loss%: " (weighted-stat move-tree :black))
    (println "total score: " (weighted-stat move-tree :score))
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
