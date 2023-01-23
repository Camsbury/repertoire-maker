(ns repertoire-maker.export
  (:require
   [python-base]
   [libpython-clj2.python :refer [py. py.-] :as py]
   [libpython-clj2.require :refer [require-python]]
   [repertoire-maker.tree :as tutil]))

(require-python
 '[chess     :as chess]
 '[chess.pgn :as pgn])

(defn export-repertoire
  ([move-tree]
   (export-repertoire move-tree nil))
  ([move-tree path]
   (let [game (pgn/Game)]
     ;;  traversal writing the game tree
     (loop [stack (mapv
                   (fn [move] [[move] [game]])
                   (->> move-tree
                        :responses
                        keys
                        reverse))]
       (when (seq stack)
         (let [[moves nodes] (last stack)
               nodes         (->> moves
                                  last
                                  (py. chess/Move "from_uci")
                                  (py. (last nodes) "add_variation")
                                  (conj nodes))
               next          (->> moves
                                  (tutil/resp-in-tree move-tree)
                                  (remove #(:trim? (second %)))
                                  keys
                                  reverse)
               stack         (->> stack
                                  drop-last
                                  (into []))]
           (if (seq next)
             (recur
              (reduce
               (fn [acc move]
                 (conj acc [(conj moves move) nodes]))
               stack
               next))
             (recur stack)))))
     (if path
       ;; NOTE: maybe need to with-out-str and print, idk
       (spit path game)
       (println game))
     move-tree)))
