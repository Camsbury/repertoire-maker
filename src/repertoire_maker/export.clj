(ns repertoire-maker.export
  (:require
   [python-base]
   [libpython-clj2.python :refer [py. py.-] :as py]
   [libpython-clj2.require :refer [require-python]]))

(require-python
 '[chess     :as chess]
 '[chess.pgn :as pgn])

(defn- get-in-tree
  [tree moves]
  (get-in
   tree
   (-> :responses
       (interpose moves)
       vec
       (conj :responses))))

(defn export-repertoire
  ([move-tree]
   (export-repertoire move-tree nil))
  ([move-tree path]
   (let [game (pgn/Game)]
     ;;  traversal writing the game tree
     (loop [stack (mapv (fn [move] [[move] [game]]) (keys move-tree))]
       (when (seq stack)
         (let [[moves nodes] (last stack)
               nodes         (conj nodes
                                   (py. (last nodes)
                                        "add_variation"
                                        (py. chess/Move "from_uci" (last moves))))
               next          (keys (get-in-tree move-tree moves))]
           (if (seq next)
             (recur
              (reduce
               (fn [acc move]
                 (conj acc [(conj moves move) nodes]))
               (drop-last stack)
               next))
             (recur (drop-last stack))))))
     (if path
       ;; NOTE: maybe need to with-out-str and print, idk
       (spit path game)
       (println game))
     move-tree)))
