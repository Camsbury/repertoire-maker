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

(defn- do-weighted-stat
  [move-tree stat]
  (reduce
   (fn [acc [_ {:keys [pct responses] :as parent}]]
     (let [parent-stat (get parent stat)
           stat-piece
           (cond
             (nil? parent-stat)
             (do-weighted-stat responses stat)

             (seq responses)
             (let [parent-component
                   (- parent-stat
                      (->> responses
                           (map #(* (get % :play-pct 0) (get-in % [1 stat] 0)))
                           (reduce +)))]
               (+ parent-component (do-weighted-stat responses stat)))

             :else
             parent-stat)]
       (+ acc (* pct stat-piece))))
   0.0
   move-tree))

(defn weighted-stat
  [move-tree stat]
  (let [parent-pct
        (if (->> move-tree first second :pct some?)
          (->> move-tree
               (map #(get-in % [1 :pct] 0.0))
               (reduce +))
          (loop [child (->> move-tree first second :responses first second)]
            (let [pct (:pct child)
                  responses (:responses child)]
              (cond
                (some? pct)
                pct

                (seq responses)
                (recur (->> responses first second))

                :else
                (do
                  (println child)
                  (throw (Exception. "This stat doesn't exist for any main node"))))))
          )]
    (/
     (do-weighted-stat move-tree stat)
     parent-pct)))

(defn export-repertoire
  [move-tree]
  (let [game (pgn/Game)]
    #_
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
