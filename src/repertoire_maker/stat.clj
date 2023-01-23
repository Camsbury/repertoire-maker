(ns repertoire-maker.stat
  (:require
   [taoensso.timbre :as log]))

(defn agg-stat
  [stat]
  (keyword (str (name stat) "-agg")))

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

(defn total-prob-agg
  [move-tree]
  (->> move-tree
       get-leaves
       (map :prob-agg)
       (reduce +)))

#_
(defn log-stats
  [move-tree]
  (println
   "stats: "
   {:average-depth (average-depth move-tree)
    :width         (tree-width    move-tree)
    :white         (weighted-stat move-tree :white)
    :black         (weighted-stat move-tree :black)
    :score         (weighted-stat move-tree :score)})
  move-tree)
