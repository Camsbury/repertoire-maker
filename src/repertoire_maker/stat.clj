(ns repertoire-maker.stat
  (:require
   [taoensso.timbre :as log]))

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

(defn total-play-pct
  [move-tree]
  (->> move-tree
       get-leaves
       (map :pct)
       (reduce +)))

(defn- do-weighted-stat
  [move-tree stat]
  (if (->> move-tree first second :pct some?)
    (loop [trees (map second move-tree) total 0.0]
      (if (seq trees)
        (let [{:keys [chosen? pct responses] :as parent} (first trees)
              trees (rest trees)
              parent-stat (get parent stat)]

          (cond
            (nil? parent-stat)
            (recur
             (into (map second responses) trees)
             total)

            (not chosen?)
            (recur
             (into [(->> responses first second)] trees)
             total)

            (seq responses)
            (let [other-component
                  (->> responses
                       (map
                        #(*
                          (get-in % [1 :play-pct] 0)
                          (get-in % [1 stat] 0)))
                       (reduce +))
                  other-pct
                  (->> responses
                       (map
                        #(get-in % [1 :play-pct] 0))
                       (reduce +))
                  parent-component
                  (if (= stat :score)
                    (* parent-stat
                       (- 1 other-pct))
                    (- parent-stat
                       other-component))]
              (recur
               (into
                (map
                 (fn [[_ {:keys [responses]}]]
                   (map second responses))
                 responses)
                trees)
               (+ total (* pct parent-component))))

            (seq trees)
            (recur trees (+ total (* pct parent-stat)))

            :else
            (+ total (* pct parent-stat))))
        total))
    (recur (->> move-tree first second :responses) stat)))

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
                (throw (Exception. "pct doesn't exist for any main node"))))))]
    (/
     (do-weighted-stat move-tree stat)
     parent-pct)))

(defn log-stats
  [move-tree]
  (log/info
   "stats: "
   {:average-depth (average-depth move-tree)
    :width         (tree-width    move-tree)
    :white         (weighted-stat move-tree :white)
    :black         (weighted-stat move-tree :black)
    :score         (weighted-stat move-tree :score)})
  move-tree)
