(ns repertoire-maker.stat)

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
        0.0))
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
