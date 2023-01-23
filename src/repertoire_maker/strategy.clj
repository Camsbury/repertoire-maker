(ns repertoire-maker.strategy)

(defn- agg-stat
  [stat]
  (keyword (str (name stat) "-agg")))

(defn- m-stat
  [stat]
  (keyword (str (name stat) "-m")))

(defmulti apply-strategy
  "Apply a strategy to choose moves"
  :strategy)

(defmethod apply-strategy :default
  [opts]
  (-> opts
      (assoc :strategy :min-loss)
      apply-strategy))

(defmethod apply-strategy :min-loss
  [{:keys [color children]}]
  ;; loss is the opposite color
  (let [color ({:black :white :white :black} color)]
    (->> children
         (sort-by
          (fn [[_ node]]
            (or
             (get node (agg-stat (m-stat color)))
             (get node (agg-stat color)))))
         ffirst)))

(defmethod apply-strategy :max-win-over-loss
  [{:keys [color children]}]
  (let [opp-color ({:black :white :white :black} color)]
    (->> children
         (sort-by
          (fn [[_ node]]
            (let [my-m-stat  (get node (agg-stat (m-stat color)))
                  opp-m-stat (get node (agg-stat (m-stat opp-color)))
                  my-stat    (get node (agg-stat color))
                  opp-stat   (get node (agg-stat opp-color))]
              (or
               (when (and (some? my-m-stat)
                          (some? opp-m-stat)
                          (not (zero?  opp-m-stat)))
                 (/ my-m-stat opp-m-stat))

               (if (and (some? my-stat)
                          (some? opp-stat)
                          (not (zero? opp-stat)))
                 (/ my-stat opp-stat)
                 0.0))))
          >)
         ffirst)))

