(ns repertoire-maker.strategy)

(defn- agg-stat
  [stat]
  (keyword (str (name stat) "-agg")))

(defn- m-stat
  [stat]
  (keyword (str (name stat) "-m")))

(defmulti strategy->sort-fn
  "Gets the correct sort fn for a strategy"
  :strategy)

(defmethod strategy->sort-fn :default
  [opts]
  (-> opts
      (assoc :strategy :min-loss)
      strategy->sort-fn))

(defmethod strategy->sort-fn :min-loss
  [{:keys [color]}]
  ;; loss is the opposite color
  (let [color ({:black :white :white :black} color)]
    (fn [node]
      (or
       (get node (agg-stat (m-stat color)))
       (get node (agg-stat color))))))

(defmethod strategy->sort-fn :max-win-over-loss
  [{:keys [color]}]
  (let [opp-color ({:black :white :white :black} color)]
    (fn [node]
      (let [my-m-stat  (get node (agg-stat (m-stat color)))
            opp-m-stat (get node (agg-stat (m-stat opp-color)))
            my-stat    (get node (agg-stat color))
            opp-stat   (get node (agg-stat opp-color))]
        (or
         (when (and (some? my-m-stat)
                    (some? opp-m-stat)
                    (not (zero? my-m-stat))
                    (not (zero? opp-m-stat)))
           (/ opp-m-stat my-m-stat))

         (if (and (some? my-stat)
                  (some? opp-stat)
                  (not (zero? my-stat))
                  (not (zero? opp-stat)))
           (/ opp-stat my-stat)
           0.0))))))

(defn apply-strategy
  [{:keys [children] :as opts}]
  (->> children
       (sort-by
        (fn [[_ node]]
          ((strategy->sort-fn opts)
           node)))
       ffirst))
