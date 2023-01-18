(ns repertoire-maker.strategy
  (:require
   [repertoire-maker.engine :as ngn]
   [repertoire-maker.history :as h]
   [repertoire-maker.default :refer [defaults]]))

(defn- extract-filtered-moves
  [opts candidates]
  (let [best-score (->> candidates first :score)
        allowable-loss (or (:allowable-loss opts)
                           (get-in defaults [:engine :allowable-loss]))]
    (->> candidates
         (filter #(> allowable-loss (/ (:score %) best-score)))
         (mapv :uci))))

(defn- filter-engine
  [opts engine-candidates]
  (fn
    [move-candidates]
    (if (->> engine-candidates
             (extract-filtered-moves opts)
             seq?)
      (filter #(contains? (set engine-candidates) (:uci %)) move-candidates)
      move-candidates)))

(defn- prepare-masters-candidates
  [{:keys [masters? min-total-masters moves]
    :or   {min-total-masters (get-in defaults [:algo :min-total-masters])}
    :as opts}]
  (when masters?
    (let [candidates
          (h/moves->candidates
           (assoc opts :group :masters)
           moves)

          total-plays (->> candidates
                           (map :play-count)
                           (reduce +))]
      (when (> total-plays min-total-masters)
        candidates))))

(defn- prepare-player-move
  [{:keys [player moves] :as opts} engine-filter]
  (when player
    (some->> moves
             (h/moves->candidates
              (assoc opts :group :player))
             engine-filter
             first
             :uci)))

(defn- get-candidate
  [candidates move]
  (->> candidates
       (filter #(= (:uci move) (:uci %)))
       first))

(defn choose-move
  "This is the core of the entire repertoire. What move do you make in a given
  position?"
  [{:keys [color moves overrides] :as opts}]

  (let [engine-candidates  (ngn/prepare-engine-candidates opts)
        engine-filter      (filter-engine opts engine-candidates)
        player-move        (prepare-player-move opts engine-filter)
        overridden-move    (get overrides moves)
        lichess-candidates (delay (-> opts
                                      (assoc :group :lichess)
                                      (h/moves->candidates moves)))
        candidates         (or (prepare-masters-candidates opts)
                               @lichess-candidates)
        chosen-move
        (cond
          (some? overridden-move)
          (->> candidates
               (filter #(= overridden-move (:uci %)))
               first)
          (some? player-move)
          (->> candidates
               engine-filter
               (filter #(= player-move (:uci %)))
               first)
          :else
          (->> candidates
               (filter #(< (or (:min-plays opts)
                               (get-in defaults [:algo :min-plays]))
                           (:play-count %)))
               (filter #(< (or (:move-choice-pct opts)
                               (get-in defaults [:algo :move-choice-pct]))
                           (:play-pct %)))
               engine-filter
               (sort-by (get {:black :white :white :black} color))
               first))

        chosen-move
        (or chosen-move (first engine-candidates))]

    (some-> chosen-move
            (merge
             {:chosen? true
              :pct     (:pct opts)
              :moves   (conj moves (:uci chosen-move))
              :white   (->> chosen-move
                            (get-candidate @lichess-candidates)
                            :white)
              :black   (->> chosen-move
                            (get-candidate @lichess-candidates)
                            :black)
              :score   (->> chosen-move
                            (get-candidate engine-candidates)
                            :score)}))))

