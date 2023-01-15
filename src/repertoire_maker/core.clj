(ns repertoire-maker.core
  (:require
   [taoensso.timbre :as log]
   [repertoire-maker.default :refer [defaults]]
   [repertoire-maker.engine :as ngn]
   [repertoire-maker.export :as export]
   [repertoire-maker.strategy :as strategy]
   [repertoire-maker.util :as util]
   [slingshot.slingshot :refer [try+ throw+]]
   [clojure.string :as str]
   [clj-http.client :as http]))

(def urls
  {:local "http://localhost:9002/"
   :public "https://explorer.lichess.ovh/"})

(defn- total-option
  [{:keys [white draws black]}]
  (+ white draws black))

(defn process-option
  [total-count {:keys [uci white draws black] :as option}]
  (let [total (total-option option)
        white (float (/ white total))
        black (float (/ black total))]
    {:uci        uci
     :white      white
     :black      black
     :play-count total
     :play-pct   (float (/ total total-count))}))

(defn process-options
  [options]
  (let [total-count
        (->> options
             (map total-option)
             (reduce +))]
    (mapv #(process-option total-count %) options)))

(defn moves->options
  [{:keys [group since color player local?]
    :or   {since (get-in defaults [:history :since])}
    :as   opts}
   moves]
  (let [speeds  (get-in defaults [:history :speeds])
        ratings (get-in defaults [:history :ratings])
        url     (if local?
                  (:local urls)
                  (:public urls))]
    (try+
     (-> url
         (str (name group))
         (http/get
          {:query-params
           (cond-> {:moves (get-in defaults [:history :moves])
                    :topGames (get-in defaults [:history :top-games])
                    :play (str/join "," moves)}
             (= group :lichess)
             (merge
              {:recentGames (get-in defaults [:history :recent-games])
               :speeds      (str/join "," speeds)
               :ratings     (str/join "," ratings)})
             (= group :player)
             (merge
              {:recentGames (get-in defaults [:history :recent-games])
               :player      player
               :color       (name color)
               :since       since
               :speeds      (str/join "," speeds)}))})
         :body
         util/from-json
         :moves
         process-options)
     (catch [:status 429] _
       (log/info "Hit the book rate limit. Waiting one minute before resuming requests.")
       (Thread/sleep 60000)
       (moves->options opts moves))
     (catch Object _
       (println (:throwable &throw-context) "error for moves: " moves)
       (throw+)))))

(defn- expand-moves
  [{:keys [moves parent-pct filter-pct local?]}]
  (->> moves
       (moves->options {:group :lichess :local? local?})
       (filter #(< filter-pct (* parent-pct (:play-pct %))))
       (map (fn [move] {:moves (conj moves (:uci move))
                        :pct   (* parent-pct (:play-pct move))}))))

(defn- expand-movesets
  [{:keys [movesets filter-pct local?]
    :or   {filter-pct (get-in defaults [:algo :filter-pct])}}]
  (reduce
   (fn [acc {:keys [moves pct]}]
     (if-let [moveset (seq (expand-moves
                            {:moves      moves
                             :parent-pct pct
                             :filter-pct filter-pct
                             :local?     local?}))]
       (update acc 1 into moveset)
       (update acc 0 conj moves)))
   [[] []]
   movesets))

(defn- select-options
  [{:keys [allowable-loss
           color
           local?
           movesets
           player
           since
           use-engine?]
    :or   {allowable-loss  (get-in defaults [:engine :allowable-loss])
           since           (get-in defaults [:history :since])}
    :as   opts}]
  (reduce
   (fn [acc {:keys [moves] :as moveset}]
     (if-let [new-moves
              (strategy/select-option
               (cond->
                   (merge opts
                          {:moves           moves
                           #_#_
                           :masters         (moves->options
                                             {:group :masters
                                              :local? local?}
                                             moves)
                           :lichess         (moves->options
                                             {:group :lichess
                                              :local? local?}
                                             moves)
                           :engine          (when use-engine?
                                              (ngn/moves->engine-options
                                               (-> defaults
                                                   :engine
                                                   (assoc :moves
                                                          moves)
                                                   (assoc :allowable-loss
                                                          allowable-loss))))})
                 (some? player)
                 (assoc :player (moves->options
                                 {:group  :player
                                  :color  color
                                  :player player
                                  :since  since
                                  :local? local?}
                                 moves))))]
       (update acc 1 conj (assoc moveset :moves new-moves))
       (update acc 0 conj moves)))
   [[] []]
   movesets))

(defn- overrides->uci
  [overrides]
  (->> overrides
       (map
        (fn [[base tip]]
          (let [ucis (util/sans->ucis (conj base tip))
                base (into [] (drop-last ucis))
                tip (last ucis)]
            [base tip])))
       (into {})))

(defn- calc-prob
  [{:keys [moves color local?]}]
  (:pct
   (reduce
    (fn [{:keys [pct stack]} move]
      (if (= color (util/whose-turn? stack))
        {:pct pct :stack (conj stack move)}
        (let [move-eval (->> stack
                             (moves->options
                              {:group :lichess
                               :local? local?})
                             (filter #(= move (:uci %)))
                             first)]
          {:pct (* pct (:play-pct move-eval))
           :stack (conj stack move)})))
    {:pct 1.0 :stack []}
    moves)))

(defn build-repertoire
  [{:keys [color moves] :as opts}]
  (loop [exhausted []
         movesets  [{:moves moves
                     :pct   (calc-prob opts)}]]
    (let [opts (assoc opts :movesets movesets)
          move-selector (if (->> movesets
                                 first
                                 :moves
                                 util/whose-turn?
                                 (= color))
                          select-options
                          expand-movesets)]
      (if (empty? movesets)
        ;; TODO: include win% and engine eval in "exhausted" as well
        ;; (do a max on 1 and score for moves on eval calc)
        ;; TODO: do depth first traversal to fill out the lines in the natural
        ;; order, then sorting here is unnecessary
        exhausted
        (let [[new-exhausted movesets]
              (move-selector opts)]
          (recur
           (into exhausted new-exhausted)
           movesets))))))

(def overrides
  {["e4" "e5"]                       "Nf3"
   ["e4" "c5"]                       "Nf3"
   ["e4" "e6"]                       "d4"
   ["e4" "c5" "Nf3" "d6"]            "d4"
   ["d4" "f5"]                       "e4"
   ["d4" "b6"]                       "e4"
   ["d4" "d5" "c4" "c6" "Nf3" "Nf6"] "Qc2"})

(defn build-and-export
  [config]
  (-> config
      (update :moves util/sans->ucis)
      (update :overrides overrides->uci)
      build-repertoire
      export/export-repertoire))

(comment

  (-> "http://localhost:9002/lichess"
      (http/get
       {:query-params
        {:moves       30
         :topGames    0
         :play        "d2d4,g8f6,c2c4"
         :recentGames 0
         :speeds      "bullet,blitz,rapid"
         :ratings     "2000,2200,2500"}})
      :body
      util/from-json
      :moves
      process-options)

  (let [config
        {:allowable-loss  0.9
         :color           :white
         :filter-pct      0.01
         :move-choice-pct 0.01
         :moves           ["e4" "e5" "Nf3" "Nc6" "Bb5"]
         #_#_
         :use-engine?     true
         :local?          true
         #_#_
         :overrides       overrides
         #_#_
         :player          "JackSilver"}]
    (build-and-export config))

  (let [config
        {:allowable-loss  0.9
         :color           :white
         :filter-pct      0.001
         :move-choice-pct 0.01
         :local?          true}]
    (doall
     (map #(build-and-export (assoc config :moves %))
          [["e4" "e5" "Nf3" "Nc6" "Bb5"]
           ["e4" "e5" "Nf3" "Nc6" "Bc4"]
           ["e4" "e5" "Nf3" "Nc6" "d4"]
           ["e4" "e5" "Nf3" "Nc6" "Nc3"]])))

  )
