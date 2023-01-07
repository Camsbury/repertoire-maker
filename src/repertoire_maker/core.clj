(ns repertoire-maker.core
  (:require
   [repertoire-maker.engine :as ngn]
   [repertoire-maker.export :as export]
   [repertoire-maker.strategy :as strategy]
   [repertoire-maker.util :as util]
   [clojure.core.async :refer [thread]]
   [clojure.string :as str]
   [clj-http.client :as http]))

;; (def request-wait-periods
;;   {:games 700})

;; (def request-wait-state
;;   {:games (agent 0)})

;; (defn- trigger-request-wait-period
;;   [endpoint]
;;   (let [state (get request-wait-state endpoint)]
;;     (Thread/sleep (max @state 0))
;;     (send state (fn [_] (get request-wait-periods endpoint)))
;;     (thread
;;       (loop []
;;         (Thread/sleep 100)
;;         (when (< 0 @state)
;;           (send state #(- % 100))
;;           (recur))))))

(defn- total-option
  [{:keys [white draws black]}]
  (+ white draws black))

(defn- process-option
  [total-count {:keys [uci white draws black] :as option}]
  (let [total (total-option option)
        white (float (/ white total))
        black (float (/ black total))]
    {:uci        uci
     :white      white
     :black      black
     :play-count total
     :play-pct   (float (/ total total-count))}))

(defn- process-options
  [options]
  (let [total-count
        (->> options
             (map total-option)
             (reduce +))]
    (mapv #(process-option total-count %) options)))

(defn moves->options
  [{:keys [group since color player]
    :or   {since "1952-01"}}
   moves]
  (let [speeds  ["bullet" "blitz" "rapid"]
        ratings [2000 2200 2500]]
    #_
    (trigger-request-wait-period :games)
    ;; (Thread/sleep 700)
    (try
      (-> "https://explorer.lichess.ovh/"
          (str (name group))
          (http/get
           {:query-params
            (cond-> {:moves 30
                     :topGames 0
                     :play (str/join "," moves)}
              (= group :lichess)
              (merge
               {:recentGames 0
                :speeds      (str/join "," speeds)
                :ratings     (str/join "," ratings)})
              (= group :player)
              (merge
               {:recentGames 0
                :player      player
                :color       color
                :since       since
                :speeds      (str/join "," speeds)}))})
          :body
          util/from-json
          :moves
          process-options)
      (catch Exception e
        (do
          (println "Errored on: " moves)
          (throw e))))))

(defn- expand-moves
  [moves parent-pct filter-pct]
  (->> moves
       (moves->options {:group :lichess})
       (filter #(< filter-pct (* parent-pct (:play-pct %))))
       (map (fn [move] {:moves (conj moves (:uci move))
                        :pct   (* parent-pct (:play-pct move))}))))

(defn- expand-movesets
  [movesets filter-pct]
  (reduce
   (fn [acc {:keys [moves pct]}]
     (if-let [moveset (seq (expand-moves moves pct filter-pct))]
       (update acc 1 into moveset)
       (update acc 0 conj moves)))
   [[] []]
   movesets))

(defn- select-options
  [{:keys [allowable-loss
           color
           move-choice-pct
           movesets
           overrides
           player
           since]}]
  (reduce
   (fn [acc {:keys [moves pct] :as moveset}]
     (if-let [new-moves (strategy/select-option
                         (cond->
                             {:moves           moves
                              :move-choice-pct move-choice-pct
                              ;; :masters  (moves->options {:group :masters} moves)
                              :lichess         (moves->options {:group :lichess} moves)
                              :engine          (ngn/moves->engine-options
                                                {:moves          moves
                                                 :m-count        10
                                                 :allowable-loss allowable-loss})
                              :overrides       overrides
                              :color           color}
                           (some? player)
                           (assoc :player (moves->options
                                           {:group  :player
                                            :color  (name color)
                                            :player player
                                            :since  since}
                                           moves))))]
       (update acc 1 conj (assoc moveset :moves new-moves))
       (update acc 0 conj moves)))
   [[] []]
   movesets))

(defn build-repertoire
  [{:keys [allowable-loss
           color
           filter-pct
           move-choice-pct
           moves
           overrides
           player
           since]
    :or   {allowable-loss  100
           filter-pct      0.01
           move-choice-pct 0.01
           since           "1952-01"}}]
  (loop [exhausted []
         movesets  [{:moves moves
                     :pct   1.0}]]
    (if (empty? movesets)
      (->> movesets
           (into exhausted (map :moves))
           (sort-by count >))
      (if (->> movesets
               first
               :moves
               util/whose-turn?
               (= color))
        (let [[new-exhausted movesets]
              (select-options
               {:allowable-loss  allowable-loss
                :color           color
                :move-choice-pct move-choice-pct
                :movesets        movesets
                :overrides       overrides
                :player          player
                :since           since})]
          (recur
           (into exhausted new-exhausted)
           movesets))
        (let [[new-exhausted movesets]
              (expand-movesets movesets filter-pct)]
          (recur
           (into exhausted new-exhausted)
           movesets))))))

(def overrides
  {["e2e4" "e7e5"]               "g1f3"
   ["e2e4" "c7c5"]               "g1f3"
   ["e2e4" "e7e6"]               "d2d4"
   ["e2e4" "c7c5" "g1f3" "d7d6"] "d2d4"})

(comment

  (let [allowable-loss  100
        color           :white
        filter-pct      0.005
        move-choice-pct 0.01
        moves           ["e2e4"]
        overrides       overrides
        player          "JackSilver"]
    (-> {:allowable-loss  allowable-loss
         :color           color
         :filter-pct      filter-pct
         :move-choice-pct move-choice-pct
         :moves           moves
         :overrides       overrides
         :player          player}
        build-repertoire
        export/export-repertoire))
  )
