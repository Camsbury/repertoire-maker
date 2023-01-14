(ns repertoire-maker.core
  (:require
   [repertoire-maker.engine :as ngn]
   [repertoire-maker.export :as export]
   [repertoire-maker.strategy :as strategy]
   [repertoire-maker.util :as util]
   [slingshot.slingshot :refer [try+ throw+]]
   [clojure.core.async :refer [thread]]
   [clojure.string :as str]
   [clj-http.client :as http]))

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
  [{:keys [group since color player local?]
    :or   {since "1952-01"}
    :as   opts}
   moves]
  (let [speeds  ["bullet" "blitz" "rapid"]
        ratings [2000 2200 2500]
        url     (if local?
                  "http://localhost:9002/"
                  "https://explorer.lichess.ovh/")]
    (try+
     (-> url
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
     (catch [:status 429] _
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
  [{:keys [movesets filter-pct local?]}]
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
           move-choice-pct
           movesets
           overrides
           player
           since
           use-engine?]}]
  (reduce
   (fn [acc {:keys [moves pct] :as moveset}]
     (if-let [new-moves
              (strategy/select-option
               (cond->
                   {:moves           moves
                    :move-choice-pct move-choice-pct
                    ;; :masters         (moves->options
                    ;;                   {:group :masters
                    ;;                    :local? local?}
                    ;;                   moves)
                    :lichess         (moves->options
                                      {:group :lichess
                                       :local? local?}
                                      moves)
                    :engine          (when use-engine?
                                       (ngn/moves->engine-options
                                        {:moves          moves
                                         :m-count        10
                                         :allowable-loss allowable-loss}))
                    :overrides       overrides
                    :color           color}
                 (some? player)
                 (assoc :player (moves->options
                                 {:group  :player
                                  :color  (name color)
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

(defn build-repertoire
  [{:keys [allowable-loss
           color
           filter-pct
           local?
           move-choice-pct
           moves
           overrides
           player
           since
           use-engine?]
    :or   {allowable-loss  100
           filter-pct      0.01
           move-choice-pct 0.01
           since           "1952-01"}}]
  (loop [exhausted []
         movesets  [{:moves (util/sans->ucis moves)
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
                :local?          local?
                :move-choice-pct move-choice-pct
                :movesets        movesets
                :overrides       (overrides->uci overrides)
                :player          player
                :since           since
                :use-engine?     use-engine?})]
          (recur
           (into exhausted new-exhausted)
           movesets))
        (let [[new-exhausted movesets]
              (expand-movesets
               {:movesets   movesets
                :filter-pct filter-pct
                :local?     local?})]
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
      build-repertoire
      export/export-repertoire))

(comment

  (-> "http://localhost:9002/lichess"
      (http/get
       {:query-params
        {:moves 30
         :topGames 0
         :play "d2d4,g8f6,c2c4"
         :recentGames 0
         :speeds "bullet,blitz,rapid"
         :ratings "2000,2200,2500"}})
      :body
      util/from-json
      :moves
      process-options)

  ;; test for API speed:
  ;; (time
  ;;  (let [moves (atom []) exit (atom false)]
  ;;    (loop []
  ;;      (try+
  ;;       (let [move
  ;;             (->
  ;;              (http/get
  ;;               "https://explorer.lichess.ovh/lichess"
  ;;               {:query-params
  ;;                {:moves 30
  ;;                 :topGames 0
  ;;                 :play (str/join "," @moves)
  ;;                 :recentGames 0
  ;;                 :speeds "bullet,blitz,rapid"
  ;;                 :ratings "2000,2200,2500"}})
  ;;              :body
  ;;              util/from-json
  ;;              :moves
  ;;              process-options
  ;;              first
  ;;              :uci)]
  ;;         (swap! moves conj move))
  ;;       (catch [:status 429] _
  ;;         (reset! exit true)))
  ;;      (if-not (or @exit (nil? (last @moves)))
  ;;        (recur)
  ;;        (println (count @moves))))))

  (let [repertoire-config
        {:allowable-loss  10
         :color           :white
         :filter-pct      0.01
         :move-choice-pct 0.01
         :moves           ["e4" "e5" "Nf3" "Nc6" "Bc4"]
         ;; :use-engine?     true
         :local?          true
         #_#_#_#_
         :overrides       overrides
         :player          "JackSilver"}]
    (-> repertoire-config
        build-repertoire
        export/export-repertoire))

  )
