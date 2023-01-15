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
       (log/info "Hit the move history rate limit. Waiting one minute before resuming requests.")
       (Thread/sleep 60000)
       (moves->options opts moves))
     (catch Object _
       (println (:throwable &throw-context) "error for moves: " moves)
       (throw+)))))

(defn- expand-moves
  [{:keys [filter-pct local? moves parent-pct]}]
  (->> moves
       (moves->options {:group :lichess :local? local?})
       (filter #(< filter-pct (* parent-pct (:play-pct %))))
       (map (fn [move] {:moves (conj moves (:uci move))
                        :pct   (* parent-pct (:play-pct move))}))))

(defn- expand-movesets
  [{:keys [filter-pct
           local?
           movesets
           tree]
    :or   {filter-pct (get-in defaults [:algo :filter-pct])}
    :as   opts}]
  (reduce
   (fn [acc {:keys [moves pct]}]
     (if-let [moveset (seq (expand-moves
                            {:moves      moves
                             :parent-pct pct
                             :filter-pct filter-pct
                             :local?     local?}))]
       (update acc :movesets into moveset)
       acc))
   (assoc opts :movesets [])
   movesets))

(defn- choose-moves
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
              (-> opts
                  (merge
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
                  (cond->
                      (some? player)
                    (assoc :player (moves->options
                                    {:group  :player
                                     :color  color
                                     :player player
                                     :since  since
                                     :local? local?}
                                    moves)))
                  strategy/choose-move
                  :moves)]
       ;; want to update this to take all the move data into the tree...
       ;; but currently new-moves are only the ucis!
       ;; this is likely the same in expand movesets
       (-> acc
           (update :tree util/add-tree-branch new-moves)
           (update :movesets conj (assoc moveset :moves new-moves)))
       acc))
   (assoc opts :movesets [])
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
  (loop [opts
         (merge opts
                {:tree (util/add-tree-branch nil moves)
                 :movesets  [{:moves moves
                              :pct   (calc-prob opts)}]})]
    (let [move-selector
          (if (->> opts
                   :movesets
                   first
                   :moves
                   util/whose-turn?
                   (= color))
            choose-moves
            expand-movesets)]
      (if (empty? (:movesets opts))
        ;; TODO: include win% and engine eval in the move tree as well
        ;; (do a max on 1 and score for moves on eval calc)
        ;; TODO: do depth first traversal to fill out the lines in the natural
        ;; order, then sorting here is unnecessary
        (:tree opts)
        (recur (move-selector opts))))))

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
         :filter-pct      0.05
         :move-choice-pct 0.01
         :moves           ["e4"]
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
