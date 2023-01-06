(ns repertoire-maker.core
  (:require
   [repertoire-maker.export :as export]
   [repertoire-maker.strategy :as strategy]
   [repertoire-maker.util :as util]
   [clojure.core.async :refer [thread]]
   [clojure.string :as str]
   [clj-http.client :as http]))

(def request-wait-periods
  {:games 700})

(def request-wait-state
  {:games (agent 0)})

(defn- trigger-request-wait-period
  [endpoint]
  (let [state (get request-wait-state endpoint)]
    (Thread/sleep (max @state 0))
    (send state (fn [_] (get request-wait-periods endpoint)))
    (thread
      (loop []
        (Thread/sleep 100)
        (when (< 0 @state)
          (send state #(- % 100))
          (recur))))))

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
  [group moves]
  (let [
        speeds  ["bullet" "blitz" "rapid"]
        ratings [2000 2200 2500]]
    #_
    (trigger-request-wait-period :games)
    (Thread/sleep 700)
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
              :ratings     (str/join "," ratings)}))})
        :body
        util/from-json
        :moves
        process-options)))

(defn- expand-moves
  [moves]
  (let [filter-pct   0.01
        filter-plays 100]
    (->> moves
         (moves->options :lichess)
         (filter #(< filter-pct (:play-pct %)))
         (filter #(< filter-plays (:play-count %)))
         (map :uci)
         (map #(conj moves %)))))

(defn- expand-movesets
  [movesets]
  (reduce
   (fn [acc moves]
     (if-let [moveset (seq (expand-moves moves))]
       (update acc 1 into moveset)
       (update acc 0 conj moves)
       ))
   [[] []]
   movesets))

(defn build-repertoire
  [{:keys [color moves depth]
    :or   {depth 5}}]
  (loop [exhausted [] movesets [moves]]
    (if (->> movesets
             first
             count
             (= depth))
      (into exhausted movesets)
      (if (->> movesets
               first
               util/whose-turn?
               (= color))
        (recur
         exhausted
         (map
          #(strategy/select-option
            {:moves   %
             :master  (moves->options :masters %)
             :lichess (moves->options :lichess %)
             :engine  [] ;; TODO: pull in engine analyzed moves
             :color   color})
          movesets))
        (let [[exhausted movesets] (expand-movesets movesets)]
          (recur
           exhausted
           movesets))))))

(comment

  (let [color :white
        moves ["e2e4" "c7c5"]
        depth 10]
    (-> {:color color
         :moves moves
         :depth depth}
        build-repertoire
        export/export-repertoire))
  )
