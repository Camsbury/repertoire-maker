(ns repertoire-maker.history
  (:require
   [taoensso.timbre :as log]
   [repertoire-maker.notation :as not]
   [repertoire-maker.default :refer [defaults]]
   [repertoire-maker.util.web :as web]
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
  [total-count {:keys [uci white black] :as option}]
  (let [total (total-option option)
        white (float (/ white total))
        black (float (/ black total))]
    {:uci        uci
     :white      white
     :black      black
     :play-count total
     :prob       (float (/ total total-count))}))

(defn process-candidates
  [candidates]
  (let [total-count
        (->> candidates
             (map total-option)
             (reduce +))]
    (mapv #(process-option total-count %) candidates)))

(defn- do-query-history
  [url params]
  (http/get url params))

(def query-history
  (memoize do-query-history))

(defn historic-moves
  [{:keys [group since color ucis player local?]
    :or   {since (get-in defaults [:history :since])}
    :as   opts}]
  (let [speeds  (get-in defaults [:history :speeds])
        ratings (get-in defaults [:history :ratings])
        url     (if local?
                  (:local urls)
                  (:public urls))]
    (try+
     (-> url
         (str (name group))
         (query-history
          {:query-params
           (cond-> {:moves (get-in defaults [:history :moves])
                    :topGames (get-in defaults [:history :top-games])
                    :fen      (not/ucis->fen ucis)}
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
         web/from-json
         :moves
         process-candidates)
     (catch [:status 429] _
       (log/info "Hit the move history rate limit. Waiting one minute before resuming requests.")
       (Thread/sleep 60000)
       (historic-moves opts))
     (catch Object _
       (log/error (:throwable &throw-context) "error for moves: " ucis)
       (throw+)))))

(comment
  (historic-moves
   {:group :lichess
    :color :white
    :ucis ["e2e4"]
    :local? true}))
