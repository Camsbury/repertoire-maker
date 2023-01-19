(ns repertoire-maker.history
  (:require
   [taoensso.timbre :as log]
   [repertoire-maker.util.notation :as not]
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
  [total-count {:keys [uci white draws black] :as option}]
  (let [total (total-option option)
        white (float (/ white total))
        black (float (/ black total))]
    {:uci        uci
     :white      white
     :black      black
     :play-count total
     :play-pct   (float (/ total total-count))}))

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

(defn moves->candidates
  [{:keys [group since color moves player local?]
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
                    :fen      (not/ucis->fen moves)}
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
       (moves->candidates opts))
     (catch Object _
       (log/error (:throwable &throw-context) "error for moves: " moves)
       (throw+)))))


(comment
  (-> :public
      urls
      (str "lichess")
      (query-history
       {:query-params
        {:moves       30
         :topGames    0
         :play        "e2e4,c7c5,g1f3"
         :recentGames 0
         :speeds      "bullet,blitz,rapid"
         :ratings     "2000,2200,2500"}})
      :body
      web/from-json
      :moves
      process-candidates)
  )
