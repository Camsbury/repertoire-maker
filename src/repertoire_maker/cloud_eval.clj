(ns repertoire-maker.cloud-eval
  (:require
   [taoensso.timbre :as log]
   [repertoire-maker.score :as score]
   [repertoire-maker.util.web :as web]
   [slingshot.slingshot :refer [try+ throw+]]
   [clojure.string :as str]
   [clj-http.client :as http]))


(def url
  "https://lichess.org/api/cloud-eval")
(def lc-breadth
  "The standard for lichess"
  5)

(defn- uci-and-score
  [{:keys [moves] :as pv}]
  {:uci (first (str/split moves #" "))
   :score (score/standardize-score pv)})

(defn- flip-if-black
  [color]
  (if (= :black color)
    (fn [{:keys [cp mate] :as pv}]
      (merge pv {:cp (some->> cp (* -1)) :mate (some->> mate (* -1))}))
    identity))

(defn- parse-cloud-eval
  [{:keys [color depth pvs]}]
  {:depth depth
   :candidates
   (->> pvs
        (map (flip-if-black color))
        (map uci-and-score)
        (sort-by :score >)
        (into []))})

(defn- do-get-cloud-eval
  [url params]
  (http/get url params))

(def get-cloud-eval
  (memoize do-get-cloud-eval))

(defonce kill-switch
  (atom false))

(defn fen->cloud-eval
  [{:keys [fen color breadth] :as opts}]
  (when (not @kill-switch)
    (try+
     (->
      (get-cloud-eval
       url
       {:query-params
        {:fen fen
         :multiPv (or breadth lc-breadth)}})
      :body
      web/from-json
      (assoc :color color)
      parse-cloud-eval)
     (catch [:status 429] _
       ;; NOTE: could just switch to local eval to save time
       (log/info "Hit the cloud eval rate limit. Waiting one minute before resuming requests.")
       (Thread/sleep 60000)
       (fen->cloud-eval opts))
     (catch [:status 502] _
       ;; NOTE: could just switch to local eval to save time
       (log/info "Maybe hit the cloud eval rate limit?? status 502")
       (Thread/sleep 60000)
       (fen->cloud-eval opts))
     (catch [:status 404] _
       (log/debug (str "Cloud eval for fen: " fen " is unavailable"))
       nil)
     (catch Object _
       (log/info (:throwable &throw-context) "error for fen: " fen)
       (log/info "Stopping cache access and reverting to local engine")
       (reset! kill-switch true)
       nil))))


(comment
  (fen->cloud-eval
   {:fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    :color :white})
  )
