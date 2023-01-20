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
(def breadth
  "The standard for lichess"
  5)

(defn- uci-and-score
  [{:keys [moves] :as pv}]
  {:uci (first (str/split moves #" "))
   :score (score/standardize-score pv)})

(defn- parse-cloud-eval
  [{:keys [depth pvs]}]
  {:depth depth
   :candidates
   (->> pvs
        (map uci-and-score)
        (sort-by :score >)
        (into []))})

(defn- do-get-cloud-eval
  [url params]
  (http/get url params))

(def get-cloud-eval
  (memoize do-get-cloud-eval))

(defn fen->cloud-eval
  [fen]
  (try+
   (->
    (get-cloud-eval
     url
     {:query-params
      {:fen fen
       :multiPv breadth}})
    :body
    web/from-json
    parse-cloud-eval)
   (catch [:status 429] _
     ;; NOTE: could just switch to local eval to save time
     (log/info "Hit the cloud eval rate limit. Waiting one minute before resuming requests.")
     (Thread/sleep 60000)
     (fen->cloud-eval fen))
   (catch [:status 404] _
     (log/debug (str "Cloud eval for fen: " fen " is unavailable"))
     nil)
   (catch Object _
     (log/error (:throwable &throw-context) "error for fen: " fen)
     (throw+))))


(comment
  (->
   (get-cloud-eval
    "https://lichess.org/api/cloud-eval"
    {:query-params
     {:fen "rnbqkb1r/pp2pppp/3p1n2/8/3QP3/2N2N2/PPP2PPP/R1B1KB1R b KQkq - 0 5"
      ;; 5 is what lichess caches deeply
      :multiPv 5}})
   :body
   web/from-json))
