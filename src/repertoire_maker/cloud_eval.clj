(ns repertoire-maker.cloud-eval
  (:require
   [taoensso.timbre :as log]
   [repertoire-maker.score :as score]
   [repertoire-maker.util :as util]
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
   :moves (mapv uci-and-score pvs)})

(defn fen->cloud-eval
  [fen]
  (try+
   (->
    (http/get
     url
     {:query-params
      {:fen fen
       :multiPv breadth}})
    :body
    util/from-json
    parse-cloud-eval)
   (catch [:status 429] _
     ;; NOTE: could just switch to local eval to save time
     (log/info "Hit the cloud eval rate limit. Waiting one minute before resuming requests.")
     (Thread/sleep 60000)
     (fen->cloud-eval fen))
   (catch Object _
     (log/error (:throwable &throw-context) "error for fen: " fen)
     (throw+))))
