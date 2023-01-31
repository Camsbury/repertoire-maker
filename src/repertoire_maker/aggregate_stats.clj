(ns repertoire-maker.aggregate-stats
  (:require
   [taoensso.timbre :as log]
   [repertoire-maker.notation :as not]
   [clj-http.client :as http]
   [clojure.string :as str]
   [repertoire-maker.util.web :as web]
   [slingshot.slingshot :refer [try+ throw+]]))

(def urls
  {:lichess "http://localhost:9090"})

(defn- do-query-aggregate-stats
  [url params]
  (http/get url params))

(def query-aggregate-stats
  (memoize do-query-aggregate-stats))

(defn aggregate-stats
  [{:keys [group color ucis] :as opts}]
  (let [url (get urls group)]
    (->>
     (-> url
         (query-aggregate-stats
          {:query-params
           {:fen (not/ucis->fen ucis)}})
         :body
         web/from-json
         :game-moves)
     (map
      (fn [[k v]]
        (-> v
            (assoc :uci (str/replace (name k) #"-" ""))
            (assoc :draws (:draw v))
            (dissoc :draw))))
     (sort-by
      (fn [{:keys [black white draws]}]
        (* -1 (+ black white draws))))
     (into []))))

(comment
  (aggregate-stats
   {:group :lichess
    :color :white
    :ucis  ["e2e4"]}))
