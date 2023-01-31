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

(defn- do-query-aggregate-stats
  [url params]
  (http/get url params))

(def query-aggregate-stats
  (memoize do-query-aggregate-stats))

(defn aggregate-stats
  [{:keys [group ucis]}]
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
     process-candidates)))

(comment
  (aggregate-stats
   {:group :lichess
    :color :white
    :ucis  ["e2e4"]}))
