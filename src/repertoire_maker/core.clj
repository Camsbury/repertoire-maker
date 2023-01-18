(ns repertoire-maker.core
  (:require
   [taoensso.timbre :as log]
   [repertoire-maker.build :refer [expand-movesets choose-moves]]
   [repertoire-maker.export :as export]
   [repertoire-maker.init :refer [init-opts]]
   [repertoire-maker.stat :as stat]
   [repertoire-maker.util.core :as util]))

(defn- my-turn?
  [{:keys [movesets color]}]
  (->> movesets
       first
       :moves
       util/whose-turn?
       (= color)))

(defn- build-step
  [opts]
  ((if (my-turn? opts)
     choose-moves
     expand-movesets)
   opts))

(defn build-repertoire
  "Build a tree of moves and their attributes corresponding to
  an opening stratategy based on the passed options.

  Conditionally runs stats and exports as PGN"
  [{:keys [log-stats? export? export-path] :as opts}]
  (let [move-tree
        (loop [opts (init-opts opts)]
          (if (empty? (:movesets opts))
            (:tree opts)
            (recur (build-step opts))))]
    (cond-> move-tree
      log-stats?
      stat/log-stats
      export?
      (export/export-repertoire export-path))))

(comment

  (def overrides
    {["e4" "e5"]                       "Nf3"
     ["e4" "c5"]                       "Nf3"
     ["e4" "e6"]                       "d4"
     ["e4" "c5" "Nf3" "d6"]            "d4"
     ["d4" "f5"]                       "e4"
     ["d4" "b6"]                       "e4"
     ["d4" "d5" "c4" "c6" "Nf3" "Nf6"] "Qc2"})

  (def ruy-lopez
    (let [opts
          {:allowable-loss  0.1
           :color           :white
           :filter-pct      0.01
           :move-choice-pct 0.01
           :moves           ["e4" "e5" "Nf3" "Nc6" "Bb5"]
           :use-engine?     true
           :log-stats?      true
           :export?         true
           #_#_
           :local?          true
           :masters?        true
           #_#_
           :overrides       overrides
           #_#_
           :player          "JackSilver"}]
      (build-repertoire opts)))

  (let [opts
        {:allowable-loss  0.1
         :color           :white
         :filter-pct      0.001
         :move-choice-pct 0.01
         :use-engine?     true
         :log-stats?      true
         :export?         true
         :masters?        true
         #_#_
         :local?          true}]
    (map #(build-repertoire (assoc opts :moves %))
         [["e4" "e5" "Nf3" "Nc6" "Bb5"]
          ["e4" "e5" "Nf3" "Nc6" "Bc4"]
          ["e4" "e5" "Nf3" "Nc6" "d4"]
          ["e4" "e5" "Nf3" "Nc6" "Nc3"]]))

  )
