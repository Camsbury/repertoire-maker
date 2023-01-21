(ns repertoire-maker.core
  (:require
   [taoensso.timbre :as log]
   [repertoire-maker.export :as export]
   [repertoire-maker.stat :as stat]
   [repertoire-maker.tree :as tree]))

(defn build-repertoire
  "Build a tree of moves and their attributes corresponding to
  an opening stratategy based on the passed options.

  Conditionally runs stats and exports as PGN"
  [{:keys [log-stats? export? export-path] :as opts}]
  (let [move-tree
        (tree/build-tree opts)
        #_
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

  (build-repertoire
   {:allowable-loss  0.05
    :color           :white
    :min-prob-agg        0.1
    :min-cand-prob 0.01
    :use-engine?     true
    :export?         true
    :strategy        :max-win-over-loss
    :search-depth    2
    :masters?        true})
  )
