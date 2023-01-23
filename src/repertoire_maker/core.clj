(ns repertoire-maker.core
  (:require
   [taoensso.timbre :as log]
   [repertoire-maker.action.core :refer [run-action]]
   [repertoire-maker.engine :as ngn]
   [repertoire-maker.history :as h]
   [repertoire-maker.notation :as not]
   [repertoire-maker.tree :as t]
   [repertoire-maker.export :as export]))

(defn- my-turn? [color moves]
  (= color
     (if (zero? (mod (count moves) 2))
       :white
       :black)))

(defn base-node
  "Numbers based on lichess and engine cache"
  [color]
  (let [score
        (if (= :white color)
          0.531
          0.484)]
    {:white-m  0.33
     :black-m  0.24
     :white    0.49
     :black    0.45
     :score    score
     :prob     1.0
     :prob-m   1.0
     :prob-agg 1.0
     :ucis     []}))

(defn- overrides->uci
  [overrides]
  (->> overrides
       (map
        (fn [[base tip]]
          (let [ucis (not/sans->ucis (conj base tip))
                base (into [] (drop-last ucis))
                tip (last ucis)]
            [base tip])))
       (into {})))

(defn init-score
  [{:keys [node] :as opts}]
  (->> opts
       ngn/prepare-engine-candidates
       (filter #(= node (:uci %)))
       first
       :score))

(defn init-move-eval
  [{:keys [color ucis uci prob-agg masters?] :as opts}]
  (let [move-eval
        (->> (assoc opts :group :lichess)
             h/historic-moves
             (filter #(= uci (:uci %)))
             first)

        masters-eval (when masters?
                       (->> (assoc opts :group :masters)
                            h/historic-moves
                            (filter #(= uci (:uci %)))
                            first))

        move-eval (-> move-eval
                      (assoc :white-m (:white masters-eval))
                      (assoc :black-m (:black masters-eval))
                      (assoc :prob-m  (:prob  masters-eval)))

        prob-agg (cond-> prob-agg
                   (not (my-turn? color ucis))
                   (* (:prob move-eval)))]

    (-> move-eval
        (merge
         {:ucis    (conj ucis uci)
          :score    (init-score opts)
          :prob-agg prob-agg}))))

(defn starting-state
  [{:keys [moves] :as opts}]
  (let [{:keys [ucis color] :as opts}
        (-> opts
            (assoc  :ucis (not/sans->ucis moves))
            (update :overrides overrides->uci)
            (dissoc :moves))

        stack
        (if (my-turn? color ucis)
          (list
           {:action :candidates
            :ucis   ucis
            :depth  0}
           {:action :prune
            :ucis   ucis}
           {:action :trans-stats
            :ucis   ucis})
          (list
           {:action :init-responses
            :ucis   ucis}
           {:action :calc-stats
            :ucis   ucis}))

        node
        (reduce
         (fn [{:keys [prob-agg ucis]} uci]
           (-> opts
               (merge
                {:ucis     ucis
                 :prob-agg prob-agg
                 :uci      uci})
               init-move-eval))
         (base-node color)
         ucis)]

    (merge
     opts
     {:stack stack
      :tree  (t/assoc-tree-branch node)})))

(defn build-tree
  [opts]
  (loop [{:keys [stack tree] :as opts} (starting-state opts)]
    (if (empty? stack)
      tree
      (let [{:keys [action] :as step}
            (peek stack)

            #_#_
            _ (when true
                ;; (log/info (take 5 stack))
                (log/info "step: " step)
                (println "tree")
                (println tree))

            opts (-> opts
                     (update :stack pop)
                     (assoc  :step  step)
                     (assoc  :action action))]
        (recur (run-action opts))))))

(defn build-repertoire
  "Build a tree of moves and their attributes corresponding to
  an opening stratategy based on the passed options.

  Conditionally runs stats and exports as PGN"
  [{:keys [log-stats? export? export-path] :as opts}]
  (cond-> (build-tree opts)
    #_#_
    log-stats?
    stat/log-stats
    export?
    (export/export-repertoire export-path)))

(comment
  (build-repertoire
   {:allowable-loss 0.05
    :color          :white
    ;; :moves          ["e4"]
    :min-prob-agg   0.1
    :min-resp-prob  0.05
    :min-cand-prob  0.05
    :use-engine?    true
    :export?        true
    :strategy       :max-win-over-loss
    :search-depth   1
    :masters?       true}))
