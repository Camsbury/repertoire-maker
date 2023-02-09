(ns repertoire-maker.action.candidates
  (:require
   [malli.core :as m]
   [taoensso.timbre :as log]
   [repertoire-maker.candidate :refer [get-candidate prepare-masters-candidates]]
   [repertoire-maker.stat :refer [agg-stat]]
   [repertoire-maker.action.multi :refer [run-action]]
   [repertoire-maker.default :refer [defaults]]
   [repertoire-maker.engine :as ngn]
   [repertoire-maker.history :as h]
   [repertoire-maker.schema :as schema]
   [repertoire-maker.strategy :refer [strategy->sort-fn]]
   [repertoire-maker.tree :as t]))

(defn- extract-filtered-moves
  [{:keys [allowable-loss]} candidates]
  (let [best-score (->> candidates first :score)
        allowable-loss (or allowable-loss
                           (get-in defaults [:engine :allowable-loss]))]
    (->> candidates
         (filter #(< allowable-loss (/ (:score %) best-score)))
         (mapv :uci))))
(m/=>
 extract-filtered-moves
 [:=>
  [:cat
   schema/config-opts
   [:sequential schema/move-node]]
  [:sequential schema/move-node]])

(defn- filter-engine
  [opts engine-candidates]
  (fn [move-candidates]
    (if-let
        [engine-candidates
         (->> engine-candidates
              (extract-filtered-moves opts)
              seq)]
      (filter
       #(contains? (set engine-candidates) (:uci %))
       move-candidates)
      move-candidates)))
(m/=>
 filter-engine
 [:=>
  [:cat
   schema/config-opts
   [:sequential schema/move-node]]
  [:=>
   [:cat
    [:sequential schema/move-node]]
   [:sequential schema/move-node]]])

(defn- prepare-player-move
  [{:keys [player] :as opts} engine-filter]
  (when player
    (some->> (assoc opts :group :player)
             h/historic-moves
             engine-filter
             first
             :uci)))
(m/=>
 prepare-player-move
 [:=>
  [:cat
   schema/config-opts
   [:=>
    [:cat
     [:sequential schema/move-node]]
    [:sequential schema/move-node]]]
  [:maybe schema/uci]])


(defn- init-agg-stats
  [node]
  (let [stats [:white :black :score :white-m :black-m]]
    (reduce
     (fn [node stat]
       (assoc node (agg-stat stat) (get node stat)))
     node
     stats)))
(m/=>
 init-agg-stats
 [:=>
  [:cat schema/move-node]
  schema/move-node])

(defn enumerate-candidates
  "Enumerate candidates for a given move sequence"
  [{:keys [min-plays
           min-cand-prob
           max-cand-breadth
           overrides
           search-depth
           stack
           step
           tree]
    :or   {min-plays        (get-in defaults [:algo :min-plays])
           min-cand-prob    (get-in defaults [:algo :min-cand-prob])
           max-cand-breadth (get-in defaults [:algo :max-cand-breadth])
           search-depth     (get-in defaults [:algo :search-depth])}
    :as   opts}]
  (let [{:keys [ucis cons-prob depth pruned?]} step
        depth                            (inc depth)
        {:keys [prob-agg]}               (t/get-in-tree tree ucis)
        opts                             (assoc opts :ucis ucis)
        engine-candidates                (ngn/prepare-engine-candidates opts)
        engine-filter                    (filter-engine opts engine-candidates)
        player-move                      (prepare-player-move opts engine-filter)
        overridden-move                  (get overrides ucis)

        lichess-candidates (delay (-> opts
                                      (assoc :group :lichess)
                                      h/historic-moves))
        masters-candidates (prepare-masters-candidates opts)
        candidates         (or masters-candidates
                               @lichess-candidates)
        candidates
        (cond
          (some? overridden-move)
          (->> candidates
               (filter #(= overridden-move (:uci %))))
          (some? player-move)
          (->> candidates
               engine-filter
               (filter #(= player-move (:uci %))))
          :else
          (->> candidates
               (filter #(< min-plays (:play-count %)))
               (filter #(< min-cand-prob (:prob %)))
               engine-filter))


        candidates (if (seq candidates)
                     candidates
                     (->> engine-candidates engine-filter))

        candidates
        (->> candidates
             (map
              #(merge
                %
                {:prob-agg prob-agg
                 :ucis     (conj ucis (:uci %))
                 :white    (->> %
                                (get-candidate @lichess-candidates)
                                :white)
                 :black    (->> %
                                (get-candidate @lichess-candidates)
                                :black)
                 :prob     (->> %
                                (get-candidate @lichess-candidates)
                                :prob)
                 :white-m  (->> %
                                (get-candidate masters-candidates)
                                :white-m)
                 :black-m  (->> %
                                (get-candidate masters-candidates)
                                :black-m)
                 :prob-m   (->> %
                                (get-candidate masters-candidates)
                                :prob)
                 :score    (->> %
                                (get-candidate engine-candidates)
                                :score)}))
             (sort-by (strategy->sort-fn opts))
             (take max-cand-breadth))

        tree (->> candidates
                  ;; init agg stats to the same as the nominal stats
                  (map init-agg-stats)
                  (reduce t/assoc-tree-branch tree))

        _ (println "Looking at the following candidates following " ucis ":\n " (map :uci candidates))

        stack
        (if (or
             (and pruned? (= 1 (count candidates)))
             (= depth search-depth))
          stack
          (->> candidates
               (reduce
                (fn [s c]
                  (-> s
                      (conj {:action :calc-stats
                             :ucis   (:ucis c)})
                      (conj {:action    :responses
                             :ucis      (:ucis c)
                             :cons-prob cons-prob
                             :depth     depth})))
                stack)))]
    (-> opts
        (assoc :tree tree)
        (assoc :stack stack))))
(m/=>
 enumerate-candidates
 [:=>
  [:cat
   [:and
    schema/build-tree-opts
    schema/config-opts
    [:map [:step schema/build-step]]]]
  schema/build-tree-opts])

;; Enumerate all move candidates
(defmethod run-action :candidates
  [opts]
  (enumerate-candidates opts))
