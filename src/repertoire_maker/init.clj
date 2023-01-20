(ns repertoire-maker.init
  (:require
   [taoensso.timbre :as log]
   [repertoire-maker.engine :as ngn]
   [repertoire-maker.history :as h]
   [repertoire-maker.util.tree :as tree]
   [repertoire-maker.util.notation :as not]
   [repertoire-maker.util.core :as util]))

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
  [{:keys [stack move] :as opts}]
  (->> (assoc opts :moves stack)
       ngn/prepare-engine-candidates
       (filter #(= move (:uci %)))
       first
       :score))

;; TODO: rewrite to include masters, forget about "chosen"
(defn- init-move-eval
  [{:keys [stack move pct] :as opts}]
  (let [move-eval
        (->> (merge opts {:group :lichess :moves stack})
             h/moves->candidates
             (filter #(= move (:uci %)))
             first)

        pct (cond-> pct
              (not= (:color opts) (util/whose-turn? stack))
              (* (:play-pct move-eval)))]

    (-> move-eval
        (merge
         {:stack   (conj stack move)
          :score   (init-score opts)
          :chosen? (= (:color opts)
                      (util/whose-turn? stack))
          :pct     pct}))))

;; TODO: rewrite for the new tree module
(defn init-opts
  [opts]
  (let [{:keys [moves] :as opts}
        (-> opts
            (update :moves not/sans->ucis)
            (update :overrides overrides->uci))

        moveset
        (reduce
         (fn [{:keys [pct stack]} move]
           (-> opts
               (merge
                {:stack stack
                 :pct   pct
                 :move  move})
               init-move-eval))
         {:pct 1.0 :stack []}
         (:moves opts))

        moveset (assoc moveset :moves moves)]

    (merge
     opts
     {:tree (tree/add-tree-branch moveset)
      :movesets [moveset]})))
