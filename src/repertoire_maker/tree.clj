(ns repertoire-maker.tree
  (:require
   [clojure.set :as set]
   [taoensso.timbre :as log]
   [repertoire-maker.engine :as ngn]
   [repertoire-maker.history :as h]
   [repertoire-maker.util.core :as util]
   [repertoire-maker.util.notation :as not]
   [repertoire-maker.default :refer [defaults]]
   [flatland.ordered.map :refer [ordered-map]]))

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

(defn get-leaves
  "get all the leaf nodes of the tree"
  [tree]
  (->> tree
       :responses
       (mapcat
        (fn [[_ {:keys [responses] :as node}]]
          (if (seq responses)
            (get-leaves responses)
            [node])))))

(defn get-in-tree
  "Get a branch in the tree by uci"
  [tree ucis]
  (if (seq ucis)
    (get-in
     tree
     (-> :responses
         (interpose ucis)
         (conj :responses)
         vec))
    tree))

(defn resp-in-tree
  "Get a branch in the tree by uci"
  [tree ucis]
  (if (seq ucis)
    (get-in
     tree
     (-> :responses
         (interpose ucis)
         (conj :responses)
         vec
         (conj :responses)))
    (get tree :responses)))

(defn dissoc-in-tree
  [tree ucis]
  (let [base (drop-last ucis)
        tip  (last ucis)]
    (if (seq base)
      (update-in
       tree
       (-> :responses
           (interpose base)
           (conj :responses)
           vec
           (conj :responses))
       #(dissoc % tip))
      (update tree :responses #(dissoc % tip)))))

(defn assoc-tree-branch
  "Insert a branch into the move tree"
  ([node]
   (assoc-tree-branch nil node))
  ([tree {:keys [ucis stack] :as node}]
   (let [stack (or stack ucis)
         tree (or tree {:responses (ordered-map)})
         node (-> node
                  (assoc :stack stack)
                  (update :responses #(or % (ordered-map))))]
     (cond
       (empty? ucis)
       (-> node
           (dissoc :stack)
           (merge tree))

       (= 1 (count stack))
       (assoc-in tree [:responses (first stack)] (dissoc node :stack))

       (seq stack)
       (update-in
        tree
        [:responses (first stack)]
        #(assoc-tree-branch % (update node :stack rest)))

       :else
       tree))))

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
  [{:keys [ucis node] :as opts}]
  (->> (assoc opts :moves ucis)
       ngn/prepare-engine-candidates
       (filter #(= node (:uci %)))
       first
       :score))

(defn init-move-eval
  [{:keys [color ucis node prob-agg masters?] :as opts}]
  (let [opts (assoc opts :moves ucis)

        move-eval
        (->> (assoc opts :group :lichess)
             h/moves->candidates
             (filter #(= node (:uci %)))
             first)

        masters-eval (when masters?
                       (->> (assoc opts :group :masters)
                            h/moves->candidates
                            (filter #(= node (:uci %)))
                            first))

        move-eval (-> move-eval
                      (assoc :white-m (:white masters-eval))
                      (assoc :black-m (:black masters-eval))
                      (assoc :prob-m  (:prob  masters-eval)))

        prob-agg (cond-> prob-agg
                   (not (util/my-turn? color ucis))
                   (* (:prob move-eval)))]

    (-> move-eval
        (merge
         {:ucis    (conj ucis node)
          :score    (init-score opts)
          :prob-agg prob-agg}))))

;; TODO
;; basically this should grab the analysis of the starting move
;; and jam it into the tree, as well as creating the correct
;; starting stack for the situation
(defn starting-state
  [{:keys [moves] :as opts}]
  (let [{:keys [ucis color] :as opts}
        (-> opts
            (assoc  :ucis (not/sans->ucis moves))
            (update :overrides overrides->uci)
            (dissoc :moves))

        stack
        (if (util/my-turn? color ucis)
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

        ;; FIXME: outer node has metadata, inner node is UCI
        node
        (reduce
         (fn [{:keys [prob-agg ucis]} node]
           (-> opts
               (merge
                {:ucis    ucis
                 :prob-agg prob-agg
                 :node     node})
               init-move-eval))
         (base-node color)
         ucis)]

    (merge
     opts
     {:stack stack
      :tree  (assoc-tree-branch node)})))

(defn- extract-filtered-moves
  [opts candidates]
  (let [best-score (->> candidates first :score)
        allowable-loss (or (:allowable-loss opts)
                           (get-in defaults [:engine :allowable-loss]))]
    (->> candidates
         (filter #(< allowable-loss (/ (:score %) best-score)))
         (mapv :uci))))

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

(defn- prepare-masters-candidates
  [{:keys [masters? min-total-masters]
    :or   {min-total-masters (get-in defaults [:algo :min-total-masters])}
    :as opts}]
  (when masters?
    (let [candidates
          (h/moves->candidates
           (assoc opts :group :masters))

          total-plays (->> candidates
                           (map :play-count)
                           (reduce +))]
      (when (> total-plays min-total-masters)
        (map
         #(set/rename-keys % {:white :white-m :black :black-m})
         candidates)))))

(defn- prepare-player-move
  [{:keys [player] :as opts} engine-filter]
  (when player
    (some->> (assoc opts :group :player)
             h/moves->candidates
             engine-filter
             first
             :uci)))

(defn- get-candidate
  [candidates move]
  (->> candidates
       (filter #(= (:uci move) (:uci %)))
       first))

(defn- agg-stat
  [stat]
  (keyword (str (name stat) "-agg")))

(defn- m-stat
  [stat]
  (keyword (str (name stat) "-m")))

(defn- init-agg-stats
  [node]
  (let [stats [:white :black :score :white-m :black-m]]
    (reduce
     (fn [node stat]
       (assoc node (agg-stat stat) (get node stat)))
     node
     stats)))

;; TODO
(defn enumerate-candidates
  "enumerate all move candidates"
  [{:keys [min-plays min-prob search-depth overrides step tree stack]
    :or   {min-plays    (get-in defaults [:algo :min-plays])
           min-prob     (get-in defaults [:algo :min-prob])
           search-depth (get-in defaults [:algo :search-depth])}
    :as   opts}]

  (let [{:keys [ucis depth]} step
        depth                (inc depth)
        {:keys [prob-agg]}   (get-in-tree tree ucis)
        opts                 (assoc opts :moves ucis)
        engine-candidates    (ngn/prepare-engine-candidates opts)
        engine-filter        (filter-engine opts engine-candidates)
        player-move          (prepare-player-move opts engine-filter)
        overridden-move      (get overrides ucis)
        lichess-candidates   (delay (-> opts
                                        (assoc :group :lichess)
                                        h/moves->candidates))
        masters-candidates   (prepare-masters-candidates opts)
        candidates           (or masters-candidates
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
               (filter #(< min-prob (:prob %)))
               engine-filter))

        candidates
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
                           :score)})
         candidates)

        ;; CLEAN - these candidates are being correctly appended
        ;; _ (when (= ucis ["g1f3" "d7d5"])
        ;;     (println "PRE-APPEND-TREE" tree)
        ;;     (println "CANDIDATES: " candidates))

        tree (->> candidates
                  ;; init agg stats to the same as the nominal stats
                  (map init-agg-stats)
                  (reduce assoc-tree-branch tree))

        ;; CLEAN
        ;; _ (when (= ucis ["g1f3" "d7d5"])
        ;;     (println "POST-APPEND-TREE" tree))

        ;; per candidate (if depth < search-depth)
        ;; Responses -> CalcStat -> stack
        stack (if (< depth search-depth)
                (->> candidates
                     (reduce
                      (fn [s c]
                        (-> s
                            (conj {:action :calc-stats
                                   :ucis   (:ucis c)})
                            (conj {:action :responses
                                   :ucis   (:ucis c)
                                   :depth  depth})))
                      stack))
                stack)]

    (-> opts
        (assoc :tree tree)
        (assoc :stack stack))))

;; TODO: test
(defn enumerate-responses
  "enumerate all opponent responses"
  [{:keys [min-prob step tree stack]
    :or   {min-prob (get-in defaults [:algo :min-prob])}
    :as opts}]
  (let [{:keys [ucis depth]} step
        {:keys [prob-agg]} (get-in-tree tree ucis)

        masters-responses
        (prepare-masters-candidates opts)

        responses
        (->> (-> opts
                 (assoc :group :lichess)
                 (assoc :moves ucis))
             h/moves->candidates
             (filter #(< min-prob (:prob %)))
             (map (fn [move]
                    (merge move
                           {:ucis (conj ucis (:uci move))
                            :prob-m (:prob (get-candidate masters-responses move))
                            :prob-agg (* prob-agg (:prob move))}))))


        ;; tree is updated with responses as in expand-moves
        ;; don't worry about stats, we just need prob
        ;; always created
        tree (reduce assoc-tree-branch tree responses)

        ;; per response
        ;; Candidates -> TransStat -> stack
        stack (->> responses
                   reverse ; prioritize the most common responses
                   (reduce
                    (fn [s r]
                      (-> s
                          (conj {:action :trans-stats
                                 :ucis   (:ucis r)})
                          (conj {:action :candidates
                                 :ucis   (:ucis r)
                                 :depth  depth})))
                    stack))]
    (-> opts
        (assoc :tree tree)
        (assoc :stack stack))))

(defmulti apply-strategy
  "Apply a strategy to choose moves"
  :strategy)

(defmethod apply-strategy :default
  [opts]
  (-> opts
      (assoc :strategy :min-loss)
      apply-strategy))

(defmethod apply-strategy :min-loss
  [{:keys [color children]}]
  ;; loss is the opposite color
  (let [color ({:black :white :white :black} color)]
    (->> children
         (sort-by
          (fn [[_ node]]
            (or
             (get node (agg-stat (m-stat color)))
             (get node (agg-stat color)))))
         ffirst)))

(defmethod apply-strategy :max-win-over-loss
  [{:keys [color children]}]
  (let [opp-color ({:black :white :white :black} color)]
    (->> children
         (sort-by
          (fn [[_ node]]
            (or
             (some->
              (get node (agg-stat (m-stat color)))
              (/ (get node (agg-stat (m-stat opp-color)))))
             (/ (get node (agg-stat color))
                (get node (agg-stat opp-color)))))
          >)
         ffirst)))


(def alternate-stats
  {:calc-stats  :trans-stats
   :trans-stats :calc-stats})


(defn- do-prune-hooks
  [action]
  (fn [[_ {:keys [ucis responses]}]]
    ;; alternate stat pushes to stack
    ;; if no responses, then push responses!
    (into
     [{:action action
       :ucis   ucis}]
     (mapcat (do-prune-hooks (alternate-stats action)) responses))))

(defn prune-hooks
  [[_ {:keys [ucis responses]}]]
  (into
   ;; for each filtered child
   ;; Prune -> stack
   ;; TransStat -> stack
   [{:action :prune
     :ucis   ucis}
    {:action :trans-stats
     :ucis   ucis}]
   (mapcat (do-prune-hooks :calc-stats) responses)))

(defn create-prune-hooks
  [{:keys [min-prob step tree stack]
    :or   {min-prob (get-in defaults [:algo :min-prob])}
    :as   opts}]
  (let   [{:keys [ucis]} step
          viable-responses
          (->> ucis
               (get-in-tree tree)
               :responses
               (filter #(< min-prob (:prob-agg (second %))))
               ;; push the most common last
               reverse)]
    (assoc opts :stack (into stack (mapcat prune-hooks viable-responses)))))

(defn prune-tree
  [{:keys [step tree stack] :as opts}]
  (let [{:keys [ucis]} step

        node       (get-in-tree tree ucis)
        children   (:responses node)
        choice-uci (-> opts
                       (assoc :children children)
                       apply-strategy)

        tree (->> children
                  (remove #(= (first %) choice-uci))
                  (map #(:ucis (second %)))
                  (reduce dissoc-in-tree tree))

        ucis (conj ucis choice-uci)

        ;; push to the stack CalcStat for the chosen candidate
        stack (-> stack
                  (conj {:action :calc-stats
                         :ucis   ucis})
                  (conj {:action :create-prune-hooks
                         :ucis   ucis})
                  (conj {:action :responses
                         :ucis   ucis
                         :depth  0}))]

    (log/info "Pruned tree to " ucis)

    (-> opts
        (assoc :tree tree)
        (assoc :stack stack))))

;; TODO
;; * per response
(defn init-responses
  [{:keys [min-prob step tree stack]
    :or   {min-prob (get-in defaults [:algo :min-prob])}
    :as opts}]
  (let [{:keys [ucis]} step
        {:keys [prob-agg]} (get-in-tree tree ucis)

        responses
        (->> (-> opts
                 (assoc :group :lichess)
                 (assoc :moves ucis))
             h/moves->candidates
             (filter #(< min-prob (:prob %)))
             (map (fn [move]
                    (merge move
                           {:ucis (conj ucis (:uci move))
                            :prob-agg (* prob-agg (:prob move))}))))

        tree (reduce assoc-tree-branch tree responses)

        ;; for each response
        ;; Candidates -> Prune -> TransStat -> stack
        stack (->> responses
                   reverse ; prioritize the most common responses
                   (reduce
                    (fn [s r]
                      (-> s
                          (conj {:action :trans-stats
                                 :ucis   (:ucis r)})
                          (conj {:action :prune
                                 :ucis   (:ucis r)})
                          (conj {:action :candidates
                                 :ucis   (:ucis r)
                                 :depth  0})))
                    stack))]

    (-> opts
        (assoc :tree tree)
        (assoc :stack stack))))

(defn do-trans-stats
  [tree ucis]
  (let [children (resp-in-tree tree ucis)]
    (fn [node stat]
      (some->> children
           (map #(get-in % [1 (agg-stat stat)]))
           (filter some?)
           seq
           (apply max)
           (assoc node (agg-stat stat))))))

;; FIXME: culprit for screwing up the tree
;; replaces the responses with an empty ordered map...
;;probably an issue with `do-trans-stats`
(defn transfer-stats
  "bubble best child stat up to parent"
  [{:keys [step tree] :as opts}]
  (let [{:keys [ucis]} step
        node
        (reduce
         (do-trans-stats tree ucis)
         (get-in-tree tree ucis) ; is this what I think it is?
         [:white :black :score :white-m :black-m])]
    (update opts :tree assoc-tree-branch node)))

(defn- prob-attr
  [stat]
  (if (#{:white-m :black-m} stat)
    :prob-m
    :prob))

(defn- children-total
  [stat children]
  (->> children
       (map second)
       ;; FIXME currently fails on vanilla masters stats at last calc
       (map #(* (or 0 (get % stat))
                (or 0 ((prob-attr stat) %))))
       (reduce +)))

(defn do-calc-stats
  [tree ucis]
  (let [children (resp-in-tree tree ucis)]
    (fn [node stat]
      (let [nominal (get node stat)
            prob-non-child
            (- 1 (->> children (map #(or (get-in % [1 (prob-attr stat)]) 0)) (reduce +)))
            children-nominal (when (not= stat :score)
                               (children-total stat children))
            children-aggregate (children-total (agg-stat stat) children)

            calced
            (when (and (some? nominal)
                       (some? prob-non-child)
                       (some? children-nominal)
                       (some? children-aggregate))
                (+
                 (if (= stat :score)
                   (* prob-non-child nominal)
                   (- nominal children-nominal))
                 children-aggregate))]
        (assoc
         node
         (agg-stat stat)
         calced)))))

(defn calc-stats
  "weight parent stats based on children"
  [{:keys [step tree] :as opts}]
  (let [{:keys [ucis]} step
        node
        (reduce
         (do-calc-stats tree ucis)
         (get-in-tree tree ucis)
         [:white :black :white-m :black-m :score])]
    (update opts :tree assoc-tree-branch node)))

(defn build-tree
  [opts]
  (loop [{:keys [stack tree] :as opts} (starting-state opts)]
    (if (empty? stack)
      tree
      (let [{:keys [action] :as step}
            (peek stack)

            _ (when true
                (log/info (take 5 stack))
                #_#_
                (println "tree")
                (println tree))

            opts (-> opts
                     (update :stack pop)
                     (assoc  :step  step))]
        (case action
          :candidates
          (recur
           (enumerate-candidates opts))

          :responses
          (recur
           (enumerate-responses opts))

          :calc-stats
          (recur
           (calc-stats opts))

          :trans-stats
          (recur
           (transfer-stats opts))

          :init-responses
          (recur
           (init-responses opts))

          :prune
          (recur
           (prune-tree opts))

          :create-prune-hooks
          (recur
           (create-prune-hooks opts)))))))

(comment

  (build-tree
   {:allowable-loss  0.05
    :color           :white
    :min-prob        0.1
    :move-choice-pct 0.01
    :use-engine?     true
    :log-stats?      true
    :export?         true
    :strategy        :min-loss
    :search-depth    1
    :moves           []
    :masters?        true})



  )
