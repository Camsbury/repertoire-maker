(ns repertoire-maker.tree
  (:require
   [python-base]
   [taoensso.timbre :as log]
   [libpython-clj2.python :refer [py. py.-]]
   [libpython-clj2.require :refer [require-python]]
   [repertoire-maker.engine :as ngn]
   [repertoire-maker.history :as h]
   [repertoire-maker.util.core :as util]
   [repertoire-maker.util.notation :as not]
   [flatland.ordered.map :refer [ordered-map]]))

(require-python
 '[chess :as chess])

;; params of search-depth, branching-factor

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
  (get-in
   tree
   (-> :responses
       (interpose ucis)
       (conj :responses)
       vec)))

(defn resp-in-tree
  "Get a branch in the tree by uci"
  [tree ucis]
  (get-in
   tree
   (-> :responses
       (interpose ucis)
       (conj :responses)
       vec
       (conj :responses))))

(defn assoc-tree-branch
  "Insert a branch into the move tree"
  ([node]
   (assoc-tree-branch nil node))
  ([tree {:keys [ucis stack] :as node}]
   (let [stack (or stack ucis)
         tree (or tree {:responses (ordered-map)})
         node (-> node
                  (assoc :stack stack)
                  (assoc :responses (ordered-map)))]
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
                      (assoc :black-m (:black masters-eval)))

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

    (log/info "pre-tree node" node)
    {:stack stack
     :tree  (assoc-tree-branch node)}))

;; TODO
;; NOTE: increase depth by 1 before doing anything, then produce stack if ok
;; NOTE: push to stack [[Responses, CalcStat]*] - * per candidate
(defn enumerate-candidates
  "enumerate all move candidates"
  [opts])

;; TODO
;; NOTE: this should push to stack [[Candidates, TransStat]*]
(defn enumerate-responses
  "enumerate all opponent responses"
  [opts])

;; TODO
;; NOTE: this should push to stack [[Responses*, Score@, Prune*]]
;; * for all leaves that belong to this nodes responses that have > :prob
;; e.g. e4 has e5, c5, d6 and only e5 has > :prob, so we enumerate all leaves with responses
;; @ score backwards from leaves up to branches with > :prob (since those will be searched)
;; then prune all branches with > :prob
(defn prune-tree
  [opts])

;; TODO
;; NOTE: this should push to stack [[Candidates, Prune, TransStat]*]
;; * per response
(defn initialize-responses
  [opts])

(defn- agg-stat
  [stat]
  (keyword (str (name stat) "-agg")))

(defn do-trans-stats
  [tree ucis]
  (let [children (resp-in-tree tree ucis)]
    (fn [node stat]
      (->> children
           (map #(get % (agg-stat stat)))
           (apply max)
           (assoc node (agg-stat stat))))))

(defn transfer-stats
  "bubble best child stat up to parent"
  [{:keys [ucis tree] :as opts}]
  (let [node
        (reduce
         (do-trans-stats tree ucis)
         (get-in-tree tree ucis)
         [:white :black :score])]
    (update opts :tree assoc-tree-branch node)))

;; FIXME: need a fallback if nils in the children
;; (notably for masters win%s)
(defn- children-total
  [stat children]
  (->> children
       (map #(* (get % stat)
                (:prob %)))
       (reduce +)))

(defn do-calc-stats
  [tree ucis]
  (let [children (resp-in-tree tree ucis)]
    (fn [node stat]
      (assoc
       node
       (agg-stat stat)
       (+
        (if (= stat :score)
          (* (- 1 (->> children (map :prob) (reduce +)))
           (get node :score))
          (- (get node stat)
             (children-total stat children)))
        (children-total (agg-stat stat) children))))))

(defn calculate-stats
  "weight parent stats based on children"
  [{:keys [ucis tree] :as opts}]
  (let [node
        (reduce
         (do-calc-stats tree ucis)
         (get-in-tree tree ucis)
         [:white :black :score])]
    (update opts :tree assoc-tree-branch node)))

(defn build-tree
  [opts]
  (loop [{:keys [stack tree] :as opts} (starting-state opts)]
    (if (empty? stack)
      tree
      (let [{:keys [action] :as step}
            (peek stack)

            opts (-> opts
                     (update :stack pop)
                     (assoc  :step  step))]
        (case action
          :candidates
          (recur (enumerate-candidates opts))

          :responses
          (recur (enumerate-responses opts))

          :calc-stats
          (recur (calculate-stats step))

          :trans-stats
          (recur (transfer-stats step))

          :init-responses
          (recur (initialize-responses opts))

          :prune
          (recur (prune-tree opts)))))))

(comment

  (into '(1 2 3) '(4 5 6))


  )
