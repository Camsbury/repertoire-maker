(ns repertoire-maker.tree
  (:require
   [python-base]
   [libpython-clj2.python :refer [py. py.-]]
   [libpython-clj2.require :refer [require-python]]
   [repertoire-maker.util.core :as util]
   [flatland.ordered.map :refer [ordered-map]]))

(require-python
 '[chess :as chess])

;; params of search-depth, branching-factor

(def base-node
  "Numbers based on lichess and engine cache"
  {:white 0.49
   :black 0.45
   :score 0.53
   :prob  1.0
   :ucis  []})

(defn- starting-stack
  [color node]
  (list
   (if (util/my-turn? color (:ucis node))
     {:action :candidates
      :node   node
      :depth  0}
     {:action :responses
      :node   node
      :depth  0})))

(defn enumerate-candidates
  [opts step])

(defn enumerate-responses
  [opts step])

(defn calculate-stats
  [opts {:keys [node]} tree]
   )

(defn build-tree
  [{:keys [node tree color]
    :or   {node {:ucis []}
           tree (ordered-map)}
    :as   opts}]
  (loop [stack (starting-stack color node)
         tree  tree]
    (if (empty? stack)
      tree
      (let [{:keys [action] :as step}
            (first stack)

            stack (rest stack)]
        (case action
          :candidates
          (let [{:keys [stack tree]}
                (enumerate-candidates opts step)]
            (recur stack tree))

          :responses
          (let [{:keys [stack tree]}
                (enumerate-responses opts step)]
            (recur stack tree))

          :stats
          (recur stack (calculate-stats opts step tree)))))))

(comment



  )
