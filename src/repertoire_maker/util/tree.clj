(ns repertoire-maker.util.tree
  (:require
   [flatland.ordered.map  :refer [ordered-map]]))

(defn add-tree-branch
  ([moveset]
   (add-tree-branch nil moveset))
  ([tree {:keys [moves] :as move}]
   (let [tree (or tree (ordered-map))]
     (cond
       (= 1 (count moves))
       (assoc tree (first moves) (dissoc move :moves))

       (seq moves)
       (update-in
        tree
        [(first moves) :responses]
        #(add-tree-branch % (update move :moves rest)))

       :else
       tree))))
