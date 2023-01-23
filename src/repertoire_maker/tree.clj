(ns repertoire-maker.tree
  (:require
   [flatland.ordered.map  :refer [ordered-map]]))

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
