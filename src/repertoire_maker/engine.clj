(ns repertoire-maker.engine
  (:require
   [python-base]
   [environ.core :refer [env]]
   [clojure.string :as str]
   [libpython-clj2.python :refer [py.] :as py]
   [libpython-clj2.require :refer [require-python]]))

(require-python
 '[chess        :as chess]
 '[chess.engine :as ngn])

(def stockfish-path (env :stockfish-path))

(defn color-score
  [score color]
  (if (= color :white)
    (py. score "white")
    (py. score "black")))

(defn parse-score
  [score]
  (-> score
      (str/replace #"\+" "")
      (Integer/parseInt)))

(defn uci-and-score
  [color option]
  {:uci (-> option
            (py/get-item "pv")
            first
            (py. "uci"))
   :score (-> option
              (py/get-item "score")
              (color-score color)
              parse-score)})

(defn extract-filtered-moves
  [allowable-loss options]
  (let [best-score (->> options first :score)]
    (->> options
         (filter #(> allowable-loss (- best-score (:score %))))
         (mapv :uci))))

(defn engine-moves
  [{:keys [moves depth m-count allowable-loss]}]
  (let [engine (py. ngn/SimpleEngine "popen_uci" stockfish-path)
        board  (chess/Board)
        _      (reduce
                (fn [_ move] (py. board "push" (py. chess/Move "from_uci" move)))
                nil
                moves)
        info   (py. engine
                    "analyse"
                    board
                    (ngn/Limit :depth depth)
                    :multipv m-count)
        moves (->> info
                   (map #(uci-and-score :white %))
                   (extract-filtered-moves allowable-loss))]
    (py. engine "quit")
    moves))

(comment

  (engine-moves
   {:moves ["e2e4" "e7e5"]
    :depth 5
    :m-count 10
    :allowable-loss 100})


  )
