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
  (if (str/includes? score "#")
    (/ 1e6
       (-> score
           (str/replace #"#" "")
           (Integer/parseInt)))
    (-> score
        (str/replace #"\+" "")
        (Integer/parseInt))))

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

(defn moves->engine-options
  [{:keys [moves move-count depth allowable-loss hash threads]}]
  (let [engine (py. ngn/SimpleEngine "popen_uci" stockfish-path)
        _      (py. engine "configure" (py/->py-dict {"Hash"    hash
                                                      "Threads" threads}))
        board  (chess/Board)
        _      (reduce
                (fn [_ move] (py. board "push" (py. chess/Move "from_uci" move)))
                nil
                moves)
        info   (py. engine
                    "analyse"
                    board
                    (ngn/Limit :depth depth)
                    :multipv move-count)
        moves  (->> info
                   (map #(uci-and-score :white %))
                   (extract-filtered-moves allowable-loss))]
    (py. engine "quit")
    moves))

(comment


  )
