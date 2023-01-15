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

(defn sigmoid
  "normalizes a number a range between 0 and 1"
  [x]
  (/ 1 (+ 1 (Math/exp (* -1 x)))))

(defn color-score
  [score color]
  (if (= color :white)
    (py. score "white")
    (py. score "black")))

(defn parse-score
  [score]
  (if (str/includes? score "#")
    ;; inverting the mate score prioritizes lower move mates
    ;; adding 1 ensures that the score is always higher than a centipawn score
    (+ 1
       (/ 1
          (-> score
              (str/replace #"#" "")
              (Integer/parseInt))))
    ;; this is normalized via the sigmoid function to bound it and provide
    ;; a percent likelihood of winning given the current state
    (-> score
        (str/replace #"\+" "")
        (Integer/parseInt)
        sigmoid)))

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
         (filter #(> allowable-loss (/ (:score %) best-score)))
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
