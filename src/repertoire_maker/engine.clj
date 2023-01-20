(ns repertoire-maker.engine
  (:require
   [python-base]
   [environ.core :refer [env]]
   [clojure.string :as str]
   [repertoire-maker.util.notation :as not]
   [repertoire-maker.cloud-eval :as cloud-eval]
   [repertoire-maker.default :refer [defaults]]
   [repertoire-maker.score :as score]
   [libpython-clj2.python :refer [py. py.-] :as py]
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
    {:mate
     (-> score
         (str/replace #"#" "")
         (Integer/parseInt))}
    {:cp
     (-> score
         (str/replace #"\+" "")
         (Integer/parseInt))}))

(defn uci-and-score
  [color option]
  {:uci (-> option
            (py/get-item "pv")
            first
            (py. "uci"))
   :score (-> option
              (py/get-item "score")
              (color-score color)
              parse-score
              score/standardize-score)})

(defn moves->engine-candidates
  [{:keys [color moves move-count depth hash threads]}]
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
                    (map #(uci-and-score color %))
                    (sort-by :score >))]
    (py. engine "quit")
    moves))


;; TODO: update moves to ucis after one big loop completion
(defn prepare-engine-candidates
  [{:keys [use-engine? moves color] :as opts}]
  (when use-engine?
    (let [opts (-> defaults :engine (merge opts))

          {:keys [depth candidates]}
          (cloud-eval/fen->cloud-eval
           {:fen   (not/ucis->fen moves)
            :color color})]

      (if (some-> depth
                  (> (:depth opts)))
        candidates
        (moves->engine-candidates opts)))))
