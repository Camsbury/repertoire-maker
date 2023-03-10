(ns repertoire-maker.engine
  (:require
   [python-base]
   [taoensso.timbre :as log]
   [environ.core :refer [env]]
   [clojure.string :as str]
   [repertoire-maker.notation :as not]
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

(defn ucis->engine-candidates
  [{:keys [color ucis move-count depth hash threads]}]
  (let [engine (py. ngn/SimpleEngine "popen_uci" stockfish-path)
        _      (py. engine "configure" (py/->py-dict {"Hash"    hash
                                                      "Threads" threads}))
        board  (chess/Board)
        _      (reduce
                (fn [_ uci] (py. board "push" (py. chess/Move "from_uci" uci)))
                nil
                ucis)
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


(defn prepare-engine-candidates
  [{:keys [use-engine? move-count ucis color] :as opts}]
  (when use-engine?
    (let [opts (-> defaults :engine (merge opts))

          {:keys [depth candidates]}
          (merge
           (cloud-eval/fen->cloud-eval
            {:fen     (not/ucis->fen ucis)
             :color   color
             :breadth move-count})
           (cloud-eval/fen->cloud-eval
            {:fen   (not/ucis->fen ucis)
             :color color
             :breadth 5}))]

      (if (some-> depth
                  (> (:depth opts)))
        candidates
        (ucis->engine-candidates opts)))))

