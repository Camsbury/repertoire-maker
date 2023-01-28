(ns repertoire-maker.game-filter.core
  (:require
   [python-base]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [criterium.core :as bench]
   [libpython-clj2.python :refer [py. py.-] :as py]
   [libpython-clj2.require :refer [require-python]])
  (:import
   [com.github.luben.zstd ZstdInputStream ZstdOutputStream]
   [java.io BufferedReader StringReader File FileInputStream FileOutputStream]))

(require-python
 '[io :as python-io]
 '[chess :as chess]
 '[chess.pgn :as pgn])

(defn- path->reader
  [path]
  (io/reader
   (ZstdInputStream.
    (FileInputStream.
     (File. path)))))

(defn- path->writer
  [path]
  (io/writer
   (ZstdOutputStream.
    (FileOutputStream.
     (File. path)))))

(def pgn-start #"\[Event")

(defn- partition-pgns
  "Partitions a stream of lines into PGNs"
  [reader]
  (->>
   (line-seq reader)
   (partition-by #(re-find pgn-start %))
   (partition 2)
   (map (fn [[[a] bs]] (str a (str/join "\n" bs) "\njn")))))

(defn- filter-pgn
  "Applies a filter to a PGN"
  [filter-fn pgn]
  (-> pgn
      python-io/StringIO
      pgn/read_headers
      filter-fn))

(defn filter-compressed-pgns
  "Apply a filter function to PGNs in a .zst file"
  [{:keys [in-file out-file filter-fn]}]
  (with-open [writer (path->writer out-file)
              reader (path->reader in-file)]
    (run! #(.write writer %)
          (eduction
           (filter #(filter-pgn filter-fn %))
           (partition-pgns reader)))))

(defn min-rating-filter
  "Both players need to be above the passed rating"
  [rating]
  (fn [pgn-headers]
    (let [parse-rating
          #(try
             (Integer/parseInt %)
             (catch NumberFormatException _ 0))
          white (parse-rating (py. pgn-headers "get" "WhiteElo"))
          black (parse-rating (py. pgn-headers "get" "BlackElo"))]
      (->> ["BlackElo" "WhiteElo"]
           (map #(py. pgn-headers "get" %))
           (map parse-rating)
           (every? #(>= % rating))))))

(comment


  (time
   (with-open [reader (path->reader "/media/monoid/compressed-games/indexed/lichess_db_standard_rated_2017-02.pgn.zst")]
     (->> reader
          partition-pgns
          #_
          (map (fn [pgn]
                 (-> pgn
                  python-io/StringIO
                  pgn/read_headers)))
          count)))

  (filter-compressed-pgns
   {:in-file "/media/monoid/compressed-games/indexed/lichess_db_standard_rated_2013-01.pgn.zst"
    :out-file "/tmp/example.pgn.zst"
    :filter-fn (min-rating-filter 2200)}))
