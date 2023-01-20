(ns repertoire-maker.util.test
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [flatland.ordered.map]))

(defn close-to
  ([a b]
   (close-to a b 1e-7))
  ([a b epsilon]
   (< (Math/abs (- a b)) epsilon)))

(defn load-test-data
  [& dataset-names]
  (into
   {}
   (map (fn [dataset-name]
          [(keyword dataset-name)
           (edn/read-string
            {:readers
             {'ordered/map flatland.ordered.map/ordered-map-reader}}
            (slurp
             (io/resource (str "test/" dataset-name ".edn"))))]))
   dataset-names))
