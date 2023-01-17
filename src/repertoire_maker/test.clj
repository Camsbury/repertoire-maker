(ns repertoire-maker.test
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [flatland.ordered.map]))

(defn close-to
  ([a b]
   (close-to a b 1e-7))
  ([a b epsilon]
   (< (Math/abs (- a b)) epsilon)))

(defn load-fixtures
  [& fixture-names]
  (into
   {}
   (map (fn [fixture-name]
          [(keyword fixture-name)
           (edn/read-string
            {:readers
             {'ordered/map flatland.ordered.map/ordered-map-reader}}
            (slurp
             (io/resource (str "fixtures/" fixture-name ".edn"))))]))
   fixture-names))
