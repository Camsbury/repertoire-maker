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
