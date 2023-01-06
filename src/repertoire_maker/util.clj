(ns repertoire-maker.util
  (:require
   [camel-snake-kebab.core :as csk]
   [clojure.data.json :as json]))

(defn from-json [raw]
  (json/read-str raw :key-fn (comp csk/->kebab-case keyword)))

(defn whose-turn? [moves]
  (if (= 0 (mod (count moves) 2))
    :white
    :black))
