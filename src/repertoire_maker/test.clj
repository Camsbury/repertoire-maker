(ns repertoire-maker.test)

(defn close-to
  ([a b]
   (close-to a b 1e-7))
  ([a b epsilon]
   (< (Math/abs (- a b)) epsilon)))
