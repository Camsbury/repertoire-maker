(ns repertoire-maker.test)

(defn close-to
  [a b]
  (< (Math/abs (- a b)) 1e-5))
