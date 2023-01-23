(ns repertoire-maker.action.multi)

(defmulti run-action
  "Runs the traversal action on the passed opts"
  :action)
