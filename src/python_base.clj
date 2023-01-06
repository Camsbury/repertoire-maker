(ns python-base
  (:require [environ.core :refer [env]]
            [libpython-clj2.python :as py]))

;;; Initialize python env
(py/initialize! :python-home (env :python-home))
