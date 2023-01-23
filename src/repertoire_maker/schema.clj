(ns repertoire-maker.schema
  (:require
   [malli.core :as m]
   [malli.clj-kondo :as mc]
   [malli.dev :as md]))

(def uci
  [:re #"[a-h][1-8][a-h][1-8]"])

(def non-agg-stat
  [:enum
   :white
   :black
   :white-m
   :black-m
   :score])

(def agg-stat
  [:enum
   :white-agg
   :black-agg
   :white-m-agg
   :black-m-agg
   :score-agg])

(def stat
  [:enum non-agg-stat agg-stat])

(def prob
  [:enum :prob :prob-m])

(def move-node
  [:map
   [:white       :double]
   [:white-agg   :double]
   [:white-m     :double]
   [:white-m-agg :double]
   [:black       :double]
   [:black-agg   :double]
   [:black-m     :double]
   [:black-m-agg :double]
   [:score       :double]
   [:score-agg   :double]
   [:prob        :double]
   [:prob-m      :double]
   [:prob-agg    :double]
   [:ucis        [:sequential uci]]])

(def local-registry
  {::responses
   [:map-of uci [:ref ::move-tree]]

   ::move-tree
   [:and
    move-node
    [:map
     [:responses   [:ref ::responses]]]]})

(def responses
  [:schema {:registry local-registry} ::responses])

(def move-tree
  [:schema {:registry local-registry} ::move-tree])

(def action
  [:enum
   :calc-stats
   :candidates
   :create-prune-hooks
   :enum
   :init-responses
   :prune
   :responses
   :trans-stats])

(def build-step
  [:map
   [:action action]
   [:ucis
    [:sequential uci]]])

(def depth-step
  [:and build-step
        [:map
         [:depth
          [:and
           :int
           [:>= 0]]]]])

(def color
  [:enum
   :black
   :white])

(def strategy
  [:enum
   :min-loss
   :max-win-over-loss])

(def overrides
  [:map-of
   [:sequential uci]
   uci])

(def config-opts
  [:map
   [:allowable-loss :double]
   [:color          color]
   [:export?        :boolean]
   [:masters?       :boolean]
   [:min-cand-prob  :double]
   [:min-plays      :int]
   [:min-prob-agg   :double]
   [:min-resp-prob  :double]
   [:overrides      overrides]
   [:search-depth   :int]
   [:strategy       strategy]
   [:use-engine?    :boolean]])

(def build-tree-opts
  [:map
   [:tree  move-tree]
   [:stack
    [:and
     [:sequential build-step]
     list?]]])

;; NOTE: start collects and instruments, can pass collect output to mc/linter-config to get that going, then emit will create the files
(comment
  (md/start!)
  (mc/emit!)
  )
