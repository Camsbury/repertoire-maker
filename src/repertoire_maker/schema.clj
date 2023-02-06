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
   [:white       {:optional true} [:maybe :double]]
   [:white-agg   {:optional true} [:maybe :double]]
   [:white-m     {:optional true} [:maybe :double]]
   [:white-m-agg {:optional true} [:maybe :double]]
   [:black       {:optional true} [:maybe :double]]
   [:black-agg   {:optional true} [:maybe :double]]
   [:black-m     {:optional true} [:maybe :double]]
   [:black-m-agg {:optional true} [:maybe :double]]
   [:score       {:optional true} [:maybe :double]]
   [:score-agg   {:optional true} [:maybe :double]]
   [:prob        {:optional true} [:maybe :double]]
   [:prob-m      {:optional true} [:maybe :double]]
   [:prob-agg    {:optional true} [:maybe :double]]
   [:ucis        [:sequential uci]]])

(def local-registry
  {::responses
   [:map-of uci [:ref ::move-tree]]

   ::move-tree
   [:and
    move-node
    [:map
     [:responses {:optional true} [:ref ::responses]]]]})

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
   [:allowable-loss {:optional true}  :double]
   [:color          {:optional false} color]
   [:export?        {:optional true}  :boolean]
   [:masters?       {:optional true}  :boolean]
   [:moves?         {:optional true}  [:sequential :string]] ; NOTE: could come up with some kind of SAN validation
   [:min-cand-prob  {:optional true}  :double]
   [:min-plays      {:optional true}  :int]
   [:min-prob-agg   {:optional true}  :double]
   [:min-resp-pct  {:optional true}  :double]
   [:overrides      {:optional true}  overrides]
   [:search-depth   {:optional true}  [:and :int [:> 0]]]
   [:strategy       {:optional true}  strategy]
   [:use-engine?    {:optional true}  :boolean]])

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
