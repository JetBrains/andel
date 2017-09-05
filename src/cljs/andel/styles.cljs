(ns andel.styles
  (:require [clojure.core.async :as a]
            [garden.core :as g])
  (:require-macros [reagent.interop :refer [$ $!]]
                   [cljs.core.async.macros :refer [go]]))


(defn head []
  (aget (js/document.getElementsByTagName "head") 0))


(defn body []
  js/document.body)


(defn include-script [src]
  (let [e (js/document.createElement "script")
        res (a/promise-chan)]
    (doto e
      (.setAttribute "type" "text/javascript")
      (.setAttribute "src" src))
    (aset e "onload" #(a/put! res :done))
    (.appendChild (head) e)
    res))


(defn include-style [src cb]
  (let [e (js/document.createElement "link")]
    (aset e "onload" cb)
    (doto e
      (.setAttribute "type" "text/css")
      (.setAttribute "rel" "stylesheet")
      (.setAttribute "href" src))
    (.appendChild (head) e)))


(defn- defstyle-impl
  ([key style]
   (let [id  (str "style_" (if key (name key) (hash-coll style)))
         e   (or (js/document.getElementById id)
                 (let [e (js/document.createElement "style")]
                   (aset e "id" id)
                   (aset e "type" "text/css")
                   (.appendChild (head) e)
                   e))
         css (g/css style)]
     (aset e "innerHTML" css)))
  ([style]
   (defstyle-impl nil style)))


(defonce defstyle (memoize defstyle-impl))


(defn style->class [style]
  (let [name (str "style__" (hash-coll style))]
    (defstyle [(str "." name) style])
    name))


(defn px [x]
  (str x "px"))
