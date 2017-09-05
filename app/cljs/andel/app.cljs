(ns andel.app
  (:require [clojure.core.async :as a]
            [clojure.tools.reader.edn :as edn]
            [cljs-http.client :as http]
            
            [andel.core :as core]
            [andel.editor :as editor]
            [andel.controller :as controller])
  (:require-macros [reagent.interop :refer [$ $!]]
                   [cljs.core.async.macros :refer [go]]))


(enable-console-print!)


(defonce editor-state-promise (let [promise (a/promise-chan)]
                                (go
                                  (let [*editor-state (editor/make-editor-state)
                                        text (:body (a/<! (http/get "EditorImpl.java")))
                                        markup (->> (:body (a/<! (http/get "markup.txt")))
                                                    edn/read-string
                                                    (sort-by :from)
                                                    (mapv core/create-marker))]
                                    (swap! *editor-state controller/set-text text)
                                    (swap! *editor-state controller/add-markup markup)
                                    (core/bind-keymap! *editor-state)
                                    (a/>! promise *editor-state)))
                                promise))


(defn init! []
  (go
    (let [*editor-state (a/<! editor-state-promise)
          root (.getElementById js/document "app")]
      (.render js/ReactDOM
               (editor/editor-view *editor-state)
               root))))

(init!)
