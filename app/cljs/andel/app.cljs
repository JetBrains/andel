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

(def keymap
  {"backspace"    controller/backspace
   "delete"       controller/delete
   "pgup"         #(controller/pg-move % :up false)
   "pgdown"       #(controller/pg-move % :down false)
   "shift-pgup"   #(controller/pg-move % :up true)
   "shift-pgdown" #(controller/pg-move % :down true)
   "home"         #(controller/home % false)
   "shift-home"   #(controller/home % true)
   "end"          #(controller/end % false)
   "shift-end"    #(controller/end % true)
   "tab"          (fn [state] (controller/type-in state "    "))
   "left"         #(controller/move-caret % :left false)
   "right"        #(controller/move-caret % :right false)
   "up"           #(controller/move-caret % :up false)
   "down"         #(controller/move-caret % :down false)
   "shift-left"   #(controller/move-caret % :left true)
   "shift-right"  #(controller/move-caret % :right true)
   "shift-up"     #(controller/move-caret % :up true)
   "shift-down"   #(controller/move-caret % :down true)
   "esc"          controller/drop-selection-on-esc})

(defonce editor-state-promise (let [promise (a/promise-chan)]
                                (go
                                  (let [*editor-state (editor/make-editor-state)
                                        text (:body (a/<! (http/get "EditorImpl.java")))
                                        markup (edn/read-string (:body (a/<! (http/get "markup.txt"))))]
                                    (swap! *editor-state core/insert-at-offset 0 text)
                                    (swap! *editor-state core/insert-markers markup)
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
