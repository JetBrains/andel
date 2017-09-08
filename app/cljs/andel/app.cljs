(ns andel.app
  (:require [clojure.core.async :as a]
            [clojure.tools.reader.edn :as edn]
            [cljs-http.client :as http]
            
            [andel.core :as core]
            [andel.editor :as editor]
            [andel.controller :as controller]
            [andel.keybind :as keybind]
            [andel.benchmarks :as bench]
            [andel.intervals :as intervals])
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
   "esc"          controller/drop-selection-on-esc
   "ctrl-b"       (fn [state]
                    (let [itree (get-in state [:document :markup])]
                      (bench/bench "TYPE-IN BENCH"
                                   (fn []
                                     (let [offset (rand-int 160000)
                                           size 1]
                                       (andel.intervals/type-in itree [offset size])))
                                   :count 10000))
                    state)})

(defonce editor-state-promise (let [promise (a/promise-chan)]
                                (go
                                  (let [text (:body (a/<! (http/get "EditorImpl.java")))
                                        markup (->> (edn/read-string (:body (a/<! (http/get "markup.txt"))))
                                                    (mapv editor/create-marker)
                                                    (sort-by (fn [m] (.-from m))))
                                        metrics (:font-metrics (a/<! andel.editor/*editors-common))
                                        editor-state (-> (core/make-editor-state)
                                                         (assoc-in [:viewport :focused?] true)
                                                         (assoc-in [:viewport :metrics] metrics)
                                                         (core/insert-at-offset 0 text)
                                                         (core/insert-markers markup))]
                                    (a/>! promise editor-state)))
                                promise))

(def next-tick
  (let [w js/window]
    (or ($ w :requestAnimationFrame)
        ($ w :webkitRequestAnimationFrame)
        ($ w :mozRequestAnimationFrame)
        ($ w :msRequestAnimationFrame))))

(defn init! []
  (go
    (let [*editor-state (atom (a/<! editor-state-promise))
          *keybindings (atom (keybind/make-bindings keymap))
          root (.getElementById js/document "app")
          *scheduled? (atom false)
          render (fn []
                   (.render js/ReactDOM
                            (editor/editor-view @*editor-state
                                                {:on-cursor-activity (fn [line-col]
                                                                       (swap! *editor-state (fn [state] state)))
                                                 :on-input           (fn [input]
                                                                       (swap! *editor-state #(controller/type-in % input)))
                                                 :on-mouse-down      (fn [x y]
                                                                       (swap! *editor-state (fn [state]
                                                                                              (let [viewport (:viewport state)
                                                                                                    line-col (andel.utils/pixels->grid-position [x y] viewport)]
                                                                                                (controller/set-caret-at-grid-pos state line-col false)))))
                                                 :on-drag-selection  (fn [x y]
                                                                       (swap! *editor-state (fn [state]
                                                                                              (let [viewport (:viewport state)
                                                                                                    line-col (andel.utils/pixels->grid-position [x y] viewport)]
                                                                                                (controller/set-caret-at-grid-pos state line-col true)))))
                                                 :on-scroll          (fn [dx dy]
                                                                       (swap! *editor-state #(controller/scroll % dx dy)))
                                                 :on-resize          (fn [width height]
                                                                       (swap! *editor-state #(controller/resize % width height)))
                                                 :on-focus           (fn [] nil)
                                                 :on-key-down        (fn [evt]
                                                                       (let [[bindings' callback] (keybind/dispatch @*keybindings evt)]
                                                                         (reset! *keybindings bindings')
                                                                         (when callback
                                                                           (swap! *editor-state callback))))})
                            root))]
      (add-watch *editor-state :editor-view
                         (fn [_ _ old-state new-state]
                           (when (and (not= old-state new-state) (not @*scheduled?))
                             (reset! *scheduled? true)
                             (next-tick (fn []
                                          (reset! *scheduled? false)
                                          (render))))))
      (andel.editor/attach-lexer! *editor-state)
      (render))))

(init!)
