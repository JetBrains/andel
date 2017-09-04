(ns andel.core
  (:require   [clojure.core.async :as a]
              
              [andel.editor :as editor]
              [andel.controller :as controller]
              [andel.intervals :as intervals]
              [andel.keybind :as keybind]
              [andel.styles :as styles]
              
              #_[andel.benchmarks])
    (:require-macros [reagent.interop :refer [$ $!]]
                     [cljs.core.async.macros :refer [go]]))


(defn bind-keymap! [*editor-state]
  (letfn [(capture [f]
            (fn [evt _]
              (f)
              (.stopPropagation evt)
              (.preventDefault evt)))
          (bind-function! [key f & args]
            (keybind/bind! key :global (capture #(swap! *editor-state (fn [s] (apply f s args))))))]
    (js/window.addEventListener "keydown" (keybind/dispatcher) true)
    (bind-function! "backspace" controller/backspace)
    (bind-function! "delete" controller/delete)
    (bind-function! "pgup" controller/pg-move :up false)
    (bind-function! "pgdown" controller/pg-move :down false)
    (bind-function! "shift-pgup" controller/pg-move :up true)
    (bind-function! "shift-pgdown" controller/pg-move :down true)
    (bind-function! "home" controller/home false)
    (bind-function! "shift-home" controller/home true)
    (bind-function! "end" controller/end false)
    (bind-function! "shift-end" controller/end true)
    (bind-function! "tab" (fn [state] (controller/type-in state "    ")))
    (bind-function! "left" controller/move-caret :left false)
    (bind-function! "right" controller/move-caret :right false)
    (bind-function! "up" controller/move-caret :up false)
    (bind-function! "down" controller/move-caret :down false)
    (bind-function! "shift-left" controller/move-caret :left true)
    (bind-function! "shift-right" controller/move-caret :right true)
    (bind-function! "shift-up" controller/move-caret :up true)
    (bind-function! "shift-down" controller/move-caret :down true)
    (bind-function! "esc" controller/drop-selection-on-esc)
    (bind-function! "enter" controller/on-enter)))


;; proto-marker-map -> marker-record
(defn create-marker [proto-marker]
  (letfn [(class-by-keys [ks style]
            (let [style (select-keys style ks)]
              (when (not-empty style)
                (styles/style->class style))))
          (classes-by-keys [ks styles]
            (let [classes (->> styles
                               (map (partial class-by-keys ks))
                               (filter some?))]
              (when (not-empty classes)
                (->> classes
                     (interpose " ")
                     (apply str)))))]
    (-> proto-marker
        intervals/map->Marker
        (assoc :foreground (classes-by-keys
                            [:color
                             :font-weight
                             :font-style]
                            (:style proto-marker)))
        (assoc :background (classes-by-keys
                            [:background-color
                             :border-bottom-style
                             :border-color
                             :border-width
                             :border-radius]
                            (:style proto-marker))))))

