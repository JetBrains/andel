(ns andel.core
  (:require   [clojure.core.async :as a]
              
              [andel.editor :as editor]
              [andel.controller :as controller]
              [andel.text :as text]
              [andel.intervals :as intervals]
              [andel.keybind :as keybind]
              [andel.styles :as styles]
              [andel.intervals :as intervals]
              
              #_[andel.benchmarks])
    (:require-macros [reagent.interop :refer [$ $!]]
                     [cljs.core.async.macros :refer [go]]))

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

(defn insert-markers [editor markers]
  (let [intervals (->> markers
                       (mapv create-marker)
                       (sort-by (fn [m] (.-from m))))]
    (update-in editor [:document :markup] intervals/add-intervals intervals)))

(defn update-selection [editor selection caret]
  (-> editor
      (assoc-in [:editor :selection] selection)
      (assoc-in [:editor :caret] {:offset caret :v-col 0})))

(defn insert-at-offset [editor offset insertion]
  (-> editor
      (controller/edit-at-offset offset #(text/insert % insertion))
      (update-in [:document :markup] intervals/type-in [offset (count insertion)])))

(defn delete-at-offset [editor offset length]
  (-> editor
      (controller/edit-at-offset offset #(text/delete % length))
      (update-in [:document :markup] intervals/delete-range [offset length])))
