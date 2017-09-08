(ns andel.core
  (:require   [clojure.core.async :as a]
              [andel.utils :as utils]
              [andel.text :as text]
              [andel.intervals :as intervals]
              [andel.styles :as styles]
              [andel.intervals :as intervals]
              #_[andel.benchmarks])
    (:require-macros [reagent.interop :refer [$ $!]]
                     [cljs.core.async.macros :refer [go]]))

;; proto-marker-map -> marker-record
(defn- create-marker [proto-marker]
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

(defn- edit-at-offset
  [{:keys [document] :as state} offset f]
  (let [{:keys [text]} document
        edit-point (utils/offset->loc offset text)]
    (-> state
        (assoc-in [:document :text] (-> edit-point
                                        (f)
                                        (text/root)))
        (update-in [:document :timestamp] inc)
        (update-in [:document :first-invalid] min (utils/loc->line edit-point)))))

(defn caret-offset [state]
  (get-in state [:editor :caret :offset]))

(defn selection [state]
  (get-in state [:editor :selection]))

(defn insert-markers [state markers]
  (let [intervals (->> markers
                       (mapv create-marker)
                       (sort-by (fn [m] (.-from m))))]
    (update-in state [:document :markup] intervals/add-intervals intervals)))

(defn set-selection [state selection caret-offset]
  (-> state
      (assoc-in [:editor :selection] selection)
      (assoc-in [:editor :caret :offset] caret-offset)))

(defn insert-at-offset [state offset insertion]
  (let [[sel-from sel-to] (selection state)
        added-length (count insertion)
        caret-offset (caret-offset state)
        text-length (text/text-length (-> state :document :text))]
    (-> state
        (edit-at-offset offset #(text/insert % insertion))
        (update-in [:document :markup] intervals/type-in [offset (count insertion)])
        (cond->
          (<= offset sel-from) (update-in [:editor :selection 0] #(+ % added-length))
          (<= offset caret-offset) (update-in [:editor :caret :offset] #(+ % added-length))
          (<= offset sel-to) (update-in [:editor :selection 1] #(+ % added-length)))
        (update :log (fn [l]
                       (conj (or l []) [[:retain offset] [:insert insertion] [:retain (- text-length offset)]]))))))

(defn delete-at-offset [state offset length]
  (let [[sel-from sel-to] (selection state)
        caret-offset (caret-offset state)
        text (-> state :document :text)
        old-text (-> (text/zipper text)
                     (text/scan-to-offset offset)
                     (text/text length))
        text-length (text/text-length text)]
    (-> state
        (edit-at-offset offset #(text/delete % length))
        (update-in [:document :markup] intervals/delete-range [offset length])
        (cond->
          (<= offset sel-from) (update-in [:editor :selection 0] #(max offset (- % length)))
          (<= offset caret-offset) (update-in [:editor :caret :offset] #(max offset (- % length)))
          (<= offset sel-to) (update-in [:editor :selection 1] #(max offset (- % length))))
        (update :log (fn [l]
                       (conj (or l []) [[:retain offset] [:delete old-text] [:retain (- text-length offset length)]]))))))
