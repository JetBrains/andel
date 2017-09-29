(ns andel.core
  (:require [clojure.core.async :as a]
            [andel.utils :as utils]
            [andel.text :as text]
            [andel.intervals :as intervals]))

(defn make-editor-state []
  {:document {:text (text/make-text "")
              :markup (intervals/make-interval-tree)
              :lexer-broker (a/chan)
              :modespec "text/x-java"
              :timestamp 0
              :lines []
              :first-invalid 0
              :deleted-markers #{}}
   :editor {:caret {:offset 0 :v-col 0}
            :selection [0 0]}
   :viewport {:pos [0 0]
              :view-size [0 0]
              :metrics nil
              :focused? false
              :sync-with-clipboard false}})

(defn- edit-at-offset
  [{:keys [document] :as state} offset f]
  (let [edit-point (-> (:text document)
                       (text/zipper)
                       (text/scan-to-offset offset))]
    (-> state
        (assoc-in [:document :text] (-> edit-point
                                         (f)
                                         (text/root)))
        (update-in [:document :timestamp] inc)
        (update-in [:document :first-invalid] min (utils/line-number edit-point)))))

(defn caret-offset [state]
  (get-in state [:editor :caret :offset]))

(defn selection [state]
  (get-in state [:editor :selection]))

(defn caret->offset [{:keys [offset] :as caret}]
  offset)

(defn count-lines-in-view [viewport metrics]
  (let [{:keys [view-size]} viewport
        [_ view-size] view-size]
    (Math/round (/ view-size (utils/line-height metrics)))))

(defn get-view-in-lines [viewport metrics]
  (let [{:keys [pos]} viewport
        [_ pos-px] pos
        pos-in-lines (Math/round (/ pos-px (utils/line-height metrics)))
        pos-in-lines-end (+ pos-in-lines (count-lines-in-view viewport metrics))]
    [pos-in-lines pos-in-lines-end]))

(defn set-view-to-line [state line metrics]
  (assoc-in state [:viewport :pos 1] (* line (utils/line-height metrics))))

(defn move-view-if-needed [{:keys [document editor viewport] :as state}]
  (let [{:keys [text]} document
        {:keys [caret]} editor
        {:keys [metrics]} viewport
        caret-l (utils/offset->line (caret->offset caret) text)
        [from-l to-l] (get-view-in-lines viewport metrics)
        view-in-lines (- to-l from-l)]
    (cond (< caret-l from-l)
          (set-view-to-line state (max 0 caret-l) metrics)

          (< (dec to-l) caret-l)
          (set-view-to-line state (- caret-l (max 0 (dec view-in-lines))) metrics)

          :else state)))

(defn insert-markers [state markers]
  (update-in state [:document :markup] intervals/add-markers markers))

(defn delete-markers [state marker-ids]
  (update-in state [:document :deleted-markers] into marker-ids))

(defn set-selection [state selection caret-offset]
  (-> state
      (assoc-in [:editor :selection] selection)
      (assoc-in [:editor :caret :offset] caret-offset)
      (move-view-if-needed)))

(defn insert-at-offset [state offset insertion]
  (let [[sel-from sel-to] (selection state)
        added-length (count insertion)
        caret-offset (caret-offset state)
        text-length (-> state :document :text text/text-length)]
    (-> state
        (edit-at-offset offset #(text/insert % insertion))
        (update-in [:document :markup] intervals/type-in offset (count insertion))
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
        (update-in [:document :markup] intervals/delete-range offset length)
        (cond->
          (<= offset sel-from) (update-in [:editor :selection 0] #(max offset (- % length)))
          (<= offset caret-offset) (update-in [:editor :caret :offset] #(max offset (- % length)))
          (<= offset sel-to) (update-in [:editor :selection 1] #(max offset (- % length))))
        (update :log (fn [l]
                       (conj (or l []) [[:retain offset] [:delete old-text] [:retain (- text-length offset length)]]))))))
