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
              :first-invalid 0}
   :editor {:caret {:offset 0 :v-col 0}
            :selection [0 0]
            ;; :carets [{:caret 0 :virtual-col 0 :selection {:from 0 :to 0}}]
            }
   :viewport {:pos [0 0]
              :view-size [0 0]
              :metrics nil
              :focused? false}})

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

(defn insert-markers [state intervals]
  (update-in state [:document :markup] intervals/add-intervals intervals))

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
