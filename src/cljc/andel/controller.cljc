(ns andel.controller
  (:require [andel.utils :as utils]
            [andel.text :as text]))

(defn set-text
  [state text]
  (-> state
      (assoc-in [:document :text] (text/make-text text))
      (assoc-in [:document :first-invalid] 0)
      (update-in [:document :timestamp] inc)))

(defn edit-at-offset
  [{:keys [document] :as state} offset f]
  (let [{:keys [text]} document
        edit-point (utils/offset->loc offset text)]
    (-> state
        (assoc-in [:document :text] (-> edit-point
                                        (f)
                                        (text/root)))
        (update-in [:document :timestamp] inc)
        (update-in [:document :first-invalid] min (utils/loc->line edit-point)))))

(defn edit-at-line-col
  [{:keys [text] :as state} [line col] f]
  (let [offset (utils/line-col->offset [line col] text)]
    (edit-at-offset state offset f)))

(defn delete-under-selection [{:keys [document editor] :as state}]
  (let [{:keys [selection]} editor
        {:keys [document]} editor
        [sel-from sel-to] selection
        sel-len (- sel-to sel-from)]
    (-> state
        (edit-at-offset sel-from #(text/delete % sel-len))
        (assoc-in [:editor :caret :offset] sel-from)
        (assoc-in [:editor :caret :v-col] 0)
        (assoc-in [:editor :selection] [sel-from sel-from]))))


(defn type-in [{:keys [editor] :as state} s]
  (let [caret-offset (get-in state [:editor :caret :offset])]
    (-> state
        (delete-under-selection)
        (edit-at-offset caret-offset #(text/insert % s))
        (update-in [:editor :caret :offset] + (count s))
        (assoc-in [:editor :selection] [(+ caret-offset (count s)) (+ caret-offset (count s))]))))

(defn set-caret-line-col
  [{:keys [editor document] :as state} line col selection?]
  (let [{:keys [caret selection]} editor
        {:keys [text]} document
        [sel-from sel-to] selection
        {caret-offset :offset} caret
        line-loc (utils/line->loc line text)
        line-len (text/line-length line-loc)
        line-off (utils/loc->offset line-loc)
        caret-offset' (+ line-off (min col line-len))]
    (-> state
        (assoc-in [:editor :caret]
                  {:offset caret-offset' :v-col 0})
        (assoc-in [:editor :selection]
                  (cond (not selection?)
                        [caret-offset' caret-offset']

                        (= caret-offset sel-from)
                        [(min caret-offset' sel-to) (max caret-offset' sel-to)]

                        (= caret-offset sel-to)
                        [(min sel-from caret-offset') (max sel-from caret-offset')]

                        :else
                        [(min caret-offset caret-offset') (max caret-offset'
                                                               caret-offset')])))))

(defn set-caret-line-begining
  [state line selection?]
  (set-caret-line-col state line 0 selection?))

(defn set-caret-line-end
  [state line selection?]
  (-> state
      (set-caret-line-begining (inc line) selection?)
      (update-in [:editor :caret :offset] dec)
      (update-in [:editor :selection 1] dec)))

(defn is-valid-caret-offset
  [offset {:keys [text] :as state}]
  (and (< 0 offset)
       (< offset (text/text-length text))))
