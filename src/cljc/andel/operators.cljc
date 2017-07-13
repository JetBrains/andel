(ns andel.operators
  (:require [andel.utils :as utils]
            [andel.text :as text]))

(defn set-text
  [state text]
  (-> state
      (assoc :text (text/make-text text)
             :first-invalid 0)
      (update :timestamp inc)))

(defn edit-at-offset
  [{:keys [text] :as state} offset f]
  (let [edit-point (utils/offset->loc offset state)]
    (-> state
        (assoc :text (-> edit-point
                         (f)
                         (text/root)))
        (update :timestamp inc)
        (update :first-invalid min (utils/loc->line edit-point)))))

(defn edit-at-line-col
  [state [line col] f]
  (let [offset (utils/line-col->offset [line col] state)]
    (edit-at-offset state offset f)))

(defn delete-under-selection [state [sel-from sel-to] sel-len]
  (-> state
      (edit-at-offset sel-from #(text/delete % sel-len))
      (assoc-in [:caret :offset] sel-from)
      (assoc-in [:caret :v-col] 0)
      (assoc :selection [sel-from sel-from])))

(defn type-in [{:keys [selection] :as state} s]
  (let [[sel-from sel-to] selection
        sel-len (- sel-to sel-from)
        state (if (< 0 sel-len)
                (delete-under-selection state selection sel-len)
                state)
        caret-offset (get-in state [:caret :offset])]
    (-> state
        (edit-at-offset caret-offset #(text/insert % s))
        (update-in [:caret :offset] + (count s))
        (assoc :selection [(+ caret-offset (count s)) (+ caret-offset (count s))]))))

(defn set-caret-line-col
  [{:keys [caret selection text] :as state} line col selection?]
  (let [[sel-from sel-to] selection
        {caret-offset :offset} caret
        line-loc (utils/line->loc line state)
        line-len (text/line-length line-loc)
        line-off (utils/loc->offset line-loc)
        caret-offset' (+ line-off (min col line-len))]
    (-> state
        (assoc :caret
               {:offset caret-offset' :v-col 0})
        (assoc :selection
               (cond (not selection?)
                     [caret-offset' caret-offset']

                     (= caret-offset sel-from)
                     [(min caret-offset' sel-to) (max caret-offset' sel-to)]

                     (= caret-offset sel-to)
                     [(min sel-from caret-offset') (max sel-from caret-offset')]

                     :else
                     [(min caret-offset caret-offset') (max caret-offset'
                                                            caret-offset')])))))

(defn set-caret-offset
  [state offset selection?]
  (let [[line col] (utils/offset->line-col offset state)]
    (set-caret-line-col state line col selection?)))

(defn set-caret-line-begining
  [state line selection?]
  (set-caret-line-col state line 0 selection?))

(defn set-caret-line-end
  [state line selection?]
  (-> state
      (set-caret-line-begining (inc line) selection?)
      (update-in [:caret :offset] dec)
      (update-in [:selection 1] dec)))

(defn is-valid-caret-offset
  [offset {:keys [text] :as state}]
  (and (< 0 offset)
       (< offset (text/text-length text))))
