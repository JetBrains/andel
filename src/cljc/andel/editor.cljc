(ns andel.editor
  (:require [andel.text :as text]
            [andel.utils :as utils]
            [andel.controller :as contr]))

(defn backspace [{:keys [text caret selection] :as state}]
  (let [{caret-offset :offset} caret
        [sel-from sel-to] selection
        sel-len (- sel-to sel-from)]
    (cond (< 0 sel-len)
          (contr/delete-under-selection state)

          (< 0 caret-offset)
          (-> state
              (contr/edit-at-offset (dec caret-offset) #(text/delete % 1))
              (assoc-in [:caret :offset] (dec caret-offset))
              (assoc-in [:caret :v-col] 0)
              (assoc :selection [(dec caret-offset) (dec caret-offset)]))

          :else state)))

(defn delete [{:keys [text caret selection] :as state}]
  (let [{caret-offset :offset} caret
        [sel-from sel-to] selection
        sel-len (- sel-to sel-from)]
    (cond (< 0 sel-len)
          (contr/delete-under-selection state)

          (< caret-offset (text/text-length text))
          (contr/edit-at-offset state caret-offset #(text/delete % 1))

          :else state)))

(defn set-view-to-line! [line viewport {:keys [height] :as metrics}]
    (swap! viewport #(assoc-in % [:pos 1] (* line height))))

(defn count-lines-in-view [viewport {:keys [height] :as metrics}]
  (let [{:keys [view-size]} @viewport
        [_ view-size] view-size]
    (Math/round (/ view-size height))))

(defn get-view-in-lines [viewport {:keys [height] :as metrics}]
  (let [{:keys [pos]} @viewport
        [_ pos-px] pos
        pos-in-lines (Math/round (/ pos-px height))
        pos-in-lines-end (+ pos-in-lines (count-lines-in-view viewport metrics))]
    [pos-in-lines pos-in-lines-end]))

(defn pg-move! [{:keys [caret text] :as state} dir selection? viewport metrics]
  (let [[from-l to-l] (get-view-in-lines viewport metrics)
        caret-line (utils/offset->line (:offset caret) text)]
    (case dir
      :up (if (or (not= caret-line from-l) (= caret-line 0))
            (contr/set-caret-line-begining state from-l selection?)
            (let [new-from-l (max 0 (- from-l (- (count-lines-in-view viewport metrics) 2)))]
              (set-view-to-line! new-from-l viewport metrics)
              (contr/set-caret-line-begining state new-from-l selection?)))
      :down (cond
              (utils/last-line? to-l text)
                (contr/set-caret-line-end state to-l selection?)

              (not= caret-line (dec to-l))
                (contr/set-caret-line-end state (- to-l 1) selection?)

              :else
                (let [delta (- (count-lines-in-view viewport metrics) 2)
                    new-from-l (+ from-l delta)
                    new-to-l (+ to-l delta (- 1))]
                (set-view-to-line! new-from-l viewport metrics)
                (contr/set-caret-line-end state new-to-l selection?))))))

(defn home [{:keys [caret text] :as state} selection?]
  (let [{caret-offset :offset} caret
        line (utils/offset->line caret-offset text)]
        (contr/set-caret-line-begining state line selection?)))

(defn end [{:keys [caret text selection] :as state} selection?]
  (let [{caret-offset :offset} caret
        line (utils/offset->line caret-offset text)]
    (contr/set-caret-line-end state line selection?)))

(defn move-view-if-needed! [{:keys [caret text] :as state} viewport metrics]
  (let [{caret-offset :offset} caret
        caret-l (utils/offset->line caret-offset text)
        [from-l to-l] (get-view-in-lines viewport metrics)
        view-in-lines (- to-l from-l)]
    (cond (and (< caret-l  from-l) (not= from-l 0))
          (set-view-to-line! caret-l viewport metrics)

          (< (dec to-l) caret-l)
          (set-view-to-line! (- caret-l (dec view-in-lines)) viewport metrics)))
  state)

(defn move-caret [{:keys [caret text selection] :as state} dir selection? viewport metrics]
  (let [{caret-offset :offset v-col :v-col} caret
        [sel-from sel-to] selection
        {caret-offset' :offset :as caret'} (case dir
                 :left (if (< 0 caret-offset)
                         {:offset (dec caret-offset) :v-col 0}
                         caret)
                 :right (if (< caret-offset (dec (text/text-length text)))
                          {:offset (inc caret-offset) :v-col 0}
                          caret)
                 :up (let [[cur-line cur-col] (utils/offset->line-col caret-offset text)
                           cur-begin (utils/line->offset cur-line text)
                           prev-end (dec cur-begin)
                           prev-line (dec cur-line)
                           prev-begin (utils/line->offset prev-line text)
                           new-v-col (max v-col cur-col)]
                       (if (< 0 cur-line)
                         {:offset (min prev-end (+ prev-begin new-v-col)) :v-col new-v-col}
                         caret))
                 :down (let [[cur-line cur-col] (utils/offset->line-col caret-offset text)
                             next-line-loc (utils/next-line-loc cur-line text)
                             next-begin (utils/loc->offset next-line-loc)
                             next-end (+ next-begin (text/line-length next-line-loc))
                             new-v-col (max v-col cur-col)]
                         (if (utils/last-line? (inc cur-line) text)
                             caret
                           {:offset (min next-end (+ next-begin new-v-col)) :v-col new-v-col})))
        selection' (cond
                     (not selection?) [caret-offset' caret-offset']
                     (= caret-offset sel-from) [(min caret-offset' sel-to) (max caret-offset' sel-to)]
                     (= caret-offset sel-to) [(min sel-from caret-offset') (max sel-from caret-offset')]
                     :else [(min caret-offset caret-offset') (max caret-offset' caret-offset')])]
    (-> state
        (assoc :caret caret')
        (assoc :selection selection')
        (move-view-if-needed! viewport metrics))))
