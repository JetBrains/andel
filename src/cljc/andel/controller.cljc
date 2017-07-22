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
  [{:keys [text] :as state} line-col f]
  (let [offset (utils/line-col->offset line-col text)]
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

(defn set-selection-under-caret [editor]
  (let [caret-offset (get-in editor [:caret :offset])]
    (assoc editor :selection [caret-offset caret-offset])))

(defn type-in [{:keys [editor] :as state} str]
  (let [caret-offset (get-in editor [:caret :offset])
        str-len (count str)]
    (-> state
        (delete-under-selection)
        (edit-at-offset caret-offset #(text/insert % str))
        (update-in [:editor :caret :offset] + str-len)
        (update-in [:editor] set-selection-under-caret))))

(defn set-caret-at-line-col
  [{:keys [editor document] :as state} {:keys [line col]} selection?]
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

(defn set-caret-at-line-begining
  [state line selection?]
  (set-caret-at-line-col state {:line line :col 0} selection?))

(defn set-caret-at-line-end
  [state line selection?]
  (-> state
      (set-caret-at-line-begining (inc line) selection?)
      (update-in [:editor :caret :offset] dec)
      (update-in [:editor :selection 1] dec)))

(defn backspace [{:keys [document editor] :as state}]
  (let [{:keys [text]} document
        {:keys [caret selection]} editor
        {caret-offset :offset} caret
        [sel-from sel-to] selection
        sel-len (- sel-to sel-from)]
    (cond (< 0 sel-len)
          (delete-under-selection state)

          (< 0 caret-offset)
          (-> state
              (edit-at-offset (dec caret-offset) #(text/delete % 1))
              (assoc-in [:editor :caret :offset] (dec caret-offset))
              (assoc-in [:editor :caret :v-col] 0)
              (assoc-in [:editor :selection] [(dec caret-offset) (dec caret-offset)]))

          :else state)))

(defn delete [{:keys [document editor] :as state}]
  (let [{:keys [text]} document
        {:keys [caret selection]} editor
        {caret-offset :offset} caret
        [sel-from sel-to] selection
        sel-len (- sel-to sel-from)]
    (cond (< 0 sel-len)
          (delete-under-selection state)

          (< caret-offset (text/text-length text))
          (edit-at-offset state caret-offset #(text/delete % 1))

          :else state)))

(defn set-view-to-line [state line {:keys [height] :as metrics}]
  (assoc-in state [:viewport :pos 1] (* line height)))

(defn count-lines-in-view [viewport {:keys [height] :as metrics}]
  (let [{:keys [view-size]} viewport
        [_ view-size] view-size]
    (Math/round (/ view-size height))))

(defn get-view-in-lines [viewport {:keys [height] :as metrics}]
  (let [{:keys [pos]} viewport
        [_ pos-px] pos
        pos-in-lines (Math/round (/ pos-px height))
        pos-in-lines-end (+ pos-in-lines (count-lines-in-view viewport metrics))]
    [pos-in-lines pos-in-lines-end]))

(defn pg-move [{:keys [document editor viewport] :as state} dir selection?]
  (let [{:keys [text]} document
        {:keys [caret]} editor
        {:keys [metrics]} viewport
        [from-l to-l] (get-view-in-lines viewport metrics)
        caret-line (utils/offset->line (:offset caret) text)]
    (case dir
      :up (if (or (not= caret-line from-l) (= caret-line 0))
            (set-caret-at-line-begining state from-l selection?)
            (let [new-from-l (max 0 (- from-l (- (count-lines-in-view viewport metrics) 2)))]
              (-> state
                  (set-view-to-line new-from-l metrics)
                  (set-caret-at-line-begining new-from-l selection?))))
      :down (cond
              (utils/last-line? to-l text)
              (set-caret-at-line-end state to-l selection?)

              (not= caret-line (dec to-l))
              (set-caret-at-line-end state (- to-l 1) selection?)

              :else
              (let [delta (- (count-lines-in-view viewport metrics) 2)
                    new-from-l (+ from-l delta)
                    new-to-l (+ to-l delta (- 1))]
                (-> state
                    (set-view-to-line new-from-l metrics)
                    (set-caret-at-line-end new-to-l selection?)))))))

(defn home [{:keys [document editor] :as state} selection?]
  (let [{:keys [text]} document
        {:keys [caret]} editor
        {caret-offset :offset} caret
        line (utils/offset->line caret-offset text)]
    (set-caret-at-line-begining state line selection?)))

(defn end [{:keys [document editor] :as state} selection?]
  (let [{:keys [text]} document
        {:keys [caret]} editor
        {caret-offset :offset} caret
        line (utils/offset->line caret-offset text)]
    (set-caret-at-line-end state line selection?)))

(defn move-view-if-needed [{:keys [document editor viewport] :as state}]
  (let [{:keys [text]} document
        {:keys [caret]} editor
        {:keys [metrics]} viewport
        {caret-offset :offset} caret
        caret-l (utils/offset->line caret-offset text)
        [from-l to-l] (get-view-in-lines viewport metrics)
        view-in-lines (- to-l from-l)]
    (cond (and (< caret-l  from-l) (not= from-l 0))
          (set-view-to-line state caret-l metrics)

          (< (dec to-l) caret-l)
          (set-view-to-line state (- caret-l (dec view-in-lines)) metrics)

          :else state)))

(defn move-caret [{:keys [document editor viewport] :as state} dir selection? metrics]
  (let [{:keys [text]} document
        {:keys [caret selection]} editor
        {caret-offset :offset v-col :v-col} caret
        [sel-from sel-to] selection
        {caret-offset' :offset :as caret'} (case dir
                                             :left (if (< 0 caret-offset)
                                                     {:offset (dec caret-offset) :v-col 0}
                                                     caret)
                                             :right (if (< caret-offset (dec (text/text-length text)))
                                                      {:offset (inc caret-offset) :v-col 0}
                                                      caret)
                                             :up (let [{cur-line :line cur-col :col} (utils/offset->line-col caret-offset text)
                                                       cur-begin (utils/line->offset cur-line text)
                                                       prev-end (dec cur-begin)
                                                       prev-line (dec cur-line)
                                                       prev-begin (utils/line->offset prev-line text)
                                                       new-v-col (max v-col cur-col)]
                                                   (if (< 0 cur-line)
                                                     {:offset (min prev-end (+ prev-begin new-v-col)) :v-col new-v-col}
                                                     caret))
                                             :down (let [{cur-line :line cur-col :col} (utils/offset->line-col caret-offset text)
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
        (assoc-in [:editor :caret] caret')
        (assoc-in [:editor :selection] selection')
        (move-view-if-needed))))
