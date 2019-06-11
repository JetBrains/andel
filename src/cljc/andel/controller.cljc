(ns andel.controller
  (:require [andel.utils :as utils]
            [andel.text :as text]
            [andel.cursor :as cursor]
            [andel.intervals :as intervals]
            [andel.parens :as parens]
            [andel.core :as core]))

(defn selection-length [[left right :as selection]]
  (assert (<= left right) (str "Wrong selection positioning: " selection))
  (- right left))

(defn drop-virtual-position
  ([state]
   (update-in state [:editor :caret] drop-virtual-position (get-in state [:document :text])))
  ([caret text]
   (let [{:keys [line col]} (utils/offset->line-col (:offset caret) text)]
     (assoc caret :v-col col))))

(defn backspace [state]
  (let [selection (core/selection state)
        sel-from  (nth selection 0)
        sel-length (selection-length selection)
        caret-offset (core/caret-offset state)]
    (-> (cond
          (< 0 sel-length) (core/delete-at-offset state sel-from sel-length)
          (= 0 caret-offset) state
          :else (core/delete-at-offset state (dec caret-offset) 1))
        drop-virtual-position)))

(defn delete [state]
  (let [selection (core/selection state)
        text-len (text/text-length (-> state :document :text))
        sel-len (selection-length selection)
        sel-from (nth selection 0)
        caret-offset (core/caret-offset state)]
    (-> (cond
          (< 0 sel-len) (core/delete-at-offset state sel-from sel-len)
          (<= text-len caret-offset) state
          :else (core/delete-at-offset state caret-offset 1))
        drop-virtual-position)))

(defn type-in [{:keys [editor] :as state} insertion]
  (let [selection (core/selection state)
        selection-len (selection-length selection)]
    (-> state
        (cond-> (< 0 selection-len)
                (core/delete-at-offset (first selection) selection-len))
        (as-> state (core/insert-at-offset state (core/caret-offset state) insertion))
        drop-virtual-position)))

(defn update-selection [[from to :as selection] old-caret new-caret]
  (let [caret-offset  (core/caret->offset old-caret)
        caret-offset' (core/caret->offset new-caret)]
    (cond
      (< from caret-offset to) [(min caret-offset' from) (max caret-offset' to)]
      (<= caret-offset from) [(min caret-offset' to) (max caret-offset' to)]
      (<= caret-offset to) [(min from caret-offset') (max from caret-offset')])))

(defn drop-selection-on-esc [state]
  (let [caret-offset (core/caret-offset state)]
    (core/set-selection state [caret-offset caret-offset] caret-offset)))

(defn restrict-to-text-length [offset text]
  (let [text-length (text/text-length text)]
    (-> offset (max 0) (min text-length))))

(defn translate-caret [caret text delta-offset]
  (assoc caret :offset (-> (:offset caret)
                           (+ delta-offset)
                           (restrict-to-text-length text))))

(defn translate-caret-verticaly [{v-col :v-col :as caret} text delta-line]
  (let [caret-offset (core/caret->offset caret)
        {:keys [line col]} (utils/offset->line-col caret-offset text)
        to-line (+ line delta-line)
        line-len (utils/line-length to-line text)
        new-v-col (if (some? v-col) (max v-col col) col)
        new-col (min line-len new-v-col)]
    {:offset (utils/line-col->offset {:line to-line :col new-col} text)
     :v-col new-v-col}))

(def tab-or-space? #{(int \space) (int \tab)})
(def whitespace? (into tab-or-space? #{(int \newline)}))
(def stop-symbol? (into whitespace? (map int) #{\( \) \{ \} \[ \] \; \: \> \< \. \, \\ \- \+ \* \/ \= \& \| \@ \# \^}))

(defn set-caret-at-line-col [{:keys [editor document] :as state} {:keys [line col]} selection?]
  (let [{:keys [caret selection]} editor
        {:keys [text]} document
        line-offset (utils/line->offset line text)
        caret-offset' (+ line-offset col)
        caret' {:offset caret-offset' :v-col 0}
        selection' (if selection?
                     (update-selection selection caret caret')
                     [caret-offset' caret-offset'])]
    (update state :editor assoc
            :caret caret'
            :selection selection')))

(defn triple-click [{:keys [editor document] :as state} {:keys [line col]}]
  (let [[sel-from sel-to :as selection] (:selection editor)
        {:keys [text]} document
        [line-start-offset line-end-offset] (utils/line->from-to-offsets line text)
        caret-offset' (+ line-start-offset col)
        caret' {:offset caret-offset' :v-col 0}
        selection' [line-start-offset (restrict-to-text-length (inc line-end-offset)
                                                               text)]]
    (update state :editor assoc
            :caret caret'
            :selection selection')))

(defn double-click [{:keys [editor document] :as state} {:keys [line col]}]
  (let [[sel-from sel-to :as selection] (:selection editor)
        {:keys [text]} document
        [line-start-offset line-end-offset] (utils/line->from-to-offsets line text)
        caret-offset' (+ line-start-offset col)
        caret' {:offset caret-offset' :v-col 0}]
    (if (= caret-offset' (text/text-length text))
      (update state :editor assoc
              :caret caret'
              :selection [caret-offset' caret-offset'])
      (let [cursor (cursor/cursor text caret-offset')
            selection' (if (whitespace? (cursor/char cursor))
                         selection
                         (let [[c-start _] (cursor/backward-while cursor #(not (stop-symbol? %)))
                               [c-end _] (cursor/forward-while cursor #(not (stop-symbol? %)))]
                           [(inc (cursor/offset c-start))
                            (cursor/offset c-end)]))]
        (update state :editor assoc
                :caret caret'
                :selection selection')))))

(defn set-caret-at-offset [{:keys [document editor] :as state} caret-offset' selection?]
  (let [text (:text document)
        {:keys [caret selection]} editor
        caret' {:offset caret-offset' :v-col 0}
        selection' (if selection?
                     (update-selection selection caret caret')
                     [caret-offset' caret-offset'])]
    (update state :editor assoc
            :caret caret'
            :selection selection')
    (-> state
        (cond-> selection?
          (update-in [:editor :selection] update-selection caret caret'))
        (cond-> (not selection?)
          (assoc-in [:editor :selection] [caret-offset' caret-offset']))
        (assoc-in [:editor :caret] caret'))))

(defn- move-cursor-to-line-start [cursor]
  (let [[cursor' end?] (cursor/backward-while cursor #(not= % (int \newline)))]
    (if end? cursor' (cursor/next cursor'))))

(defn home [state selection?]
  (let [text (-> state :document :text)
        caret-offset (core/caret-offset state)
        caret-line (utils/offset->line caret-offset text)
        line-start-offset (utils/line->offset caret-line text)
        line-start-cursor (cursor/cursor text line-start-offset)
        text-start-cursor (first (cursor/forward-while line-start-cursor
                                                       #(or (= % (int \space))
                                                         (= % (int \tab)))))
        text-start-offset (cursor/offset text-start-cursor)]
    (if (and (< line-start-offset caret-offset)
             (<= caret-offset text-start-offset))
      (set-caret-at-offset state line-start-offset selection?)
      (set-caret-at-offset state text-start-offset selection?))))

(defn end [state selection?]
  (let [text (-> state :document :text)
        caret-line (utils/offset->line (core/caret-offset state) text)
        line-length (utils/line-length caret-line text)]
    (set-caret-at-line-col state {:line caret-line :col line-length} selection?)))

(defn next-word-delta [state]
  (let [text (-> state :document :text)
        text-len (text/text-length text)
        caret-offset (core/caret-offset state)]
    (if (not= caret-offset text-len)
      (let [cursor (cursor/cursor text caret-offset)
            char (cursor/char cursor)]
        (cond
          (whitespace? char)
          (let [[word-begin-cursor end-of-text?] (cursor/forward-while cursor whitespace?)
                delta (cursor/distance cursor word-begin-cursor)]
            (cond-> delta end-of-text? inc))
          (stop-symbol? char)
          1
          :else
          (let [[word-begin-cursor end1?] (cursor/forward-while cursor whitespace?)
                [word-end-cursor end2?] (cursor/forward-while word-begin-cursor (complement stop-symbol?))
                [next-word-start-cursor end3?] (cursor/forward-while word-end-cursor whitespace?)
                delta (cursor/distance cursor next-word-start-cursor)]
            (cond-> delta (or end1? end2? end3?) inc))))
      0)))

(defn prev-word-delta [state]
  (let [text (-> state :document :text)
        caret-offset (core/caret-offset state)]
    (if (< 0 caret-offset)
      (let [cursor (cursor/cursor text (dec caret-offset))
            char (cursor/char cursor)]
        (cond
          (and (stop-symbol? char)
               (not (whitespace? char)))
          -1
          :else
          (let [[word-end-cursor end1?] (cursor/backward-while cursor tab-or-space?)
                found-newline? (= \newline (cursor/char word-end-cursor))
                [word-begin-cursor end2?] (cursor/backward-while word-end-cursor (complement stop-symbol?))
                delta (- (cursor/distance cursor word-begin-cursor))]
            (cond-> delta (or end1? end2? found-newline?) dec))))
      0)))

(defn delete-word-forward [state]
  (let [caret-offset (core/caret-offset state)
        delta (next-word-delta state)]
    (core/delete-at-offset state caret-offset delta)))

(defn delete-word-backward [state]
  (let [caret-offset (core/caret-offset state)
        delta (prev-word-delta state)] ;; negative delta
    (core/delete-at-offset state (+ caret-offset delta) (- delta))))

(defn move-caret [{:keys [document editor] :as state} dir selection?]
  (let [{:keys [caret selection]} editor
        text (:text document)
        caret' (case dir
                 :left  (-> caret
                            (translate-caret text -1)
                            (drop-virtual-position text))
                 :right (-> caret
                            (translate-caret text 1)
                            (drop-virtual-position text))
                 :word-forward (-> caret
                                   (translate-caret text (next-word-delta state))
                                   (drop-virtual-position text))
                 :word-backward (-> caret
                                    (translate-caret text (prev-word-delta state))
                                    (drop-virtual-position text))
                 :up    (translate-caret-verticaly caret text -1)
                 :down  (translate-caret-verticaly caret text 1))
        caret-offset' (core/caret->offset caret')
        selection' (if selection?
                     (update-selection selection caret caret')
                     [caret-offset' caret-offset'])]
    (-> state
        (assoc-in [:editor :caret] caret')
        (assoc-in [:editor :selection] selection'))))

(defn update-clipboard [f]
  (fn [state content]
    (-> state
      (update-in [:editor :clipboard :content] f content)
      (update-in [:editor :clipboard :timestamp] inc))))

(def put-to-clipboard (update-clipboard (fn [_ v] v)))
(def conj-to-clipboard (update-clipboard str))

(defn select-all [state]
  (let [text (get-in state [:document :text])
        from 0
        to (text/text-length text)]
    (assoc-in state [:editor :selection] [from to])))

(defn copy [state]
  (let [text (get-in state [:document :text])
        [sel-from _ :as selection] (core/selection state)
        sel-len (selection-length selection)
        selected-text (str (core/text-at-offset text sel-from sel-len))]
    (cond-> state
      (< 0 sel-len)
      (put-to-clipboard selected-text))))

(defn cut [state]
  (let [text (get-in state [:document :text])
        [sel-from _ :as selection] (core/selection state)
        sel-len (selection-length selection)
        selected-text (str (core/text-at-offset text sel-from sel-len))]
    (cond-> state
      (< 0 sel-len)
      (-> (core/delete-at-offset sel-from sel-len)
          (put-to-clipboard selected-text)))))
