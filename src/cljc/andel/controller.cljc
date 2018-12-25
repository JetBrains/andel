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
  (let [str-len (count insertion)
        selection (core/selection state)
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
      (= caret-offset from)
      [(min caret-offset' to) (max caret-offset' to)]

      (= caret-offset to)
      [(min from caret-offset') (max from caret-offset')]

      :else
      [(min caret-offset caret-offset') (max caret-offset' caret-offset')])))

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
  (let [carret-offset (core/caret->offset caret)
        {:keys [line col]} (utils/offset->line-col carret-offset text)
        to-line (+ line delta-line)
        line-len (utils/line-length to-line text)
        new-v-col (if (some? v-col) (max v-col col) col)
        new-col (min line-len new-v-col)]
    {:offset (utils/grid-pos->offset {:line to-line :col new-col} text)
     :v-col new-v-col}))

(defn set-caret-at-grid-pos [{:keys [editor document] :as state} grid-pos selection?]
  (let [{:keys [caret selection]} editor
        {:keys [text]} document
        caret-offset' (utils/grid-pos->offset grid-pos text)
        caret' {:offset caret-offset' :v-col 0}
        selection' (if selection?
                     (update-selection selection caret caret')
                     [caret-offset' caret-offset'])]
    (update state :editor assoc
            :caret caret'
            :selection selection')))

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
  (let [[cursor' end?] (cursor/backward-while cursor #(not= % \newline))]
    (if end? cursor' (cursor/next cursor'))))

(defn home [state selection?]
  (let [text (-> state :document :text)
        caret-offset (max 0 (dec (core/caret-offset state)))
        cursor (cursor/make-cursor text caret-offset)
        line-start-cursor (move-cursor-to-line-start cursor)
        text-start-cursor (first (cursor/forward-while line-start-cursor
                                                       #(or (= % \space)
                                                         (= % \tab))))
        line-start-offset (cursor/offset line-start-cursor)
        text-start-offset (cursor/offset text-start-cursor)]
    (if (and (< line-start-offset caret-offset)
             (< caret-offset text-start-offset))
      (set-caret-at-offset state line-start-offset selection?)
      (set-caret-at-offset state text-start-offset selection?))))

(defn end [state selection?]
  (let [text (-> state :document :text)
        caret-line (utils/offset->line (core/caret-offset state) text)]
    (set-caret-at-grid-pos state {:line caret-line :col Integer/MAX_VALUE} selection?)))

(def tab-or-space? #{\space \tab})
(def whitespace? (into tab-or-space? #{\newline}))
(def stop-symbol? (into whitespace? #{\( \) \{ \} \[ \] \; \: \> \< \. \, \\ \- \+ \* \/ \= \& \| \@ \# \^}))

(defn next-word-delta [state]
  (let [text (-> state :document :text)
        text-len (text/text-length text)
        caret-offset (core/caret-offset state)]
    (if (< caret-offset text-len)
      (let [cursor (cursor/make-cursor text caret-offset)
            char (cursor/get-char cursor)]
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
      (let [cursor (cursor/make-cursor text (dec caret-offset))
            char (cursor/get-char cursor)]
        (cond
          (and (stop-symbol? char)
               (not (whitespace? char)))
          -1
          :else
          (let [[word-end-cursor end1?] (cursor/backward-while cursor tab-or-space?)
                found-newline? (= \newline (cursor/get-char word-end-cursor))
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
