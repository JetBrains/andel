(ns andel.controller
  (:require [clojure.string :as cstring]
            [andel.utils :as utils]
            [andel.text :as text]
            [andel.intervals :as intervals]
            [andel.core :as core]))

(defn selection-length [[left right :as selection]]
  (assert (<= left right) (str "Wrong selection positioning: " selection))
  (- right left))

(defn backspace [state]
  (let [selection (core/selection state)
        sel-from  (nth selection 0)
        sel-length (selection-length selection)
        caret-offset (core/caret-offset state)]
    (cond
      (< 0 sel-length) (core/delete-at-offset state sel-from sel-length)
      (= 0 caret-offset) state
      :else (core/delete-at-offset state (dec caret-offset) 1))))

(defn delete [state]
  (let [selection (core/selection state)
        text-len (text/text-length (-> state :document :text))
        sel-len (selection-length selection)
        sel-from (nth selection 0)
        caret-offset (core/caret-offset state)]
    (cond
      (< 0 sel-len) (core/delete-at-offset state sel-from sel-len)
      (<= text-len caret-offset) state
      :else (core/delete-at-offset state caret-offset 1))))

(defn type-in [{:keys [editor] :as state} str]
  (let [str-len (count str)
        caret-offset (core/caret-offset state)
        selection (core/selection state)
        selection-len (selection-length selection)]
    (-> state
        (cond-> (< 0 selection-len)
                (core/delete-at-offset (first selection) selection-len))
        (core/insert-at-offset caret-offset str))))

(defn caret->offset [{:keys [offset] :as caret}]
  offset)

(defn update-selection [[from to :as selection] old-caret new-caret]
  (let [caret-offset  (caret->offset old-caret)
        caret-offset' (caret->offset new-caret)]
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

(defn drop-virtual-position [caret text]
  (let [{:keys [line col]} (utils/offset->line-col (:offset caret) text)]
    (assoc caret :v-col col)))

(defn restrict-to-text-length [offset text]
  (let [text-length (text/text-length text)]
    (-> offset (max 0) (min text-length))))

(defn translate-caret [caret text delta-offset]
  (assoc caret :offset (-> (:offset caret)
                           (+ delta-offset)
                           (restrict-to-text-length text))))

(defn translate-caret-verticaly [{v-col :v-col :as caret} text delta-line]
  (let [carret-offset (caret->offset caret)
        {:keys [line col]} (utils/offset->line-col carret-offset text)
        to-line (+ line delta-line)
        new-v-col (max v-col col)
        new-col (min (utils/line-length to-line text) new-v-col)]
    {:offset (utils/grid-pos->offset {:line to-line :col new-col} text)
     :v-col new-v-col}))

(defn get-caret-line [caret text]
  (let [{caret-offset :offset} caret
        line (utils/offset->line caret-offset text)]
    line))

(defn get-line-ident [text line]
  (let [loc (text/scan-to-line-start (text/zipper text) line)
        line-text (text/text loc (text/distance-to-EOL loc))
        trimmed (cstring/triml line-text)
        ident-size (- (count line-text) (count trimmed))]
    (subs line-text 0 ident-size)))

(defn on-enter [{:keys [editor document] :as state}]
  (let [text (:text document)
        line (get-caret-line (:caret editor) text)
        identation (get-line-ident text line)]
    (type-in state (str "\n" identation))))

(defn set-caret-at-grid-pos [{:keys [editor document] :as state} grid-pos selection?]
  (let [{:keys [caret selection]} editor
        {:keys [text]} document
        caret-offset' (utils/grid-pos->offset grid-pos text)
        caret'   (-> {:offset caret-offset'
                      :v-col 0}
                     (drop-virtual-position text))
        selection' (if selection?
                     (update-selection selection caret caret')
                     [caret-offset' caret-offset'])]
    (-> state
        (assoc-in [:editor :caret] caret')
        (assoc-in [:editor :selection] selection'))))

(defn set-caret-at-line-begining
  [state line selection?]
  (set-caret-at-grid-pos state {:line line :col 0} selection?))

(defn set-caret-at-line-end
  [state line selection?]
  (set-caret-at-grid-pos state {:line line :col #?(:cljs js/Number.POSITIVE_INFINITY
                                                   :clj Integer/MAX_VALUE)} selection?))

(defn set-view-to-line [state line metrics]
  (assoc-in state [:viewport :pos 1] (* line (utils/line-height metrics))))

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

(defn move-view-if-needed [{:keys [document editor viewport] :as state}]
  (let [{:keys [text]} document
        {:keys [caret]} editor
        {:keys [metrics]} viewport
        caret-l (utils/offset->line (caret->offset caret) text)
        [from-l to-l] (get-view-in-lines viewport metrics)
        view-in-lines (- to-l from-l)]
    (cond (and (< caret-l  from-l) (not= from-l 0))
          (set-view-to-line state caret-l metrics)

          (< (dec to-l) caret-l)
          (set-view-to-line state (- caret-l (dec view-in-lines)) metrics)

          :else state)))

(defn pg-move [{:keys [document viewport] :as state} dir selection?]
  (let [{:keys [text]} document
        {:keys [metrics]} viewport
        view-size-in-lines (count-lines-in-view viewport metrics)
        sign (case dir :up - :down +)]
    (-> state
        (update-in [:editor :caret] translate-caret-verticaly text (sign view-size-in-lines))
        (move-view-if-needed))))

(defn home [{{:keys [caret]} :editor
             {:keys [text]} :document
             :as state} selection?]
  (let [carret-line (get-caret-line caret text)]
    (set-caret-at-line-begining state (get-caret-line caret text) selection?)))

(defn end [{{:keys [caret]} :editor
             {:keys [text]} :document
             :as state} selection?]
  (let [carret-line (get-caret-line caret text)]
    (set-caret-at-line-end state (get-caret-line caret text) selection?)))

(defn move-caret [{:keys [document editor] :as state} dir selection?]
  (let [{:keys [caret selection]} editor
        text (:text document)
        caret'     (case dir
                     :left  (-> caret
                                (translate-caret text -1)
                                (drop-virtual-position text))
                     :right (-> caret
                                (translate-caret text 1)
                                (drop-virtual-position text))
                     :up    (translate-caret-verticaly caret text -1)
                     :down  (translate-caret-verticaly caret text 1))
        caret-offset' (caret->offset caret')
        selection' (if selection?
                     (update-selection selection caret caret')
                     [caret-offset' caret-offset']) ]
    (-> state
        (assoc-in [:editor :caret] caret')
        (assoc-in [:editor :selection] selection')
        (move-view-if-needed))))

(defn scroll [{:keys [document viewport] :as state} dx dy]
  (let [screen-height (get-in viewport [:view-size 1])
        line-height (utils/line-height (:metrics viewport))
        lines-count (text/lines-count (:text document))
        document-height (* lines-count line-height)
        allowed-y-offset (max 0 (- document-height (/ screen-height 2)))
        abs (fn [x] (max x (- x)))]
    (update-in state [:viewport :pos]
               (fn [[x y]]
                 (if (< (abs dx) (abs dy))
                   [x (min allowed-y-offset (max 0 (+ y dy)))]
                   [(max 0 (+ x dx)) y])))))

(defn resize [state width height]
  (assoc-in state [:viewport :view-size] [width height]))
