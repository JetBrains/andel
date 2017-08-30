(ns andel.controller
  (:require [clojure.string :as cstring]
            [andel.utils :as utils]
            [andel.text :as text]
            [andel.intervals :as intervals]))

(defn drop-virtual-position [caret]
  (assoc caret :v-col 0))

(defn caret->offset [{:keys [offset] :as caret}]
  offset)

(defn set-caret-at-offset [caret text new-offset]
  (let [text-length (dec (text/text-length text))
        offset' (-> new-offset
                    (max 0)
                    (min text-length))]
    (-> caret
        (assoc :offset offset')
        (drop-virtual-position))))

(defn translate-caret [caret text delta-offset]
  (set-caret-at-offset caret text (+ (caret->offset caret) delta-offset)))

(defn translate-caret-verticaly [{v-col :v-col :as caret} text delta-line]
  (let [carret-offset (caret->offset caret)
        {:keys [line col]} (utils/offset->line-col carret-offset text)
        to-line (+ line delta-line)
        prev-line-length (utils/line->length to-line text)
        new-v-col (max v-col col)
        new-col (min prev-line-length new-v-col)]
    {:offset (utils/line-col->offset {:line to-line :col new-col} text)
     :v-col new-v-col}))

(defn update-selection [[from to :as selection] old-caret new-caret selection?]
  (let [caret-offset  (caret->offset old-caret)
        caret-offset' (caret->offset new-caret)]
    (cond
      (not selection?)
      [caret-offset' caret-offset']

      (= caret-offset from)
      [(min caret-offset' to) (max caret-offset' to)]

      (= caret-offset to)
      [(min from caret-offset') (max from caret-offset')]

      :else
      [(min caret-offset caret-offset') (max caret-offset' caret-offset')])))

(defn selection-length [[left right :as selection]]
  (assert (<= left right) (str "Wrong selection positioning: " selection))
  (- right left))

(defn drop-selection [{{:keys [offset]} :caret :as editor}]
  (assoc editor :selection [offset offset]))

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

(defn edit-at-caret [state fn]
  (let [caret-offset (get-in state [:editor :caret :offset])]
    (edit-at-offset state caret-offset fn)))

(defn delete-under-selection [{:keys [editor document] :as state}]
  (let [{:keys [selection]} editor
        [sel-from sel-to] selection
        sel-len (- sel-to sel-from)]
    (as-> state st
        (edit-at-offset st sel-from #(text/delete % sel-len))
        (update-in st [:editor :caret] set-caret-at-offset (:text document) sel-from)
        (update-in st [:document :markup] intervals/delete-range [(-> st :editor :caret :offset) sel-len])
        (update st :editor drop-selection))))

(defn set-selection-under-caret [editor]
  (let [caret-offset (get-in editor [:caret :offset])]
    (assoc editor :selection [caret-offset caret-offset])))

(defn type-in [{:keys [editor] :as state} str]
  (let [str-len (count str)]
    (as-> state st
        (delete-under-selection st)
        (edit-at-caret st #(text/insert % str))
        (update-in st [:document :markup] intervals/type-in [(-> st :editor :caret :offset) str-len])
        (update-in st [:editor :caret] translate-caret (-> st :document :text) str-len)
        (update-in st [:editor] set-selection-under-caret))))

(defn get-caret-line [caret text]
  (let [{caret-offset :offset} caret
        line (utils/offset->line caret-offset text)]
    line))

(defn get-line-ident [text line]
  (let [loc (text/scan-to-line (text/zipper text) line)
        line-text (text/text loc (text/line-length loc))
        trimmed (cstring/triml line-text)
        ident-size (- (count line-text) (count trimmed))]
    (subs line-text 0 ident-size)))

(defn on-enter [{:keys [editor document] :as state}]
  (let [text (:text document)
        line (get-caret-line (:caret editor) text)
        identation (get-line-ident text line)]
    (type-in state (str "\n" identation))))

(defn set-caret-at-grid-pos [{:keys [editor document] :as state} line-col selection?]
  (let [{:keys [caret selection]} editor
        {:keys [text]} document
        caret-offset' (utils/line-col->offset line-col text)
        caret' (set-caret-at-offset caret text caret-offset')
        selection' (update-selection selection caret caret' selection?)]
    (as-> state st
          (assoc-in st [:editor :caret] caret')
          (assoc-in st [:editor :selection] selection'))))

(defn set-caret-at-line-begining
  [state line selection?]
  (set-caret-at-grid-pos state {:line line :col 0} selection?))

(defn set-caret-at-line-end
  [state line selection?]
  (set-caret-at-grid-pos state {:line line :col #?(:cljs js/Number.POSITIVE_INFINITY
                                                   :clj Integer/MAX_VALUE)} selection?))

(defn backspace [{:keys [editor document] :as state}]
  (let [{:keys [caret selection]} editor
        caret-offset (caret->offset caret)]
    (cond (< 0 (selection-length selection))
          (delete-under-selection state)

          (< 0 caret-offset)
          (as-> state st
            (update-in st [:document :markup] intervals/delete-range [(dec (-> st :editor :caret :offset)) 1])
            (update-in st [:editor :caret] translate-caret (:text document) -1)
            (edit-at-caret st #(text/delete % 1))
            (update st :editor drop-selection))

          :else state)))

(defn delete [{:keys [document editor] :as state}]
  (let [{:keys [caret selection]} editor
        {caret-offset :offset} caret
        {:keys [text]} document]
    (cond (< 0 (selection-length selection))
          (delete-under-selection state)

          (< caret-offset (text/text-length text))
          (as-> state st
            (edit-at-caret st #(text/delete % 1))
            (update-in st [:document :markup] intervals/delete-range [(-> st :editor :caret :offset) 1]))

          :else state)))

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

(defn move-caret [{{:keys [text]} :document
                   {:keys [caret selection] } :editor
                   :as state} dir selection?]
  (let [caret'     (case dir
                     :left  (translate-caret caret text -1)
                     :right (translate-caret caret text 1)
                     :up    (translate-caret-verticaly caret text -1)
                     :down  (translate-caret-verticaly caret text 1))
        selection' (update-selection selection caret caret' selection?)]
    (-> state
        (assoc-in [:editor :caret] caret')
        (assoc-in [:editor :selection] selection')
        (move-view-if-needed))))

(defn drop-selection-on-esc [state]
  (update state :editor drop-selection))
