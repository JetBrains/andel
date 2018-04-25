(ns andel.core
  (:require [clojure.core.async :as a]
            [andel.utils :as utils]
            [andel.text :as text]
            [andel.intervals :as intervals]
            [clojure.spec.alpha :as s]
            [andel.tree :as tree]))

(s/def :andel/tree tree/node?)
(s/def :andel/text :andel/tree)
(s/def :andel/markup :andel/tree)
(s/def :andel/lexer any?)
(s/def :andel/document (s/keys :req [:andel/text
                                     :andel/markup
                                     :andel/lexer]))
(s/def :andel/widgets (s/map-of nat-int? map?))


(defn make-editor-state [language color-scheme]
  {:document {:text (text/make-text "")
              :markup (intervals/make-interval-tree)
              :lexer (andel.intervals/create-lexer language "" color-scheme)}
   :editor {:caret {:offset 0 :v-col 0}
            :selection [0 0]
            :widgets {}
            :clipboard {:content nil :timestamp 0}}
   :viewport {:pos [0 0]
              :view-size [0 0]
              :metrics nil
              :focused? false}
   :sibling-editors {}})

(defn- edit-at-offset
  [{:keys [document] :as state} offset f]
  (let [edit-point (-> (:text document)
                       (text/zipper)
                       (text/scan-to-offset offset))]
    (assoc-in state [:document :text] (-> edit-point
                                          (f)
                                          (text/root)))))

(defn caret-offset [state]
  (get-in state [:editor :caret :offset]))

(defn selection [state]
  (get-in state [:editor :selection]))

(defn caret->offset [{:keys [offset] :as caret}]
  offset)

(defn count-lines-in-view [viewport metrics]
  (let [{:keys [view-size]} viewport
        [_ view-size] view-size]
    (Math/round (/ (float view-size) (utils/line-height metrics)))))

;; zero based first and last full visible lines
(defn get-view-in-lines [viewport metrics]
  (let [{:keys [pos view-size]} viewport
        [_ pos-px] pos
        [_ height] view-size
        first-full-line (Math/ceil (/ (float pos-px) (utils/line-height metrics)))
        last-full-line (dec (Math/floor (/ (float (+ pos-px height)) (utils/line-height metrics))))]
    [first-full-line last-full-line]))

;; make line first in viewport
(defn set-view-to-first-line [state line metrics]
  (update state :viewport (fn [vp]
                            (-> vp
                                (dissoc :reason)
                                (assoc-in [:pos 1] (* line (utils/line-height metrics)))))))

;; make line last in viewport
(defn set-view-to-last-line [{:keys [viewport] :as state} line metrics]
  (let [[_ pos-px] (:pos viewport)
        [_ height] (:view-size viewport)]
    (update state :viewport (fn [vp]
                              (-> vp
                                  (dissoc :reason)
                                  (assoc-in [:pos 1] (- (* (inc line) (utils/line-height metrics)) height)))))))

(defn move-view-if-needed [{:keys [document editor viewport] :as state}]
  state
  #_(let [{:keys [text]} document
        {:keys [caret]} editor
        {:keys [metrics]} viewport
        caret-l (utils/offset->line (caret->offset caret) text)
        [from-l to-l] (get-view-in-lines viewport metrics)]
    (cond (< caret-l from-l)
          (set-view-to-first-line state caret-l metrics)

          (< to-l caret-l)
          (set-view-to-last-line state caret-l metrics)

          :else state)))

(defn insert-markers [state markers]
  (update-in state [:editor :markup] intervals/add-markers markers))

(defn delete-markers [state marker-ids]
  (update-in state [:editor :markup] intervals/gc marker-ids))

(defn set-selection [state selection caret-offset]
  (-> state
      (assoc-in [:editor :selection] selection)
      (assoc-in [:editor :caret :offset] caret-offset)
      (move-view-if-needed)))

(defn insert-at-editor [editor {:keys [offset length]}]
  (let [[sel-from sel-to] (get editor :selection)
        caret-offset (get-in editor [:caret :offset])
        markup (get editor :markup)]
    (cond-> editor
      (some? markup) (assoc :markup (intervals/type-in markup offset length))
      (and (some? sel-from)     (<= offset sel-from)) (assoc-in [:selection 0] (+ sel-from length))
      (and (some? sel-to)       (<= offset sel-to)) (assoc-in [:selection 1] (+ sel-to length))
      (and (some? caret-offset) (<= offset caret-offset)) (assoc-in [:caret :offset] (+ caret-offset length)))))

(defn delete-at-editor [editor {:keys [offset length]}]
  (let [[sel-from sel-to] (get editor :selection)
        caret-offset (get-in editor [:caret :offset])
        markup (get editor :markup)]
    (cond-> editor
      (some? markup) (assoc :markup (intervals/delete-range markup offset length))
      (and (some? sel-from)     (<= offset sel-from)) (assoc-in [:selection 0] (max offset (- sel-from length)))
      (and (some? sel-to)       (<= offset sel-to)) (assoc-in [:selection 1] (max offset (- sel-to length)))
      (and (some? caret-offset) (<= offset caret-offset)) (assoc-in [:caret :offset] (max offset (- caret-offset length))))))

(defn insert-at-offset [state offset insertion]
  (let [length      (count insertion)
        text-length (-> state :document :text text/text-length)]
    (-> state
        (edit-at-offset offset #(text/insert % insertion))
        (update :document (fn [{:keys [text lexer] :as document}]
                            (cond-> document
                              (some? lexer) (assoc :lexer (intervals/update-text lexer (text/text->char-seq text) offset length 0)))))
        (update-in [:document :markup] intervals/type-in offset (count insertion))
        (cond->
          (some? (:editor state))
          (update :editor insert-at-editor {:offset offset :length length})

          (some? (:sibling-editors state))
          (update :sibling-editors
                  (fn [sibs]
                    (into {} (map (fn [[id editor]]
                                    [id (insert-at-editor editor {:offset offset :length length})])) sibs))))
        (update :log (fn [l]
                       (conj (or l []) [[:retain offset] [:insert insertion] [:retain (- text-length offset)]]))))))

(defn delete-at-offset [state offset length]
  (let [text (-> state :document :text)
        old-text (-> (text/zipper text)
                     (text/scan-to-offset offset)
                     (text/text length))
        text-length (text/text-length text)]
    (-> state
        (edit-at-offset offset #(text/delete % length))
        (update :document (fn [{:keys [text] :as document}]
                            (cond-> document (some? (:lexer document))
                                    (update :lexer intervals/update-text (text/text->char-seq text) offset 0 length))))
        (update-in [:document :markup] intervals/delete-range offset length)
        (cond->
          (some? (:editor state))
          (update :editor delete-at-editor {:offset offset :length length})

          (some? (:sibling-editors state))
          (update :sibling-editors
                  (fn [sibs]
                    (into {} (map (fn [[id editor]]
                                    [id (delete-at-editor editor {:offset offset :length length})])) sibs))))
        (update :log (fn [l]
                       (conj (or l []) [[:retain offset] [:delete old-text] [:retain (- text-length offset length)]]))))))

(defn text-at-offset [text offset length]
  (let [char-seq (text/text->char-seq text)]
    (.subSequence char-seq offset (+ offset length))))

(defn play-operation [widget operation]
  (loop [i 0
         [[type x] & rest] operation
         widget widget]
    (if (some? type)
      (case type
        :insert (recur (+ i (count x)) rest (insert-at-offset widget i x))
        :retain (recur (+ i ^long x) rest widget)
        :delete (recur i rest (delete-at-offset widget i (count x))))
      widget)))
