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
(s/def :andel/deleted-markers (s/coll-of any? :kind set?))
(s/def :andel/document (s/keys :req [:andel/text
                                     :andel/markup
                                     :andel/lexer
                                     :andel/deleted-markers]))
(s/def :andel/widgets (s/map-of nat-int? map?))

(defn make-editor-state [language color-scheme]
  {:document {:text (text/make-text "")
              :markup (intervals/make-interval-tree)
              :lexer (andel.intervals/create-lexer language "" color-scheme)
              :deleted-markers #{}}
   :editor {:caret {:offset 0 :v-col 0}
            :selection [0 0]
            :widgets {}
            :kill-ring []
            :prev-kill-state nil}
   :viewport {:pos [0 0]
              :view-size [0 0]
              :metrics nil
              :focused? false}})

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
  (update-in state [:document :markup] intervals/add-markers markers))

(defn delete-markers [{{deleted-markers :deleted-markers :as document} :document :as state} marker-ids]
  (cond-> state
      (some? deleted-markers)
      (assoc-in [:document :deleted-markers] (atom (into @deleted-markers marker-ids)))))

(defn set-selection [state selection caret-offset]
  (-> state
      (assoc-in [:editor :selection] selection)
      (assoc-in [:editor :caret :offset] caret-offset)
      (move-view-if-needed)))

(defn document-insert-at-offset [{:andel/keys [text markup lexer] :as state} offset insertion]
  (let [text-length (text/text-length text)
        text' (-> text
                  (text/zipper)
                  (text/scan-to-offset offset)
                  (text/insert insertion)
                  (text/root))
        added-length (count insertion)]
    (-> state
        (assoc :andel/text text')
        (cond-> markup (update :andel/markup intervals/type-in offset added-length))
        (cond-> lexer (update :andel/lexer intervals/update-text (text/text->char-seq text') offset added-length 0))
        (update :andel/log (fn [l]
                             (conj (or l []) [[:retain offset] [:insert insertion] [:retain (- text-length offset)]]))))))

(defn insert-at-offset [state offset insertion]
  (let [[sel-from sel-to] (selection state)
        added-length (count insertion)
        caret-offset (caret-offset state)
        text-length (-> state :document :text text/text-length)]
    (-> state
        (edit-at-offset offset #(text/insert % insertion))
        (update :document (fn [{:keys [text lexer] :as document}]
                            (cond-> document
                              (some? lexer) (assoc :lexer (intervals/update-text lexer (text/text->char-seq text) offset added-length 0)))))
        (update-in [:document :markup] intervals/type-in offset (count insertion))
        (cond->
            (<= offset sel-from) (update-in [:editor :selection 0] #(+ % added-length))
            (<= offset caret-offset) (update-in [:editor :caret :offset] #(+ % added-length))
            (<= offset sel-to) (update-in [:editor :selection 1] #(+ % added-length)))
        (update :log (fn [l]
                       (conj (or l []) [[:retain offset] [:insert insertion] [:retain (- text-length offset)]]))))))


(defn document-delete-at-offset [{:andel/keys [text markup lexer] :as state} offset length]
  (let [text-length (text/text-length text)
        text-to-remove (-> (text/zipper text)
                           (text/scan-to-offset offset)
                           (text/text length))
        text' (-> text
                  (text/zipper)
                  (text/scan-to-offset offset)
                  (text/delete length)
                  (text/root))]
    (-> state
        (assoc :andel/text text')
        (cond-> markup (update :andel/markup intervals/delete-range offset length))
        (cond-> lexer (update :andel/lexer intervals/update-text (text/text->char-seq text') offset 0 length))
        (update :andel/log (fn [l]
                             (conj (or l []) [[:retain offset] [:delete text-to-remove] [:retain (- text-length offset length)]]))))))

(defn shift-caret [caret offset added-length]
  (if (<= offset caret) (max offset (+ caret added-length)) caret))

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
        (update :document (fn [{:keys [text] :as document}]
                            (cond-> document (some? (:lexer document))
                                    (update :lexer intervals/update-text (text/text->char-seq text) offset 0 length))))
        (update-in [:document :markup] intervals/delete-range offset length)
        (cond->
            (<= offset sel-from) (update-in [:editor :selection 0] #(max offset (- % length)))
            (<= offset caret-offset) (update-in [:editor :caret :offset] #(max offset (- % length)))
            (<= offset sel-to) (update-in [:editor :selection 1] #(max offset (- % length))))
        (update :log (fn [l]
                       (conj (or l []) [[:retain offset] [:delete old-text] [:retain (- text-length offset length)]]))))))

(defn text-at-offset [text offset length]
  (let [char-seq (text/text->char-seq text)]
    (.subSequence char-seq offset (+ offset length))))

(defn- next-widget-id [widgets]
  (or (some->> widgets
               keys
               (apply max)
               inc)
      0))

(defn add-widget [{{:keys [widgets]} :editor {:keys [text]} :document :as state} {:keys [element] :as widget}]
  (let [widget-id (next-widget-id widgets)]
    (assoc-in state [:editor :widgets widget-id] widget)))

(defn remove-widget [state widget-id]
  (update-in state [:editor :widgets] #(dissoc % widget-id)))

(defn play-operation [andel-document operation]
  (loop [i 0
         [[type x] & rest] operation
         andel-document andel-document]
    (if (some? type)
      (case type
        :insert (recur (+ i (count x)) rest (document-insert-at-offset andel-document i x))
        :retain (recur (+ i x) rest andel-document)
        :delete (recur i rest (document-delete-at-offset andel-document i (count x))))
      andel-document)))
