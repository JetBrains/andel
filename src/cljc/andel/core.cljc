(ns andel.core
  (:require [clojure.core.async :as a]
            [andel.utils :as utils]
            [andel.text :as text]
            [andel.intervals :as intervals]
            [clojure.spec.alpha :as s]
            [andel.tree :as tree])
  (:import [andel Rope$Tree]))

(s/def :andel/tree #(instance? Rope$Tree %))
(s/def :andel/text :andel/tree)
(s/def :andel/markup any?)
(s/def :andel/lexer any?)
(s/def :andel/document (s/keys :req [:andel/text
                                     :andel/markup
                                     :andel/lexer]))
(s/def :andel/widgets (s/map-of nat-int? map?))

(defn make-editor-state []
  {:document {:text (text/make-text "")
              :markup intervals/empty-tree
              :error-stripes intervals/empty-tree
              :line-markers intervals/empty-tree}
   :editor {:caret {:offset 0 :v-col 0}
            :selection [0 0]
            :widgets {}
            :clipboard {:content nil :timestamp 0}}
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

(defn caret->offset [{:keys [offset] :as caret}] offset)

(defn insert-markers [state markers]
  (update-in state [:editor :markup] intervals/add-markers markers))

(defn delete-markers [state marker-ids]
  (update-in state [:editor :markup] intervals/gc marker-ids))

(defn set-selection [state selection caret-offset]
  (-> state
      (assoc-in [:editor :selection] selection)
      (assoc-in [:editor :caret :offset] caret-offset)))

(defn insert-at-editor [editor {:keys [offset length]}]
  (let [[sel-from sel-to] (get editor :selection)
        caret-offset (get-in editor [:caret :offset])
        markup (:markup editor)
        error-stripes (:error-stripes editor)
        line-markers (:line-markers editor)]
    (cond-> editor
      (some? markup) (assoc :markup (intervals/type-in markup offset length))
      (some? error-stripes) (assoc :error-stripes (intervals/type-in error-stripes offset length))
      (some? line-markers) (assoc :line-markers (intervals/type-in line-markers offset length))
      (and (some? sel-from)     (<= offset sel-from)) (assoc-in [:selection 0] (+ sel-from length))
      (and (some? sel-to)       (<= offset sel-to)) (assoc-in [:selection 1] (+ sel-to length))
      (and (some? caret-offset) (<= offset caret-offset)) (assoc-in [:caret :offset] (+ caret-offset length)))))

(defn delete-at-editor [editor {:keys [offset length]}]
  (let [[sel-from sel-to] (get editor :selection)
        caret-offset (get-in editor [:caret :offset])
        markup (:markup editor)
        error-stripes (:error-stripes editor)
        line-markers (:line-markers editor)]
    (cond-> editor
      (some? markup) (assoc :markup (intervals/delete-range markup offset length))
      (some? error-stripes) (assoc :error-stripes (intervals/delete-range error-stripes offset length))
      (some? line-markers) (assoc :line-markers (intervals/delete-range line-markers offset length))
      (and (some? sel-from)     (<= offset sel-from)) (assoc-in [:selection 0] (max offset (- sel-from length)))
      (and (some? sel-to)       (<= offset sel-to)) (assoc-in [:selection 1] (max offset (- sel-to length)))
      (and (some? caret-offset) (<= offset caret-offset)) (assoc-in [:caret :offset] (max offset (- caret-offset length))))))

(defn insert-at-offset [state ^long offset insertion]
  (let [length      (text/codepoints-count insertion)]
    (-> state
        (edit-at-offset offset #(text/insert % insertion))
        (update :document (fn [{:keys [text] :as document}]
                            (cond-> document (some? (:lexer document))
                                    (update :lexer intervals/update-text text offset))))
        (update-in [:document :markup] intervals/type-in offset length)
        (update-in [:document :error-stripes] intervals/type-in offset length)
        (update-in [:document :line-markers] intervals/type-in offset length)
        (cond->
          (some? (:editor state))
          (update :editor insert-at-editor {:offset offset :length length})

          (some? (:sibling-editors state))
          (update :sibling-editors
                  (fn [sibs]
                    (into {} (map (fn [[id editor]]
                                    [id (insert-at-editor editor {:offset offset :length length})])) sibs))))
        (update :log (fn [l]
                       (let [text        (:text (:document state))
                             char-offset (text/offset->char-offset text offset)
                             chars-count (text/chars-count text)]
                         (conj (or l []) [[:retain char-offset] [:insert insertion] [:retain (- chars-count char-offset)]])))))))

(defn delete-at-offset [state ^long offset length]
  (let [text (-> state :document :text)]
    (-> state
        (edit-at-offset offset #(text/delete % length))
        (update :document (fn [{:keys [text] :as document}]
                            (cond-> document (some? (:lexer document))
                                    (update :lexer intervals/update-text text offset))))
        (update-in [:document :markup] intervals/delete-range offset length)
        (update-in [:document :error-stripes] intervals/delete-range offset length)
        (update-in [:document :line-markers] intervals/delete-range offset length)
        (cond->
          (some? (:editor state))
          (update :editor delete-at-editor {:offset offset :length length})

          (some? (:sibling-editors state))
          (update :sibling-editors
                  (fn [sibs]
                    (into {} (map (fn [[id editor]]
                                    [id (delete-at-editor editor {:offset offset :length length})])) sibs))))
        (update :log (fn [l]
                       (let [from-loc (-> (text/zipper text)
                                          (text/scan-to-offset offset))
                             old-text (text/text from-loc length)
                             total-chars-count (text/chars-count text)
                             from-char (text/char-offset from-loc)]
                         (conj (or l []) [[:retain from-char] [:delete old-text] [:retain (- total-chars-count from-char (count old-text))]])))))))

(defn text-at-offset ^String [text ^long offset ^long length]
  (-> (text/zipper text)
      (text/scan-to-offset offset)
      (text/text length)))

(defn play-operation [widget operation]
  (loop [i 0
         [[type x] & rest] operation
         widget widget]
    (if (some? type)
      (let [text (:text (:document widget))]
        (case type
          :insert (recur (+ i (count x)) rest (insert-at-offset widget (text/char-offset->offset text i) x))
          :retain (recur (+ i ^long x) rest widget)
          :delete (recur i rest (delete-at-offset widget (text/char-offset->offset text i) (text/codepoints-count x)))))
      widget)))
