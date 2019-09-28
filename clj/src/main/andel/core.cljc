(ns ^:lean-ns andel.core
  (:require [andel.utils :as utils]
            [andel.text :as text]
            [andel.intervals :as intervals]))

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
  (update-in state [:editor :markup] intervals/insert markers))

(defn delete-markers [state marker-ids]
  (update-in state [:editor :markup] intervals/remove marker-ids))

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
      (some? markup) (assoc :markup (intervals/expand markup offset length))
      (some? error-stripes) (assoc :error-stripes (intervals/expand error-stripes offset length))
      (some? line-markers) (assoc :line-markers (intervals/expand line-markers offset length))
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
      (some? markup) (assoc :markup (intervals/collapse markup offset length))
      (some? error-stripes) (assoc :error-stripes (intervals/collapse error-stripes offset length))
      (some? line-markers) (assoc :line-markers (intervals/collapse line-markers offset length))
      (and (some? sel-from)     (<= offset sel-from)) (assoc-in [:selection 0] (max offset (- sel-from length)))
      (and (some? sel-to)       (<= offset sel-to)) (assoc-in [:selection 1] (max offset (- sel-to length)))
      (and (some? caret-offset) (<= offset caret-offset)) (assoc-in [:caret :offset] (max offset (- caret-offset length))))))

(defn insert-at-offset [state ^long offset insertion]
  (let [length      (text/codepoints-count insertion)]
    (-> state
        (edit-at-offset offset #(text/insert % insertion))
        (update-in [:document :markup] intervals/expand offset length)
        (update-in [:document :error-stripes] intervals/expand offset length)
        (update-in [:document :line-markers] intervals/expand offset length)
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
        (update-in [:document :markup] intervals/collapse offset length)
        (update-in [:document :error-stripes] intervals/collapse offset length)
        (update-in [:document :line-markers] intervals/collapse offset length)
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
