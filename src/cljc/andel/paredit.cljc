(ns andel.paredit
  (:require [andel.parens :refer :all]
            [andel.core :as core]
            [andel.text :as text]
            [andel.cursor :as cursor]
            [andel.intervals :as intervals]
            [andel.controller :as controller])
  #?(:clj (:import [andel.cursor Cursor TransientCursor])))

(defn slurp-forward [{:keys [editor document] :as state}]
  (let [{:keys [text]} document
        caret-offset (core/caret-offset state)
        paren-token? (paren-token? document)]
    (loop [[_ cur-to] (enclosing-parens text paren-token? caret-offset)]
      (if (some? cur-to)
        (if-let [[_ next-to] (find-next-form text paren-token? (inc cur-to))]
          (-> state
              (core/delete-at-offset cur-to 1)
              (core/insert-at-offset next-to (core/text-at-offset text cur-to 1)))
          (recur (enclosing-parens text paren-token? (inc cur-to))))
        state))))

(defn slurp-backward [{:keys [editor document] :as state}]
  (let [{:keys [text]} document
        caret-offset (core/caret-offset state)
        paren-token? (paren-token? document)]
    (loop [[cur-from _] (enclosing-parens text paren-token? caret-offset)]
      (if (some? cur-from)
        (if-let [[prev-from _] (find-prev-form text paren-token? (dec cur-from))]
          (-> state
            (core/delete-at-offset cur-from 1)
            (core/insert-at-offset prev-from (core/text-at-offset text cur-from 1)))
          (recur (enclosing-parens text paren-token? cur-from)))
        state))))

(defn barf-backward [{:keys [editor document] :as state}]
  (let [{:keys [text]} document
        caret-offset (core/caret-offset state)
        paren-token? (paren-token? document)]
    (if-let [[cur-from cur-to] (enclosing-parens text paren-token? caret-offset)]
      (let [[_ first-to] (find-next-form text paren-token? (inc cur-from))
            [second-from _] (find-next-form text paren-token? (inc first-to))
            paren-str (core/text-at-offset text cur-from 1)]
        (-> state
            (core/insert-at-offset (or second-from cur-to) paren-str)
            (core/delete-at-offset cur-from 1)))
      state)))

(defn barf-forward [{:keys [editor document] :as state}]
  (let [{:keys [text]} document
        caret-offset (core/caret-offset state)
        paren-token? (paren-token? document)]
    (if-let [[cur-from cur-to] (enclosing-parens text paren-token? caret-offset)]
      (let [[last-from _] (find-prev-form text paren-token? (dec cur-to))
            [_ prev-to] (find-prev-form text paren-token? (dec last-from))
            paren-str (core/text-at-offset text cur-to 1)
            prev-to-inc (some-> prev-to inc)]
        (-> state
            (core/delete-at-offset cur-to 1)
            (core/insert-at-offset (or prev-to-inc (inc cur-from)) paren-str)))
      state)))

(defn splice [{:keys [editor document] :as state}]
  (let [{:keys [text]} document
        caret-offset (core/caret-offset state)
        paren-token? (paren-token? document)]
    (if-let [[cur-from cur-to] (enclosing-parens text paren-token? caret-offset)]
      (-> state
          (core/delete-at-offset cur-to 1)
          (core/delete-at-offset cur-from 1))
      state)))

(defn splice-kill-left [{:keys [editor document] :as state}]
  (let [{:keys [text]} document
        caret-offset (core/caret-offset state)
        paren-token? (paren-token? document)]
    (if-let [[cur-from cur-to] (enclosing-parens text paren-token? caret-offset)]
      (let [kill-len (- caret-offset cur-from)
            killed-text (str (core/text-at-offset text (inc cur-from) (dec kill-len)))]
        (-> state
            (controller/put-to-clipboard killed-text)
            (core/delete-at-offset cur-to 1)
            (core/delete-at-offset cur-from kill-len)))
      state)))

(defn splice-kill-right [{:keys [editor document] :as state}]
  (let [{:keys [text]} document
        caret-offset (core/caret-offset state)
        paren-token? (paren-token? document)]
    (if-let [[cur-from cur-to] (enclosing-parens text paren-token? caret-offset)]
      (let [kill-len (- cur-to caret-offset)
            killed-text (str (core/text-at-offset text caret-offset kill-len))]
        (-> state
            (controller/put-to-clipboard killed-text)
            (core/delete-at-offset caret-offset (inc kill-len))
            (core/delete-at-offset cur-from 1)))
      state)))

(defn delete [{:keys [document] :as state}]
  (let [{:keys [text]} document
        [sel-from sel-to] (core/selection state)
        paren-token? (paren-token? document)
        caret-offset (core/caret-offset state)
        max-offset (text/text-length text)
        character (when (< caret-offset max-offset) (first (core/text-at-offset text caret-offset 1)))]
    (if (or (<= max-offset caret-offset)
            (not (paren-token? caret-offset))
            (< 0 (- sel-to sel-from)))
      (controller/delete state)
      (cond
        (closing? character)
        (if-let [[from-offset _] (find-matching-paren-backward text paren-token? caret-offset)]
          (if (< 1 (- caret-offset from-offset))
            state
            (core/delete-at-offset state from-offset 2))
          (controller/delete state))

        (opening? character)
        (if-let [[_ to-offset] (find-matching-paren-forward text paren-token? caret-offset)]
          (if (< 1 (- to-offset caret-offset))
            state
            (core/delete-at-offset state caret-offset 2))
          (controller/delete state))))))

(defn backspace [{:keys [document] :as state}]
  (let [{:keys [text]} document
        [sel-from sel-to] (core/selection state)
        paren-token? (paren-token? document)
        caret-offset (core/caret-offset state)
        deletion-offset (dec caret-offset)
        character (when (<= 0 deletion-offset) (first (core/text-at-offset text deletion-offset 1)))]
    (if (or (< deletion-offset 0)
            (< 0 (- sel-to sel-from))
            (not (paren-token? deletion-offset)))
      (controller/backspace state)
      (cond
        (closing? character)
        (if-let [[from-offset _] (find-matching-paren-backward text paren-token? deletion-offset)]
          (if (< 1 (- deletion-offset from-offset))
            (controller/move-caret state :left false)
            (core/delete-at-offset state from-offset 2))
          (controller/backspace state))

        (opening? character)
        (if-let [[_ to-offset] (find-matching-paren-forward text paren-token? deletion-offset)]
          (if (< 1 (- to-offset deletion-offset))
            (controller/move-caret state :left false)
            (core/delete-at-offset state deletion-offset 2))
          (controller/backspace state))))))

(defn open-round [state]
  (-> state
      (controller/type-in "()")
      (controller/move-caret :left false)))

(defn open-square [state]
  (-> state
      (controller/type-in "[]")
      (controller/move-caret :left false)))

(defn open-curly [state]
  (-> state
      (controller/type-in "{}")
      (controller/move-caret :left false)))

(defn navigate-next-form [{:keys [document] :as state} selection?]
  (let [{:keys [text]} document
        paren-token? (paren-token? document)
        caret-offset (core/caret-offset state)
        [_ next-to] (find-next-form text paren-token? caret-offset)
        [_ cur-to] (enclosing-parens text paren-token? caret-offset)]
    (cond (some? next-to) (controller/set-caret-at-offset state (inc next-to) selection?)
          (some? cur-to) (controller/set-caret-at-offset state (inc cur-to) selection?)
          :else state)))

(defn navigate-next-form-down [{:keys [document] :as state} selection?]
  (let [{:keys [text]} document
        paren-token? (paren-token? document)
        max-offset (dec (text/text-length text))]
    (loop [offset (core/caret-offset state)]
      (let [[next-from next-to] (find-next-form text paren-token? offset)]
        (cond
          (or (not (some? next-from))
              (not (some? next-to))
              (<= max-offset offset))
          state

          (and (paren-token? next-from)
               (paren-token? next-to))
          (controller/set-caret-at-offset state (inc next-from) selection?)

          :else
          (recur (inc next-to)))))))

(defn navigate-next-form-up [{:keys [document] :as state} selection?]
  (let [{:keys [text]} document
        paren-token? (paren-token? document)
        caret-offset (core/caret-offset state)
        [_ cur-to] (enclosing-parens text paren-token? caret-offset)]
    (if (some? cur-to)
      (controller/set-caret-at-offset state (inc cur-to) selection?)
      state)))

(defn navigate-prev-form [{:keys [document] :as state} selection?]
  (let [{:keys [text]} document
        paren-token? (paren-token? document)
        caret-offset (core/caret-offset state)
        [prev-from _] (find-prev-form text paren-token? (max 0 (dec caret-offset)))
        [cur-from _] (enclosing-parens text paren-token? caret-offset)]
    (cond (some? prev-from) (controller/set-caret-at-offset state prev-from selection?)
          (some? cur-from) (controller/set-caret-at-offset state cur-from selection?)
          :else state)))

(defn navigate-prev-form-down [{:keys [document] :as state} selection?]
  (let [{:keys [text]} document
        paren-token? (paren-token? document)]
    (loop [offset (dec (core/caret-offset state))]
      (let [[prev-from prev-to] (find-prev-form text paren-token? offset)]
        (cond
          (or (not (some? prev-from))
              (not (some? prev-to))
              (= 0 offset))
          state

          (and (paren-token? prev-from)
               (paren-token? prev-to))
          (controller/set-caret-at-offset state prev-to selection?)

          :else
          (recur (dec prev-from)))))))

(defn navigate-prev-form-up [{:keys [document] :as state} selection?]
  (let [{:keys [text]} document
        paren-token? (paren-token? document)
        caret-offset (core/caret-offset state)
        [cur-from _] (enclosing-parens text paren-token? caret-offset)]
    (if (some? cur-from)
      (controller/set-caret-at-offset state cur-from selection?)
      state)))

(defn kill-form [{:keys [document editor] :as state} id]
  (let [{:keys [text]} document
        prev-was-kill? (= id (some-> editor :prev-op-ids :kill inc))
        caret-offset (core/caret-offset state)
        paren-token? (paren-token? document)
        [next-from next-to] (find-next-form text paren-token? caret-offset)]
    (if (some? (and next-from next-to))
      (let [kill-len (inc (- next-to caret-offset))
            killed-text (str (core/text-at-offset text caret-offset kill-len))]
        (-> state
            (assoc-in [:editor :prev-op-ids :kill] id)
            (core/delete-at-offset caret-offset kill-len)
            (cond-> prev-was-kill? (controller/conj-to-clipboard killed-text)
              (not prev-was-kill?) (controller/put-to-clipboard killed-text))))
      state)))

(defn yank [{:keys [document editor] :as state}]
  (let [{:keys [clipboard]} editor]
    (controller/type-in state (:content clipboard))))

(defn kill [{:keys [document editor] :as state} id]
  (let [{:keys [text]} document
        prev-was-kill? (= id (some-> editor :prev-op-ids :kill inc))
        caret-offset (core/caret-offset state)
        caret-line (andel.utils/offset->line caret-offset text)
        paren-token? (paren-token? document)
        delete-to (loop [[from to] (find-next-form text paren-token? caret-offset)]
                    (when (some? to)
                      (let [cur-line (andel.utils/offset->line to text)]
                        (if (< caret-line cur-line)
                          to
                          (if-let [[next-from next-to :as range] (find-next-form text paren-token? (inc to))]
                            (let [next-line (andel.utils/offset->line next-from text)]
                              (if (< caret-line next-line)
                                to
                                (recur range)))
                            to)))))]
    (if (some? delete-to)
      (let [kill-len (inc (- delete-to caret-offset))
            killed-text (str (core/text-at-offset text caret-offset kill-len))]
        (-> state
            (assoc-in [:editor :prev-op-ids :kill] id)
            (core/delete-at-offset caret-offset kill-len)
            (cond-> prev-was-kill? (controller/conj-to-clipboard killed-text)
              (not prev-was-kill?) (controller/put-to-clipboard killed-text))))
      state)))