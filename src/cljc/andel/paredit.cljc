(ns andel.paredit
  (:require [andel.parens :refer :all]
            [andel.core :as core]
            [andel.text :as text]
            [andel.cursor :as cursor]
            [andel.intervals :as intervals]
            [andel.controller :as controller])
  #?(:clj (:import [andel.cursor Cursor TransientCursor])))

(defn slurp-forward [{:keys [editor document] :as state}]
  (let [{:keys [text lexer]} document
        caret-offset (core/caret-offset state)
        paren-token? #(intervals/is-brace-token? lexer %)]
    (if-let [[_ cur-to] (enclosing-parens text paren-token? caret-offset)]
      (let [[_ next-to] (find-next-form text paren-token? (inc cur-to))
            paren-str (core/text-at-offset state cur-to 1)]
        (-> state
            (core/delete-at-offset cur-to 1)
            (core/insert-at-offset next-to paren-str)))
      state)))

(defn slurp-backward [{:keys [editor document] :as state}]
  (let [{:keys [text lexer]} document
        caret-offset (core/caret-offset state)
        paren-token? #(intervals/is-brace-token? lexer %)]
    (if-let [[cur-from _] (enclosing-parens text paren-token? caret-offset)]
      (let [[prev-from _] (find-prev-form text paren-token? (dec cur-from))
            paren-str (core/text-at-offset state cur-from 1)]
        (-> state
            (core/delete-at-offset cur-from 1)
            (core/insert-at-offset prev-from paren-str)))
      state)))

(defn barf-backward [{:keys [editor document] :as state}]
  (let [{:keys [text lexer]} document
        caret-offset (core/caret-offset state)
        paren-token? #(intervals/is-brace-token? lexer %)]
    (if-let [[cur-from cur-to] (enclosing-parens text paren-token? caret-offset)]
      (let [[_ first-to] (find-next-form text paren-token? (inc cur-from))
            [second-from _] (find-next-form text paren-token? (inc first-to))
            paren-str (core/text-at-offset state cur-from 1)]
        (-> state
            (core/insert-at-offset (or second-from cur-to) paren-str)
            (core/delete-at-offset cur-from 1)))
      state)))

(defn barf-forward [{:keys [editor document] :as state}]
  (let [{:keys [text lexer]} document
        caret-offset (core/caret-offset state)
        paren-token? #(intervals/is-brace-token? lexer %)]
    (if-let [[cur-from cur-to] (enclosing-parens text paren-token? caret-offset)]
      (let [[last-from _] (find-prev-form text paren-token? (dec cur-to))
            [_ prev-to] (find-prev-form text paren-token? (dec last-from))
            paren-str (core/text-at-offset state cur-to 1)
            prev-to-inc (some-> prev-to inc)]
        (-> state
            (core/delete-at-offset cur-to 1)
            (core/insert-at-offset (or prev-to-inc (inc cur-from)) paren-str)))
      state)))

(defn splice [{:keys [editor document] :as state}]
  (let [{:keys [text lexer]} document
        caret-offset (core/caret-offset state)
        paren-token? #(intervals/is-brace-token? lexer %)]
    (if-let [[cur-from cur-to] (enclosing-parens text paren-token? caret-offset)]
      (-> state
          (core/delete-at-offset cur-to 1)
          (core/delete-at-offset cur-from 1))
      state)))

(defn splice-kill-left [{:keys [editor document] :as state}]
  (let [{:keys [text lexer]} document
        caret-offset (core/caret-offset state)
        paren-token? #(intervals/is-brace-token? lexer %)]
    (if-let [[cur-from cur-to] (enclosing-parens text paren-token? caret-offset)]
      (-> state
          (core/delete-at-offset cur-to 1)
          (core/delete-at-offset cur-from (- caret-offset cur-from)))
      state)))

(defn splice-kill-right [{:keys [editor document] :as state}]
  (let [{:keys [text lexer]} document
        caret-offset (core/caret-offset state)
        paren-token? #(intervals/is-brace-token? lexer %)]
    (if-let [[cur-from cur-to] (enclosing-parens text paren-token? caret-offset)]
      (-> state
          (core/delete-at-offset caret-offset (inc (- cur-to caret-offset)))
          (core/delete-at-offset cur-from 1))
      state)))

(defn delete [{:keys [document] :as state}]
  (let [{:keys [text lexer]} document
        [sel-from sel-to] (core/selection state)
        paren-token? #(intervals/is-brace-token? lexer %)
        caret-offset (core/caret-offset state)
        character (first (core/text-at-offset state caret-offset 1))]
    (if (or (not (paren-token? caret-offset))
            (< 0 (- sel-to sel-from)))
      (controller/delete state)
      (cond
        (closing? character)
        (let [[from-offset _] (find-matching-paren-backward text paren-token? caret-offset)]
          (if (< 1 (- caret-offset from-offset))
            state
            (core/delete-at-offset state from-offset 2)))

        (opening? character)
        (let [[_ to-offset] (find-matching-paren-forward text paren-token? caret-offset)]
          (if (< 1 (- to-offset caret-offset))
            state
            (core/delete-at-offset state caret-offset 2)))))))

(defn backspace [{:keys [document] :as state}]
  (let [{:keys [text lexer]} document
        [sel-from sel-to] (core/selection state)
        paren-token? #(intervals/is-brace-token? lexer %)
        caret-offset (core/caret-offset state)
        deletion-offset (dec caret-offset)
        character (first (core/text-at-offset state deletion-offset 1))]
    (if (or (not (paren-token? deletion-offset))
            (< 0 (- sel-to sel-from))
            (< deletion-offset 0))
      (controller/backspace state)
      (cond
        (closing? character)
        (let [[from-offset _] (find-matching-paren-backward text paren-token? deletion-offset)]
          (if (< 1 (- deletion-offset from-offset))
            (controller/move-caret state :left false)
            (core/delete-at-offset state from-offset 2)))

        (opening? character)
        (let [[_ to-offset] (find-matching-paren-forward text paren-token? deletion-offset)]
          (if (< 1 (- to-offset deletion-offset))
            (controller/move-caret state :left false)
            (core/delete-at-offset state deletion-offset 2)))))))

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

;; temporary
(defn- set-caret-and-drop-selection [state offset]
  (-> state
      (assoc-in [:editor :caret :offset] offset)
      (assoc-in [:editor :v-col] 0)
      (assoc-in [:editor :selection] [offset offset])))

(defn navigate-next-form [{:keys [document] :as state}]
  (let [{:keys [text lexer]} document
        paren-token? #(intervals/is-brace-token? lexer %)
        caret-offset (core/caret-offset state)
        [_ next-to] (find-next-form text paren-token? caret-offset)
        [_ cur-to] (enclosing-parens text paren-token? caret-offset)]
    (cond (some? next-to) (set-caret-and-drop-selection state (inc next-to))
          (some? cur-to) (set-caret-and-drop-selection state (inc cur-to))
          :else state)))

(defn navigate-next-form-down [{:keys [document] :as state}]
  (let [{:keys [text lexer]} document
        paren-token? #(intervals/is-brace-token? lexer %)
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
          (set-caret-and-drop-selection state (inc next-from))

          :else
          (recur (inc next-to)))))))

(defn navigate-next-form-up [{:keys [document] :as state}]
  (let [{:keys [text lexer]} document
        paren-token? #(intervals/is-brace-token? lexer %)
        caret-offset (core/caret-offset state)
        [_ cur-to] (enclosing-parens text paren-token? caret-offset)]
    (if (some? cur-to)
      (set-caret-and-drop-selection state (inc cur-to))
      state)))

(defn navigate-prev-form [{:keys [document] :as state}]
  (let [{:keys [text lexer]} document
        paren-token? #(intervals/is-brace-token? lexer %)
        caret-offset (core/caret-offset state)
        [prev-from _] (find-prev-form text paren-token? (max 0 (dec caret-offset)))
        [cur-from _] (enclosing-parens text paren-token? caret-offset)]
    (cond (some? prev-from) (set-caret-and-drop-selection state prev-from)
          (some? cur-from) (set-caret-and-drop-selection state cur-from)
          :else state)))

(defn navigate-prev-form-down [{:keys [document] :as state}]
  (let [{:keys [text lexer]} document
        paren-token? #(intervals/is-brace-token? lexer %)]
    (loop [offset (dec (core/caret-offset state))]
      (let [[prev-from prev-to] (find-prev-form text paren-token? offset)]
        (cond
          (or (not (some? prev-from))
              (not (some? prev-to))
              (= 0 offset))
          state

          (and (paren-token? prev-from)
               (paren-token? prev-to))
          (set-caret-and-drop-selection state prev-to)

          :else
          (recur (dec prev-from)))))))

(defn navigate-prev-form-up [{:keys [document] :as state}]
  (let [{:keys [text lexer]} document
        paren-token? #(intervals/is-brace-token? lexer %)
        caret-offset (core/caret-offset state)
        [cur-from _] (enclosing-parens text paren-token? caret-offset)]
    (if (some? cur-from)
      (set-caret-and-drop-selection state cur-from)
      state)))

(defn navigate-line-start [{:keys [document] :as state}]
  (let [{:keys [text lexer]} document
        caret-offset (dec (core/caret-offset state))
        cursor (cursor/make-cursor text caret-offset)
        line-start-cursor (cursor/next (cursor/move-while cursor #(not= \newline %) :backward))
        text-start-cursor (cursor/move-while line-start-cursor #(or (= % \space)
                                                                    (= % \tab)) :forward)
        line-start-offset (cursor/offset line-start-cursor)
        text-start-offset (cursor/offset text-start-cursor)]
    (if (and (<= line-start-offset caret-offset)
             (< caret-offset text-start-offset))
      (set-caret-and-drop-selection state line-start-offset)
      (set-caret-and-drop-selection state text-start-offset))))

(defn- last-command-kill? [editor]
  false)

(defn- push-kill-ring [editor killed-text]
  (let [kill-ring-last (dec (count (:kill-ring editor)))]
    (if (last-command-kill? editor)
      (update-in editor [:kill-ring kill-ring-last] #(str % killed-text))
      (update-in editor [:kill-ring] #(conj % killed-text)))))

(defn- pop-kill-ring [editor]
  (let [killed-text (peek (:kill-ring editor))
        editor' (update editor :kill-ring pop)]
    [killed-text editor']))

(defn- peek-kill-ring [editor]
  (peek (:kill-ring editor)))

(defn yank [{:keys [document editor] :as state}]
  (let [yanked-text (peek-kill-ring editor)
        caret-offset (core/caret-offset state)]
    (if (some? yanked-text)
      (core/insert-at-offset state caret-offset yanked-text)
      state)))

(defn yank-and-pop [{:keys [document editor] :as state}]
  (let [[yanked-text editor'] (pop-kill-ring editor)
        caret-offset (core/caret-offset state)]
    (if (some? yanked-text)
      (-> state
          (assoc :editor editor')
          (core/insert-at-offset caret-offset yanked-text))
      state)))

(defn kill-form [{:keys [document editor] :as state}]
  (let [{:keys [text lexer]} document
        caret-offset (core/caret-offset state)
        paren-token? #(intervals/is-brace-token? lexer %)
        [next-from next-to] (find-next-form text paren-token? caret-offset)]
    (if (some? (and next-from next-to))
      (let [kill-len (inc (- next-to caret-offset))
            killed-text (core/text-at-offset state caret-offset kill-len)]
        (-> state
            (core/delete-at-offset caret-offset kill-len)
            (update :editor #(push-kill-ring % killed-text))))
      state)))
