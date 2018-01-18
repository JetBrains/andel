(ns andel.paredit
  (:require [andel.parens :refer :all]
            [andel.core :as core]
            [andel.intervals :as intervals]
            [andel.controller :as controller])
  #?(:clj (:import [andel.cursor Cursor TransientCursor])))

(defn slurp-forward [{:keys [editor document] :as state}]
  (let [{:keys [text lexer]} document
        caret-offset (core/caret-offset state)
        paren-token? #(intervals/is-brace-token? lexer %)
        [cur-from cur-to] (enclosing-parens text paren-token? caret-offset) ;; check for not surrounded case
        [next-from next-to] (find-next-form text paren-token? (inc cur-to))
        paren-str (core/text-at-offset state cur-to 1)]
    (-> state
        (core/delete-at-offset cur-to 1)
        (core/insert-at-offset next-to paren-str))))

(defn slurp-backward [{:keys [editor document] :as state}]
  (let [{:keys [text lexer]} document
        caret-offset (core/caret-offset state)
        paren-token? #(intervals/is-brace-token? lexer %)
        [cur-from _] (enclosing-parens text paren-token? caret-offset) ;; check for not surrounded case
        [prev-from _] (find-prev-form text paren-token? (dec cur-from))
        paren-str (core/text-at-offset state cur-from 1)]
    (-> state
        (core/delete-at-offset cur-from 1)
        (core/insert-at-offset prev-from paren-str))))

(defn barf-backward [{:keys [editor document] :as state}]
  (let [{:keys [text lexer]} document
        caret-offset (core/caret-offset state)
        paren-token? #(intervals/is-brace-token? lexer %)
        [cur-from cur-to] (enclosing-parens text paren-token? caret-offset)
        [_ first-to] (find-next-form text paren-token? (inc cur-from))
        [second-from _] (find-next-form text paren-token? (inc first-to))
        paren-str (core/text-at-offset state cur-from 1)]
    (-> state
        (core/insert-at-offset (or second-from cur-to) paren-str)
        (core/delete-at-offset cur-from 1))))

(defn barf-forward [{:keys [editor document] :as state}]
  (let [{:keys [text lexer]} document
        caret-offset (core/caret-offset state)
        paren-token? #(intervals/is-brace-token? lexer %)
        [cur-from cur-to] (enclosing-parens text paren-token? caret-offset)
        [last-from _] (find-prev-form text paren-token? (dec cur-to))
        [_ prev-to] (find-prev-form text paren-token? (dec last-from))
        paren-str (core/text-at-offset state cur-to 1)
        prev-to-inc (some-> prev-to inc)]
    (-> state
        (core/delete-at-offset cur-to 1)
        (core/insert-at-offset (or prev-to-inc (inc cur-from)) paren-str))))

(defn splice [{:keys [editor document] :as state}]
  (let [{:keys [text lexer]} document
        caret-offset (core/caret-offset state)
        paren-token? #(intervals/is-brace-token? lexer %)
        [cur-from cur-to] (enclosing-parens text paren-token? caret-offset)]
    (-> state
        (core/delete-at-offset cur-to 1)
        (core/delete-at-offset cur-from 1))))

(defn splice-kill-left [{:keys [editor document] :as state}]
  (let [{:keys [text lexer]} document
        caret-offset (core/caret-offset state)
        paren-token? #(intervals/is-brace-token? lexer %)
        [cur-from cur-to] (enclosing-parens text paren-token? caret-offset)]
    (-> state
        (core/delete-at-offset cur-to 1)
        (core/delete-at-offset cur-from (- caret-offset cur-from)))))

(defn splice-kill-right [{:keys [editor document] :as state}]
  (let [{:keys [text lexer]} document
        caret-offset (core/caret-offset state)
        paren-token? #(intervals/is-brace-token? lexer %)
        [cur-from cur-to] (enclosing-parens text paren-token? caret-offset)]
    (-> state
        (core/delete-at-offset caret-offset (inc (- cur-to caret-offset)))
        (core/delete-at-offset cur-from 1))))

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
        paren-token? #(intervals/is-brace-token? lexer %)]
    (loop [offset (core/caret-offset state)]
      (let [[next-from next-to] (find-next-form text paren-token? offset)]
        (cond
          (or (not (some? next-from))
              (not (some? next-to)))
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
              (not (some? prev-to)))
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
