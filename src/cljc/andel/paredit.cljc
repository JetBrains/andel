(ns andel.paredit
  (:require [andel.parens :refer :all]
            [andel.core :as core]
            [andel.text :as text]
            [andel.cursor :as cursor]
            [andel.intervals :as intervals]
            [andel.controller :as controller])
  #?(:clj (:import [andel.cursor Cursor TransientCursor])))

(defn get-char [text offset]
  (let [max-offset (text/text-length text)]
    (when (and (<= 0 offset)
               (< offset max-offset))
      (first (core/text-at-offset text offset 1)))))

(defn slurp-forward [{:keys [editor document] :as state}]
  (let [{:keys [text]} document
        caret-offset (core/caret-offset state)
        paren-token? (paren-token? document)]
    (loop [[_ cur-to] (enclosing-parens text paren-token? caret-offset)]
      (if (some? cur-to)
        (if-let [[_ next-to] (find-next-form text paren-token? (inc cur-to))]
          (-> state
              (core/delete-at-offset cur-to 1)
              (core/insert-at-offset next-to (str (get-char text cur-to))))
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
            (core/insert-at-offset prev-from (str (get-char text cur-from))))
          (recur (enclosing-parens text paren-token? cur-from)))
        state))))

(defn barf-backward [{:keys [editor document] :as state}]
  (let [{:keys [text]} document
        caret-offset (core/caret-offset state)
        paren-token? (paren-token? document)]
    (if-let [[cur-from cur-to] (enclosing-parens text paren-token? caret-offset)]
      (let [[_ first-to] (find-next-form text paren-token? (inc cur-from))
            [second-from _] (find-next-form text paren-token? (inc first-to))
            paren-str (str (get-char text cur-from))]
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
            paren-str (str (get-char text cur-to))
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

(defn quoted? [text offset]
  (if (= \\ (get-char text (dec offset)))
    (not (quoted? text (dec offset)))
    false))

(defn- delete-aux [{:keys [document] :as state} offset op]
  (let [paren-token? (paren-token? document)
        fallback (case op :delete controller/delete :backspace controller/backspace)
        text (:text document)
        character (get-char text offset)]
    (if (not (paren-token? offset))
      (cond
        (and (= character \")
             (= \" (get-char text (dec offset)))
             (not (quoted? text (dec offset))))
        (core/delete-at-offset state (dec offset) 2)
        (and (= character \")
             (= \" (get-char text (inc offset)))
             (not (quoted? text (inc offset))))
        (core/delete-at-offset state offset 2)
        :else
        (fallback state))
      (cond
        (closing? character)
        (if-let [[from-offset _] (find-matching-paren-backward text paren-token? offset)]
          (if (< 1 (- offset from-offset))
            (if (= :backspace op)
              (controller/move-caret state :left false)
              state)
            (core/delete-at-offset state from-offset 2))
          (fallback state))
        (opening? character)
        (if-let [[_ to-offset] (find-matching-paren-forward text paren-token? offset)]
          (if (< 1 (- to-offset offset))
            (if (= :delete op)
              (controller/move-caret state :right false)
              state)
            (core/delete-at-offset state offset 2))
          (fallback state))
        :else
        (fallback state)))))

(defn delete [{:keys [document] :as state}]
  (let [{:keys [text]} document
        [sel-from sel-to] (core/selection state)
        paren-token? (paren-token? document)
        caret-offset (core/caret-offset state)
        character (get-char text caret-offset)]
    (if (or (< 0 (- sel-to sel-from))
            (<= (text/text-length text) caret-offset))
      (controller/delete state)
      (delete-aux state caret-offset :delete))))

(defn backspace [{:keys [document] :as state}]
  (let [{:keys [text]} document
        [sel-from sel-to] (core/selection state)
        paren-token? (paren-token? document)
        caret-offset (core/caret-offset state)
        deletion-offset (dec caret-offset)
        character (get-char text deletion-offset)]
    (if (or (< 0 (- sel-to sel-from))
            (< deletion-offset 0))
      (controller/backspace state)
      (delete-aux state deletion-offset :backspace))))

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