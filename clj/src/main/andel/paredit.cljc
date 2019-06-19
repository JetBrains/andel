(ns andel.paredit
  (:require [andel.parens :as parens]
            [andel.core :as core]
            [andel.text :as text]
            [andel.intervals :as intervals]
            [andel.controller :as controller]))

(defn slurp-forward [{:keys [editor document] :as state}]
  (let [{:keys [text]} document
        caret-offset (core/caret-offset state)
        paren-token? (parens/mk-paren-token? document)]
    (loop [[_ cur-to] (parens/enclosing-parens text paren-token? caret-offset)]
      (if (some? cur-to)
        (if-let [next-to (second (parens/find-next-form text paren-token? (inc cur-to)))]
          (-> state
              (core/delete-at-offset cur-to 1)
              (core/insert-at-offset next-to (str (parens/get-char text cur-to))))
          (recur (parens/enclosing-parens text paren-token? (inc cur-to))))
        state))))

(defn slurp-backward [{:keys [editor document] :as state}]
  (let [{:keys [text]} document
        caret-offset (core/caret-offset state)
        paren-token? (parens/mk-paren-token? document)]
    (loop [[cur-from _] (parens/enclosing-parens text paren-token? caret-offset)]
      (if (and (some? cur-from) (< 0 cur-from))
        (if-let [[prev-from _] (parens/find-prev-form text paren-token? (dec cur-from))]
          (-> state
            (core/delete-at-offset cur-from 1)
            (core/insert-at-offset prev-from (str (parens/get-char text cur-from))))
          (recur (parens/enclosing-parens text paren-token? cur-from)))
        state))))

(defn barf-backward [{:keys [editor document] :as state}]
  (let [{:keys [text]} document
        caret-offset (core/caret-offset state)
        paren-token? (parens/mk-paren-token? document)]
    (if-let [[cur-from cur-to] (parens/enclosing-parens text paren-token? caret-offset)]
      (let [[_ first-to] (parens/find-next-form text paren-token? (inc cur-from))
            [second-from _] (parens/find-next-form text paren-token? (inc first-to))
            paren-str (str (parens/get-char text cur-from))]
        (-> state
            (core/insert-at-offset (or second-from cur-to) paren-str)
            (core/delete-at-offset cur-from 1)))
      state)))

(defn barf-forward [{:keys [editor document] :as state}]
  (let [{:keys [text]} document
        caret-offset (core/caret-offset state)
        paren-token? (parens/mk-paren-token? document)]
    (if-let [[cur-from cur-to] (parens/enclosing-parens text paren-token? caret-offset)]
      (let [[last-from _] (parens/find-prev-form text paren-token? (dec cur-to))
            [_ prev-to] (parens/find-prev-form text paren-token? (dec last-from))
            paren-str (str (parens/get-char text cur-to))
            prev-to-inc (some-> prev-to inc)]
        (-> state
            (core/delete-at-offset cur-to 1)
            (core/insert-at-offset (or prev-to-inc (inc cur-from)) paren-str)))
      state)))

(defn splice [{:keys [editor document] :as state}]
  (let [{:keys [text]} document
        caret-offset (core/caret-offset state)
        paren-token? (parens/mk-paren-token? document)]
    (if-let [[cur-from cur-to] (parens/enclosing-parens text paren-token? caret-offset)]
      (-> state
          (core/delete-at-offset cur-to 1)
          (core/delete-at-offset cur-from 1))
      state)))

(defn splice-kill-left [{:keys [editor document] :as state}]
  (let [{:keys [text]} document
        caret-offset (core/caret-offset state)
        paren-token? (parens/mk-paren-token? document)]
    (if-let [[cur-from cur-to] (parens/enclosing-parens text paren-token? caret-offset)]
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
        paren-token? (parens/mk-paren-token? document)]
    (if-let [[cur-from cur-to] (parens/enclosing-parens text paren-token? caret-offset)]
      (let [kill-len (- cur-to caret-offset)
            killed-text (str (core/text-at-offset text caret-offset kill-len))]
        (-> state
            (controller/put-to-clipboard killed-text)
            (core/delete-at-offset caret-offset (inc kill-len))
            (core/delete-at-offset cur-from 1)))
      state)))

(defn- delete-aux [{:keys [document] :as state} ^long offset op]
  (let [paren-token? (parens/mk-paren-token? document)
        fallback (case op :delete controller/delete :backspace controller/backspace)
        text (:text document)
        character (parens/get-char text offset)
        char-offset (text/offset->char-offset text offset)]
    (if (not (and (parens/paren? (long character))
                  (paren-token? char-offset)))
      (cond
        (and (= character \")
             (< 0 offset)
             (= \" (parens/get-char text (dec offset)))
             (not (parens/quoted? text (dec offset))))
        (core/delete-at-offset state (dec offset) 2)
        (and (= character \")
             (< (inc offset) (text/text-length text))
             (= \" (parens/get-char text (inc offset)))
             (not (parens/quoted? text (inc offset))))
        (core/delete-at-offset state offset 2)
        :else
        (fallback state))
      (cond
        (parens/closing? (int character))
        (if-let [from-offset (parens/find-matching-paren-backward text paren-token? offset)]
          (if (< 1 (- offset from-offset))
            (if (= :backspace op)
              (controller/move-caret state :left false)
              state)
            (core/delete-at-offset state from-offset 2))
          (fallback state))
        (parens/opening? (int character))
        (if-let [to-offset (parens/find-matching-paren-forward text paren-token? offset)]
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
        paren-token? (parens/mk-paren-token? document)
        caret-offset (core/caret-offset state)]
    (if (or (< 0 (- sel-to sel-from))
            (<= (text/text-length text) caret-offset))
      (controller/delete state)
      (delete-aux state caret-offset :delete))))

(defn backspace [{:keys [document] :as state}]
  (let [{:keys [text]} document
        [sel-from sel-to] (core/selection state)
        paren-token? (parens/mk-paren-token? document)
        caret-offset (core/caret-offset state)
        deletion-offset (dec caret-offset)]
    (if (or (< 0 (- sel-to sel-from))
            (< deletion-offset 0))
      (controller/backspace state)
      (delete-aux state deletion-offset :backspace))))

(defn insert-opening-paren [state [l r :as parens-pair]]
  (let [state' (controller/type-in state l)
        insertion-offset (core/caret-offset state)
        paren-token? (parens/mk-paren-token? (:document state'))
        insertion-char-offset (text/offset->char-offset (:text (:document state')) insertion-offset)]
    (if (paren-token? insertion-char-offset)
      (-> state'
          (controller/type-in r)
          (controller/move-caret :left false))
      state')))

(defn insert-closing-paren [state [l r :as parens-pair]]
  (let [state' (controller/type-in state r)
        insertion-offset (core/caret-offset state)
        paren-token? (parens/mk-paren-token? (:document state'))
        insertion-char-offset (text/offset->char-offset (:text (:document state')) insertion-offset)]
    (if (paren-token? insertion-char-offset)
      (-> state'
          (controller/move-caret :left false)
          (controller/type-in l))
      state')))

(defn open-string [state]
  (-> state
      (controller/type-in "\"\"")
      (controller/move-caret :left false)))

(defn navigate-next-form [{:keys [document] :as state} selection?]
  (let [{:keys [text]} document
        paren-token? (parens/mk-paren-token? document)
        caret-offset (core/caret-offset state)
        [_ next-to] (parens/find-next-form text paren-token? caret-offset)
        [_ cur-to] (parens/enclosing-parens text paren-token? caret-offset)]
    (cond (some? next-to) (controller/set-caret-at-offset state (inc next-to) selection?)
          (some? cur-to) (controller/set-caret-at-offset state (inc cur-to) selection?)
          :else state)))

(defn navigate-next-form-down [{:keys [document] :as state} selection?]
  (let [{:keys [text]} document
        paren-token? (parens/mk-paren-token? document)
        max-offset (dec (text/text-length text))]
    (loop [offset (core/caret-offset state)]
      (let [[next-from next-to] (parens/find-next-form text paren-token? offset)]
        (if (or (not (some? next-from))
                (not (some? next-to))
                (<= max-offset offset))
          state
          (let [next-from-chars (text/offset->char-offset text next-from)
                next-to-chars (text/offset->char-offset text next-to)]
            (if (and (parens/paren? (long (parens/get-char text next-from)))
                     (parens/paren? (long (parens/get-char text next-to)))
                     (paren-token? next-from-chars)
                     (paren-token? next-to-chars))
              (controller/set-caret-at-offset state (inc next-from) selection?)
              (recur (inc next-to)))))))))

(defn navigate-next-form-up [{:keys [document] :as state} selection?]
  (let [{:keys [text]} document
        paren-token? (parens/mk-paren-token? document)
        caret-offset (core/caret-offset state)
        [_ cur-to] (parens/enclosing-parens text paren-token? caret-offset)]
    (if (some? cur-to)
      (controller/set-caret-at-offset state (inc cur-to) selection?)
      state)))

(defn navigate-prev-form [{:keys [document] :as state} selection?]
  (let [{:keys [text]} document
        paren-token? (parens/mk-paren-token? document)
        caret-offset (core/caret-offset state)
        [prev-from _] (parens/find-prev-form text paren-token? (max 0 (dec caret-offset)))
        [cur-from _] (parens/enclosing-parens text paren-token? caret-offset)]
    (cond (some? prev-from) (controller/set-caret-at-offset state prev-from selection?)
          (some? cur-from) (controller/set-caret-at-offset state cur-from selection?)
          :else state)))

(defn navigate-prev-form-down [{:keys [document] :as state} selection?]
  (let [{:keys [text]} document
        paren-token? (parens/mk-paren-token? document)]
    (loop [offset (dec (core/caret-offset state))]
      (let [[prev-from prev-to] (parens/find-prev-form text paren-token? offset)]
        (if (or (not (some? prev-from))
                (not (some? prev-to))
                (= 0 offset))
          state
          (let [prev-from-chars (text/offset->char-offset text prev-from)
                prev-to-chars (text/offset->char-offset text prev-to)]
            (if (and (parens/paren? (long (parens/get-char text prev-from)))
                     (parens/paren? (long (parens/get-char text prev-to)))
                     (paren-token? prev-from-chars)
                     (paren-token? prev-to-chars))
              (controller/set-caret-at-offset state prev-to selection?)
              (recur (dec prev-from)))))))))

(defn navigate-prev-form-up [{:keys [document] :as state} selection?]
  (let [{:keys [text]} document
        paren-token? (parens/mk-paren-token? document)
        caret-offset (core/caret-offset state)
        [cur-from _] (parens/enclosing-parens text paren-token? caret-offset)]
    (if (some? cur-from)
      (controller/set-caret-at-offset state cur-from selection?)
      state)))

(defn kill-form [{:keys [document editor last-operation] :as state} current-operation]
  (let [{:keys [text]} document
        prev-was-kill? (= last-operation current-operation)
        caret-offset (core/caret-offset state)
        paren-token? (parens/mk-paren-token? document)
        [next-from next-to] (parens/find-next-form text paren-token? caret-offset)]
    (if (some? (and next-from next-to))
      (let [kill-len (inc (- next-to caret-offset))
            killed-text (str (core/text-at-offset text caret-offset kill-len))]
        (-> state
            (core/delete-at-offset caret-offset kill-len)
            (cond-> prev-was-kill? (controller/conj-to-clipboard killed-text)
              (not prev-was-kill?) (controller/put-to-clipboard killed-text))))
      state)))

(defn yank [{:keys [document editor] :as state}]
  (let [{:keys [clipboard]} editor]
    (controller/type-in state (:content clipboard))))

(defn kill [{:keys [document editor last-operation] :as state} current-operation]
  (let [{:keys [text]} document
        prev-was-kill? (= last-operation current-operation)
        caret-offset (core/caret-offset state)
        caret-line (andel.utils/offset->line caret-offset text)
        paren-token? (parens/mk-paren-token? document)
        delete-to (loop [[from to] (parens/find-next-form text paren-token? caret-offset)]
                    (when (some? to)
                      (let [cur-line (andel.utils/offset->line to text)]
                        (if (< caret-line cur-line)
                          to
                          (if-let [[next-from next-to :as range] (parens/find-next-form text paren-token? (inc to))]
                            (let [next-line (andel.utils/offset->line next-from text)]
                              (if (< caret-line next-line)
                                to
                                (recur range)))
                            to)))))]
    (if (some? delete-to)
      (let [kill-len (inc (- delete-to caret-offset))
            killed-text (str (core/text-at-offset text caret-offset kill-len))]
        (-> state
            (core/delete-at-offset caret-offset kill-len)
            (cond-> prev-was-kill? (controller/conj-to-clipboard killed-text)
              (not prev-was-kill?) (controller/put-to-clipboard killed-text))))
      state)))