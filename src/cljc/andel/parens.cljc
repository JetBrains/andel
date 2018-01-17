(ns andel.parens
  (:require [andel.text :as text]
            [andel.core :as core]
            [andel.cursor :as cursor]
            [andel.controller :as controller]
            [andel.intervals :as intervals])
  #?(:clj (:import [andel.cursor Cursor TransientCursor])))

(def closing? #{\) \} \]})

(def opening? #{\( \{ \[})

(def paren? (clojure.set/union closing? opening?))

(def opposite {\( \) \) \( \[ \] \] \[ \{ \} \} \{})

(defn paren-symbol? [c] (or (closing? c) (opening? c)))

(defn- find-matching-paren [text paren-token? offset should-push? should-pop? advance]
  (let [t-cursor ^TransientCursor (cursor/transient (cursor/make-cursor text offset))
        paren  (.getChar t-cursor)]
    (when (and (paren-symbol? paren)
               (paren-token? offset))
      (loop [s '()]
        (advance t-cursor)
        (let [c (.getChar t-cursor)
              o (.getOffset t-cursor)]
          (cond
            (.isExhausted t-cursor) nil
            (not (paren-token? o)) (recur s)
            (should-push? c) (recur (cons c s))
            (should-pop? c) (cond (= c (opposite (first s))) (recur (rest s))
                                  (= c (opposite paren))     o
                                  :else (recur s))
            :else (recur s)))))))

(defn find-matching-paren-forward [text paren-token? offset]
  (when-let [to (find-matching-paren text paren-token? offset opening? closing? #(.next! ^TransientCursor %))]
    [offset to]))

(defn find-matching-paren-backward [text paren-token? offset]
  (when-let [from (find-matching-paren text paren-token? offset closing? opening? #(.prev! ^TransientCursor %))]
    [from offset]))

(defn find-unbalanced-paren [text paren-token? offset should-push? should-pop? advance]
  (let [t-cursor ^TransientCursor (cursor/transient (cursor/make-cursor text offset))]
    (loop [s '()]
      (advance t-cursor)
      (when (not (.isExhausted t-cursor))
        (let [c (.getChar t-cursor)
              o (.getOffset t-cursor)]
          (cond
            (not (paren-token? o)) (recur s)
            (should-push? c) (recur (cons c s))
            (should-pop? c) (cond (= c (opposite (first s))) (recur (rest s))
                                  :else o)
            :else (recur s)))))))

(defn find-closing-paren [text paren-token? offset]
  (find-unbalanced-paren text paren-token? offset opening? closing? #(.next! ^TransientCursor %)))

(defn find-opening-paren [text paren-token? offset]
  (find-unbalanced-paren text paren-token? offset closing? opening? #(.prev! ^TransientCursor %)))

(defn find-parens-pair [text paren-token? offset]
  (let [len (text/text-length text)]
    (when (< 0 len)
      (let [prev-offset (max (dec offset) 0)
            offset (min offset (dec len))
            c0 (-> text (cursor/make-cursor prev-offset) (cursor/get-char))
            c1 (-> text (cursor/make-cursor offset) (cursor/get-char))]
        (cond
          (closing? c0) (find-matching-paren-backward text paren-token? prev-offset)
          (opening? c1) (find-matching-paren-forward text paren-token? offset)
          :else         nil)))))

(let [a (atom 0)]
  (def unique-paren-id #(swap! a inc)))

(defn highlight-parens [{:keys [document] :as state}]
  (let [caret-offset  (core/caret-offset state)
        lexer (:lexer document)
        paren-offsets (find-parens-pair (:text document)
                                          (if (some? lexer)
                                            #(intervals/is-brace-token? lexer %)
                                            (constantly true))
                                          caret-offset)
        old-paren-ids (:paren-ids document)]
    (-> state
        (core/delete-markers old-paren-ids)
        ((fn [state]
           (or (when-let [[p-from p-to] paren-offsets]
                 (when (and p-from p-to)
                   (let [from-id (str "paren-" (unique-paren-id))
                         to-id   (str "paren-" (unique-paren-id))]
                     (-> state
                         (core/insert-markers [(intervals/->Marker p-from
                                                                   (inc p-from)
                                                                   false
                                                                   false
                                                                   (intervals/->Attrs from-id "highlight-paren" "" :background))
                                               (intervals/->Marker p-to
                                                                   (inc p-to)
                                                                   false
                                                                   false
                                                                   (intervals/->Attrs to-id "highlight-paren" "" :background))])
                         (assoc-in [:document :paren-ids] [from-id to-id])))))
               state))))))


(defn enclosing-parens [text paren-token? offset]
  [(find-opening-paren text paren-token? offset) (find-closing-paren text paren-token? (dec offset))])

(def whitespace? #{\newline \space \tab})

(defn symbol? [c]
  (and (not (whitespace? c))
       (not (paren? c))))

(defn find-next-form [text paren-token? offset]
  (let [cursor (cursor/make-cursor text offset)
        form-start-cursor (cursor/move-while cursor whitespace? :forward)
        form-start-offset (+ offset (cursor/distance cursor form-start-cursor))
        form-start-char   (cursor/get-char form-start-cursor)]
    (cond (paren-symbol? form-start-char) (find-matching-paren-forward text paren-token? form-start-offset)
;         (= \" form-start-char) nil ;; fix string case
          (= \; form-start-char) nil ;; fix comment case
          :else [form-start-offset (+ form-start-offset (dec (cursor/count-matching form-start-cursor symbol? :forward)))])))

(defn find-prev-form [text paren-token? offset]
  (let [cursor          (cursor/make-cursor text offset)
        form-end-cursor (cursor/move-while cursor whitespace? :backward)
        form-end-offset (- offset (cursor/distance cursor form-end-cursor))
        form-end-char   (cursor/get-char form-end-cursor)]
    (cond (paren-symbol? form-end-char) (find-matching-paren-backward text paren-token? form-end-offset)
;         (= \" form-start-char) nil ;; fix string case
          (= \; form-end-char) nil ;; fix comment case
          :else [(- form-end-offset (dec (cursor/count-matching form-end-cursor symbol? :backward))) form-end-offset])))

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

(defn splice-left [{:keys [editor document] :as state}]
  (let [{:keys [text lexer]} document
        caret-offset (core/caret-offset state)
        paren-token? #(intervals/is-brace-token? lexer %)
        [cur-from cur-to] (enclosing-parens text paren-token? caret-offset)]
    (-> state
        (core/delete-at-offset cur-to 1)
        (core/delete-at-offset cur-from (- caret-offset cur-from)))))

(defn paredit-delete [{:keys [document] :as state}]
  (let [{:keys [text lexer]} document
        [sel-from sel-to] (core/selection state)
        paren-token? #(intervals/is-brace-token? lexer %)
        caret-offset (core/caret-offset state)
        cursor (cursor/make-cursor text caret-offset)
        character (cursor/get-char cursor)]
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

(defn paredit-backspace [{:keys [document] :as state}]
  (let [{:keys [text lexer]} document
        [sel-from sel-to] (core/selection state)
        paren-token? #(intervals/is-brace-token? lexer %)
        caret-offset (core/caret-offset state)
        deletion-offset (dec caret-offset)
        cursor (cursor/make-cursor text deletion-offset)
        character (cursor/get-char cursor)]
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

(defn paredit-open-round [state]
  (-> state
      (controller/type-in "()")
      (controller/move-caret :left false)))

(defn paredit-open-square [state]
  (-> state
      (controller/type-in "[]")
      (controller/move-caret :left false)))

(defn paredit-open-curly [state]
  (-> state
      (controller/type-in "{}")
      (controller/move-caret :left false)))
