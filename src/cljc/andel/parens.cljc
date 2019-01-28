(ns andel.parens
  (:require [clojure.data.int-map :as i]
            [andel.text :as text]
            [andel.core :as core]
            [andel.cursor :as cursor]
            [andel.intervals :as intervals])
  #?(:clj (:import [andel.cursor Cursor TransientCursor])))

(defn get-char [text ^long offset]
  (assert (and (<= 0 offset) (< offset (text/text-length text))) [0 offset (text/text-length text)])
  (.charAt (core/text-at-offset text offset 1) 0))

(defn quoted? [text offset]
  (if (and (< 0 offset) (= \\ (get-char text (dec offset))))
    (not (quoted? text (dec offset)))
    false))

(defn closing? [^long codepoint]
  (or (= codepoint (int \)))
      (= codepoint (int \]))
      (= codepoint (int \}))))

(defn opening? [^long codepoint]
  (or (= codepoint (int \())
      (= codepoint (int \{))
      (= codepoint (int \[))))

(defn paren? [^long codepoint]
  (or (closing? codepoint)
      (opening? codepoint)))

(def opposite
  (into {}
        (map (fn [[k v]] [(int k) (int v)]))
        {\( \) \) \( \[ \] \] \[ \{ \} \} \{}))

(defn mk-paren-token? [{:keys [lexer is-brace?]}]
  (if (some? lexer)
    (fn [offset]
      (is-brace? lexer offset))
    (constantly true)))

(defn- find-matching-paren [text lexer-paren? offset should-push? should-pop? advance]
  (when (and (<= 0 offset)
             (< offset (text/text-length text)))
    (let [t-cursor (cursor/transient (cursor/make-cursor text offset))
          paren (cursor/get-char t-cursor)]
      (when (and (paren? paren)
                 (lexer-paren? (cursor/char-offset t-cursor)))
        (loop [s '()]
          (when-let [t-cursor (advance t-cursor)]
            (let [c (cursor/get-char t-cursor)
                  o (cursor/offset t-cursor)
                  co (cursor/char-offset t-cursor)]
              (cond
                (not (and (paren? paren) (lexer-paren? co))) (recur s)
                (should-push? c) (recur (cons c s))
                (should-pop? c) (cond (= c (opposite (first s))) (recur (rest s))
                                  (= c (opposite paren)) o
                                  :else (recur s))
                :else (recur s)))))))))

(defn find-matching-paren-forward [text lexer-paren? offset]
  (find-matching-paren text lexer-paren? offset opening? closing? #(cursor/next! %)))

(defn find-matching-paren-backward [text lexer-paren? offset]
  (find-matching-paren text lexer-paren? offset closing? opening? #(cursor/prev! %)))

(defn find-unbalanced-paren [text lexer-paren? offset should-push? should-pop? advance]
  (when (and (<= 0 offset)
             (< offset (text/text-length text)))
    (let [t-cursor (cursor/transient (cursor/make-cursor text offset))]
      (loop [s '()]
        (when-let [t-cursor (advance t-cursor)]
          (let [c (cursor/get-char t-cursor)
                o (cursor/offset t-cursor)]
            (cond
              (not (and (paren? c)
                        (lexer-paren? o))) (recur s)
              (should-push? c) (recur (cons c s))
              (should-pop? c) (if (= c (opposite (first s)))
                                (recur (rest s))
                                o)
              :else (recur s))))))))

(defn find-closing-paren [text lexer-paren? offset]
  (find-unbalanced-paren text lexer-paren? offset opening? closing? #(cursor/next! ^TransientCursor %)))

(defn find-opening-paren [text lexer-paren? offset]
  (find-unbalanced-paren text lexer-paren? offset closing? opening? #(cursor/prev! ^TransientCursor %)))

(defn find-parens-pair [text lexer-paren? offset]
  (let [len (text/text-length text)]
    (when (< 0 len)
      (let [prev-offset (max (dec offset) 0)
            offset (min offset (dec len))
            c0 (-> text (cursor/make-cursor prev-offset) (cursor/get-char))
            c1 (-> text (cursor/make-cursor offset) (cursor/get-char))]
        (cond
          (closing? c0) [(find-matching-paren-backward text lexer-paren? prev-offset) prev-offset]
          (opening? c1) [offset (find-matching-paren-forward text lexer-paren? offset)]
          :else         nil)))))

(defn highlight-parens [{:keys [editor document marker-id-generator] :as state} paren-attrs]
  (let [caret-offset  (core/caret-offset state)
        text (:text document)
        lexer-paren?  (mk-paren-token? document)
        [p-from p-to] (find-parens-pair text
                                        lexer-paren?
                                        caret-offset)
        old-paren-ids (:paren-ids editor)
        state (core/delete-markers state old-paren-ids)]
    (if (and p-from p-to)
      (let [from-id (marker-id-generator)
            to-id   (marker-id-generator)]
        (-> state
            (core/insert-markers [(intervals/>Marker :id from-id
                                                     :from p-from
                                                     :to (inc p-from)
                                                     :greedy-right? false
                                                     :greedy-left? false
                                                     :attrs paren-attrs)
                                  (intervals/>Marker :id to-id
                                                     :from p-to
                                                     :to (inc p-to)
                                                     :greedy-right? false
                                                     :greedy-left? false
                                                     :attrs paren-attrs)])
            (assoc-in [:editor :paren-ids] (i/int-set [from-id to-id]))))
      state)))

(defn enclosing-parens [text lexer-paren? offset]
  (let [opening (find-opening-paren text lexer-paren? offset)
        closing (find-closing-paren text lexer-paren? (dec offset))]
    (when (and opening closing)
      [opening closing])))

(def whitespace? #{(int \newline) (int \space) (int \tab)})

(defn not-whitespace-or-paren? [c]
  (and (not (whitespace? c))
       (not (paren? c))))


(defn find-next-form [text lexer-paren? offset]
  (when (< offset (text/text-length text))
    (let [cursor (cursor/make-cursor text offset)
          form-start-cursor (first (cursor/forward-while cursor whitespace?))
          form-start-offset (+ offset (cursor/distance cursor form-start-cursor))
          form-start-char   (cursor/get-char form-start-cursor)
          next-char         (some-> form-start-cursor cursor/next cursor/get-char)]
      (cond
        (paren? form-start-char)
        [form-start-offset (find-matching-paren-forward text lexer-paren? form-start-offset)]

        (= (int \#) form-start-char)
        (cond
          (paren? next-char)
          [form-start-offset (find-matching-paren-forward text lexer-paren? (inc form-start-offset))]

          (= (int \") next-char)
          [form-start-offset (+ (inc form-start-offset) 1 (cursor/count-matching (cursor/next (cursor/next form-start-cursor)) #(not= (int \") %) :forward))])

        (= (int \") form-start-char)
        [form-start-offset (+ form-start-offset 1 (cursor/count-matching (cursor/next form-start-cursor) #(not= (int \") %) :forward))]

        (= (int \;) form-start-char)
        [form-start-offset (+ form-start-offset 1 (cursor/count-matching (cursor/next form-start-cursor) #(not= (int \newline) %) :forward))]

        :else
        (let [[form-end-cursor end?] (cursor/forward-while form-start-cursor not-whitespace-or-paren?)
              form-end-offset (if end?
                                (cursor/offset form-end-cursor)
                                (dec (cursor/offset form-end-cursor)))]
          [form-start-offset form-end-offset])))))

(defn find-prev-form [text lexer-paren? offset]
  (when (< 0 offset)
    (let [cursor (cursor/make-cursor text offset)
          [form-end-cursor exhausted?] (cursor/backward-while cursor whitespace?)]
      (when-not exhausted?
        (let [form-end-offset (- offset (cursor/distance cursor form-end-cursor))
              form-end-char   (cursor/get-char form-end-cursor)]
          (cond
            (paren? form-end-char)
            (when-let [form-start-offset (find-matching-paren-backward text lexer-paren? form-end-offset)]
              (let [form-start-offset (if (and (< 0 form-start-offset) (= \# (get-char text (dec form-start-offset))))
                                        (dec form-start-offset)
                                        form-start-offset)]
                [form-start-offset form-end-offset]))

            (= (int \") form-end-char)
            (let [form-start-offset (- form-end-offset 1 (cursor/count-matching (cursor/prev form-end-cursor) #(not= (int \") %) :backward))
                  form-start-offset (if (and (< 0 form-start-offset) (= \# (get-char text (dec form-start-offset))))
                                      (dec form-start-offset)
                                      form-start-offset)]
              [form-start-offset form-end-offset])


            :else
            (let [[form-start-cursor end?] (cursor/backward-while form-end-cursor not-whitespace-or-paren?)
                  form-start-offset (if end?
                                      (cursor/offset form-start-cursor)
                                      (inc (cursor/offset form-start-cursor)))]
              [form-start-offset form-end-offset])))))))
