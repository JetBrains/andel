(ns andel.parens
  (:require [andel.text :as text]
            [andel.core :as core]
            [andel.intervals :as intervals]))

(def closing? #{\) \} \]})

(def opening? #{\( \{ \[})

(def opposite {\( \) \) \( \[ \] \] \[ \{ \} \} \{})

(defn paren-symbol? [c] (or (closing? c) (opening? c)))

(defn find-matching-paren-forward [text paren-token? offset]
  (let [text-length (text/text-length text)
        char-seq    (text/text->char-seq text)
        paren       (.charAt char-seq offset)]
    (when (and (paren-symbol? paren)
               (paren-token? offset))
      (loop [offset (inc offset)
             s '()]
        (when (< offset text-length)
          (let [c (.charAt char-seq offset)]
            (cond
              (not (paren-token? offset)) (recur (inc offset) s)
              (opening? c) (recur (inc offset) (cons c s))
              (closing? c) (cond (= c (opposite (first s))) (recur (inc offset) (rest s))
                                 (= c (opposite paren))     offset
                                 :else (recur (inc offset) s))
              :else (recur (inc offset) s))))))))

(defn find-matching-paren-backward [text paren-token? offset]
  (let [text-length (text/text-length text)
        char-seq    (text/text->char-seq text)
        paren       (.charAt char-seq offset)]
    (when (and (paren-symbol? paren)
               (paren-token? offset))
      (loop [offset (dec offset)
             s '()]
        (when (< 0 offset)
          (let [c (.charAt char-seq offset)]
            (cond
              (not (paren-token? offset)) (recur (dec offset) s)
              (closing? c) (recur (dec offset) (cons c s))
              (opening? c) (cond (= c (opposite (first s))) (recur (dec offset) (rest s))
                                 (= c (opposite paren))     offset
                                 :else (recur (dec offset) s))
              :else (recur (dec offset) s))))))))

(defn find-closing-paren [text paren-token? offset]
  (let [text-length (text/text-length text)
        char-seq    (text/text->char-seq text)]
    (loop [offset offset
           s '()]
      (when (< offset text-length)
        (let [c (.charAt char-seq offset)]
          (cond
            (not (paren-token? offset)) (recur (inc offset) s)
            (opening? c) (recur (inc offset) (cons c s))
            (closing? c) (cond (= c (opposite (first s))) (recur (inc offset) (rest s))
                               :else offset)
            :else (recur (inc offset) s)))))))

(defn find-opening-paren [text paren-token? offset]
  (let [text-length (text/text-length text)
        char-seq (text/text->char-seq text)]
    (loop [offset offset
           s '()]
      (when (< 0 offset)
        (let [c (.charAt char-seq offset)]
          (cond
            (not (paren-token? offset)) (recur (dec offset) s)
            (closing? c) (recur (dec offset) (cons c s))
            (opening? c) (cond (= c (opposite (first s))) (recur (dec offset) (rest s))
                               :else offset)
            :else (recur (dec offset) s)))))))

(defn find-parens [text paren-token? offset]
  (let [len (text/text-length text)]
    (when (< 0 len)
      (let [prev-offset (max (dec offset) 0)
            offset (min offset (dec len))
            c0 (-> text text/text->char-seq (.charAt prev-offset))
            c1 (-> text text/text->char-seq (.charAt offset))]
        (cond
          (closing? c0) [(find-matching-paren-backward text paren-token? prev-offset) prev-offset]
          (opening? c1) [offset (find-matching-paren-forward text paren-token? offset)]
          :else         nil)))))

(defn enclosing-parens [text paren-token? offset]
  [(find-opening-paren text paren-token? offset) (find-closing-paren text paren-token? offset)])

(def whitespace? #{\newline \space \tab})

(defn count-matching [text offset pred]
  (let [length (text/text-length text)
        char-seq (text/text->char-seq text)
        sub-seq (.subSequence char-seq offset length)]
    (count (take-while pred sub-seq))))

;; offset -> [offset, offset]
(defn find-next-form [text paren-token? offset]
  (let [char-seq (text/text->char-seq text)
        form-start-offset (+ offset (count-matching text offset whitespace?))
        form-start-char   (.charAt char-seq form-start-offset)]
    (cond (paren-symbol? form-start-char) (find-parens text paren-token? form-start-offset)
;          (= \" form-start-char) nil ;; fix string case
          (= \; form-start-char) nil ;; fix comment case
          :else [form-start-offset (+ form-start-offset (dec (count-matching text form-start-offset (complement whitespace?))))])))

(defn slurp-forward [{:keys [editor document] :as state}]
  (let [{:keys [text lexer]} document
        caret-offset (core/caret-offset state)
        paren-token? #(intervals/is-brace-token? lexer %)
        [_ cur-to] (enclosing-parens text paren-token? caret-offset) ;; check for not surrounded case
        [_ next-to] (find-next-form text paren-token? (inc cur-to))
        paren-str (core/text-at-offset state cur-to 1)]
    (-> state
        (core/delete-at-offset cur-to 1)
        (core/insert-at-offset next-to paren-str))))

(comment

  (defn count-matching-reverse [text offset pred]
    (let [_ (prn "off" offset)
          length (text/text-length text)
          char-seq (text/text->reverse-char-seq text)
          sub-seq (.subSequence char-seq (- length offset) length)]
      (prn "sub-seq" (reverse (str sub-seq)))
      (prn "count" (count (take-while pred sub-seq)))
      (inc (count (take-while pred sub-seq)))))

  (defn find-prev-form [text paren-token? offset]
    (let [char-seq (text/text->char-seq text)
          delta (count-matching-reverse text offset whitespace?)
          _ (prn delta)
          form-start-offset (- offset delta)
          form-start-char   (.charAt char-seq form-start-offset)]
      (cond (paren-symbol? form-start-char) (find-parens text paren-token? form-start-offset)
                                        ;          (= \" form-start-char) nil ;; fix string case
            (= \; form-start-char) nil ;; fix comment case
            :else [form-start-offset (+ form-start-offset (dec (count-matching-reverse text form-start-offset (complement whitespace?))))])))

  (defn slurp-backward [{:keys [editor document] :as state}]
    (let [{:keys [text lexer]} document
          caret-offset (core/caret-offset state)
          paren-token? #(intervals/is-brace-token? lexer %)
          [cur-from cur-to] (enclosing-parens text paren-token? caret-offset) ;; check for not surrounded case
          [prev-from prev-to] (find-prev-form text paren-token? cur-from)
          paren-str (core/text-at-offset state (dec cur-from) 1)]
      (prn "cur" cur-from cur-to)
      (prn "prev" prev-from prev-to)
      (-> state
          (core/delete-at-offset (dec cur-from) 1)
          (core/insert-at-offset prev-from paren-str)))))



(comment

  (-> "..(.}.).."
      text/make-text
      (find-matching-paren-forward (constantly false) 2))

  (-> "..(.}.).."
      text/make-text
      (find-matching-paren-backward (constantly false) 6))

  (-> "..(....).."
      text/make-text
      (find-closing-paren-forward (constantly false) 3))
a
  (-> "fun x(a, b) = { ... };"
      text/make-text
      (find-closing-paren-backward (constantly false) 16))

  (-> "fun x(a, b) = { ... };"
      text/make-text
      (find-parens (constantly false) 14))



  )
