(ns andel.search
  (:require [andel.text :as text]))

#_(defn search-forward
  ([document s] (search-forward document 0 s))
  ([{:keys [text] :as document} offset s]
   (let [regex       (re-pattern (str "^[" s "]*" s))
         text-length (text/text-length text)
         char-seq    (text/text->char-seq text)
         sub-seq     (.subSequence char-seq offset text-length)]
     (some-> (re-find regex sub-seq)
;             count
;             (- (.length s))
;             (+ offset)
             ))))

(def closing? #{\) \} \]})

(def opening? #{\( \{ \[})

(def opposite {\( \) \) \( \[ \] \] \[ \{ \} \} \{})

(defn paren? [c] (or (closing? c) (opening? c)))

(defn find-matching-paren-forward [text is-comment? offset]
  (let [text-length (text/text-length text)
        char-seq    (text/text->char-seq text)
        paren       (.charAt char-seq offset)]
    (when (paren? paren)
      (loop [offset (inc offset)
             s '()]
        (when (< offset text-length)
          (let [c (.charAt char-seq offset)]
            (cond
              (is-comment? offset) (recur (inc offset) s)
              (opening? c) (recur (inc offset) (cons c s))
              (closing? c) (cond (= c (opposite (first s))) (recur (inc offset) (rest s))
                                 (= c (opposite paren))     offset
                                 :else (recur (inc offset) s))
              :else (recur (inc offset) s))))))))

(defn find-matching-paren-backward [text is-comment? offset]
  (let [text-length (text/text-length text)
        offset' (- (dec text-length) offset)
        char-seq    (text/text->reverse-char-seq text)
        paren       (.charAt char-seq offset')]
    (when (paren? paren)
      (loop [offset' (inc offset')
             s '()]
        (when (< offset' text-length)
          (let [c (.charAt char-seq offset')]
            (cond
              (is-comment? (- (dec text-length) offset')) (recur (inc offset') s)
              (closing? c) (recur (inc offset') (cons c s))
              (opening? c) (cond (= c (opposite (first s))) (recur (inc offset') (rest s))
                                 (= c (opposite paren))     (- (dec text-length) offset')
                                 :else (recur (inc offset') s))
              :else (recur (inc offset') s))))))))

(defn find-closing-paren [text is-comment? offset]
  (let [text-length (text/text-length text)
        char-seq    (text/text->char-seq text)]
    (loop [offset offset
           s '()]
      (when (< offset text-length)
        (let [c (.charAt char-seq offset)]
          (cond
            (is-comment? offset) (recur (inc offset) s)
            (opening? c) (recur (inc offset) (cons c s))
            (closing? c) (cond (= c (opposite (first s))) (recur (inc offset) (rest s))
                               :else offset)
            :else (recur (inc offset) s)))))))

(defn find-opening-paren [text is-comment? offset]
  (let [text-length (text/text-length text)
        offset' (- (dec text-length) offset)
        char-seq    (text/text->reverse-char-seq text)]
    (loop [offset' offset'
           s '()]
      (when (< offset' text-length)
        (let [c (.charAt char-seq offset')]
          (cond
            (is-comment? (- (dec text-length) offset')) (recur (inc offset') s)
            (closing? c) (recur (inc offset') (cons c s))
            (opening? c) (cond (= c (opposite (first s))) (recur (inc offset') (rest s))
                               :else (- (dec text-length) offset'))
            :else (recur (inc offset') s)))))))


(defn find-parens [text is-comment? offset]
  (let [len (text/text-length text)]
    (when (< 0 len)
      (let [prev-offset (max (dec offset) 0)
            offset (min offset (dec len))
            c0 (-> text text/text->char-seq (.charAt prev-offset))
            c1 (-> text text/text->char-seq (.charAt offset))]
        (cond
          (closing? c0)  [(find-matching-paren-backward text is-comment? prev-offset) prev-offset]
          (opening? c1) [offset (find-matching-paren-forward text is-comment? offset)]
          :else nil)))))


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

  (-> "fun x(a, b) = { ... };"
      text/make-text
      (find-closing-paren-backward (constantly false) 16))

  (-> "fun x(a, b) = { ... };"
      text/make-text
      (find-parens (constantly false) 14))



  )
