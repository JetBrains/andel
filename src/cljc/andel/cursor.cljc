(ns andel.cursor
  (:refer-clojure :exclude [next transient persistent!])
  (:require [andel.tree :as tree]
            [andel.text :as text]
            [andel.array-list :as al])
  (:import [andel.tree ZipperLocation Leaf]))

(defn- leaf-text ^String [^ZipperLocation loc]
  (let [^Leaf leaf (tree/node loc)]
    (.-data leaf)))

(definterface Cursor
  (^int getChar [])
  ( ^long getOffset [])
  ( ^long getCharOffset []))

(defn get-char ^long [cursor]
  (.getChar ^Cursor cursor))

(defn offset ^long [cursor]
  (.getOffset ^Cursor cursor))

(defn char-offset ^long [cursor]
  (.getCharOffset ^Cursor cursor))

(deftype ACursor [zipper ^long node-char-offset ^int inner-char-offset ^long offset ^long text-length ^int leaf-char-length]
  Cursor
  (getChar [this]
    (.codePointAt (leaf-text zipper) inner-char-offset))
  (getOffset [this]
    offset)
  (getCharOffset [this]
    (+ node-char-offset inner-char-offset)))

(defmacro ->cursor [& {:keys [zipper node-char-offset inner-char-offset offset text-length leaf-char-length]}]
  `(ACursor. ~zipper ~node-char-offset ~inner-char-offset ~offset ~text-length ~leaf-char-length))

(defn make-cursor [text ^long offset]
  (let [zipper           (-> (text/zipper text)
                             (text/scan-to-offset offset))
        node-char-offset (text/node-char-offset zipper)
        char-offset      (text/char-offset zipper)
        text-length      (text/text-length text)]
    (assert
      (and (<= 0 offset)
           (< offset text-length))
      (str "OUT OF BOUNDS: " 0 " <= " offset " < " text-length))
    (->cursor
     :zipper            zipper
     :node-char-offset  node-char-offset
     :inner-char-offset (- char-offset node-char-offset)
     :offset            offset
     :text-length       text-length
     :leaf-char-length  (.length (leaf-text zipper)))))

(defn next [^ACursor cursor]
  (let [zipper                 (.-zipper cursor)
        text-length            (.-text-length cursor)
        offset                 (.-offset cursor)
        inner-char-offset      (.-inner-char-offset cursor)
        node-char-offset       (.-node-char-offset cursor)
        leaf-char-length       (.-leaf-char-length cursor)
        next-inner-char-offset (if (Character/isSupplementaryCodePoint (get-char cursor))
                                 (+ 2 inner-char-offset)
                                 (inc inner-char-offset))]
    (cond
      (< next-inner-char-offset leaf-char-length)
      (->cursor
       :zipper            zipper
       :node-char-offset  node-char-offset
       :inner-char-offset next-inner-char-offset
       :offset            (inc offset)
       :text-length       text-length
       :leaf-char-length  leaf-char-length)

      (< (inc offset) text-length)
      (let [next-leaf (tree/next-leaf zipper)]
        (->cursor
         :zipper            next-leaf
         :node-char-offset  (+ node-char-offset leaf-char-length)
         :inner-char-offset 0
         :offset            (inc offset)
         :text-length       text-length
         :leaf-char-length  (.length (leaf-text next-leaf))))

      :else nil)))

(defn prev [^ACursor cursor]
  (let [zipper            (.-zipper cursor)
        inner-char-offset (.-inner-char-offset cursor)
        node-char-offset  (.-node-char-offset cursor)
        offset            (.-offset cursor)
        text-length       (.-text-length cursor)
        leaf-char-length  (.-leaf-char-length cursor)]
    (cond
      (< 0 inner-char-offset)
      (->cursor
       :zipper           zipper
       :node-char-offset node-char-offset
       :inner-char-offset (if (Character/isHighSurrogate
                               (.charAt (leaf-text zipper) (dec inner-char-offset)))
                            (- inner-char-offset 2)
                            (dec inner-char-offset))
       :offset           (dec offset)
       :text-length      text-length
       :leaf-char-length leaf-char-length)

      (< 0 offset)
      (let [prev-leaf             (tree/prev-leaf zipper)
            prev-leaf-text        (leaf-text prev-leaf)
            prev-leaf-char-length (.length prev-leaf-text)]
        (->cursor
         :zipper           prev-leaf
         :offset           (dec offset)
         :leaf-char-length prev-leaf-char-length
         :node-char-offset (- node-char-offset prev-leaf-char-length)
         :inner-char-offset
                           (if (Character/isHighSurrogate (.charAt prev-leaf-text (dec prev-leaf-char-length)))
                             (- prev-leaf-char-length 2)
                             (dec prev-leaf-char-length))
         :text-length      text-length))

      :else nil)))

(definterface MutableCursor
  (^Object next [])
  (^Object prev [])

  (^Object getZipper [])
  (^long getNodeCharOffset [])
  (^int getInnerCharOffset [])
  (^int getLeafCharLength []))

(defn next! [cursor]
  (.next ^MutableCursor cursor))

(defn prev! [cursor]
  (.prev ^MutableCursor cursor))

(deftype TransientCursor [^{:volatile-mutable true} zipper
                          ^{:volatile-mutable true :tag long} node-char-offset
                          ^long text-length
                          ^{:volatile-mutable true :tag int} inner-char-offset
                          ^{:volatile-mutable true :tag int} leaf-char-length
                          ^{:volatile-mutable true :tag long} offset]
  Cursor
  (getChar [this]
    (.codePointAt (leaf-text zipper) inner-char-offset))
  (getOffset [this] offset)
  (getCharOffset [this]
    (+ node-char-offset inner-char-offset))

  MutableCursor
  (next [this]
    (let [next-inner-char-offset (if (Character/isSupplementaryCodePoint (get-char this))
                                   (+ 2 inner-char-offset)
                                   (inc inner-char-offset))]
      (cond
        (< next-inner-char-offset leaf-char-length)
        (do
          (set! offset (inc offset))
          (set! inner-char-offset (int next-inner-char-offset))
          this)

        (< (inc offset) text-length)
        (let [next-leaf (tree/next-leaf zipper)]
          (set! zipper next-leaf)
          (set! node-char-offset (+ node-char-offset leaf-char-length))
          (set! inner-char-offset (int 0))
          (set! offset (inc offset))
          (set! leaf-char-length (.length (leaf-text next-leaf)))
          this)

        :else nil)))
  (prev  [this]
    (cond
      (< 0 inner-char-offset)
      (do
        (set! inner-char-offset (int (if (Character/isHighSurrogate
                                          (.charAt (leaf-text zipper) (dec inner-char-offset)))
                                       (- inner-char-offset 2)
                                       (dec inner-char-offset))))
        (set! offset (dec offset))
        this)

      (< 0 offset)
      (let [prev-leaf    (tree/prev-leaf zipper)
            prev-leaf-text (leaf-text prev-leaf)
            prev-leaf-char-length (.length prev-leaf-text)]
        (set! zipper prev-leaf)
        (set! offset (dec offset))
        (set! node-char-offset (- node-char-offset prev-leaf-char-length))
        (set! leaf-char-length prev-leaf-char-length)
        (set! inner-char-offset (int (if (Character/isHighSurrogate (.charAt prev-leaf-text (dec prev-leaf-char-length)))
                                       (- prev-leaf-char-length 2)
                                       (dec prev-leaf-char-length))))
        this)

      :else nil))
  (getZipper [_] zipper)
  (getNodeCharOffset [_] node-char-offset)
  (getInnerCharOffset [_] inner-char-offset)
  (getLeafCharLength [_] leaf-char-length))

(defmacro ->transient-cursor [& {:keys [zipper node-char-offset text-length inner-char-offset
                                        offset leaf-char-length]}]
  `(TransientCursor. ~zipper ~node-char-offset ~text-length
    ~inner-char-offset ~leaf-char-length ~offset))

(defn transient ^TransientCursor [^ACursor cursor]
  (->transient-cursor
   :zipper            (.-zipper cursor)
   :node-char-offset  (.-node-char-offset cursor)
   :inner-char-offset (.-inner-char-offset cursor)
   :offset            (.-offset cursor)
   :leaf-char-length  (.-leaf-char-length cursor)
   :text-length       (.-text-length cursor)))

(defn persistent! [^TransientCursor cursor]
  (->cursor
   :zipper            (.getZipper cursor)
   :node-char-offset  (.getNodeCharOffset cursor)
   :inner-char-offset (.getInnerCharOffset cursor)
   :offset            (.getOffset cursor)
   :leaf-char-length  (.getLeafCharLength cursor)
   :text-length       (.-text-length cursor)))


;;;;;;;;;;;;;;;;;;;;;; util ;;;;;;;;;;;;;;;;;;;;;

(defn set-to-offset! [^TransientCursor t-cursor ^long o]
  (cond
    (= o (offset t-cursor)) t-cursor
    (< o (offset t-cursor)) (while (< o (offset t-cursor))
                              (prev! t-cursor))
    (> o (offset t-cursor)) (while (> o (offset t-cursor))
                              (next! t-cursor)))
  t-cursor)

(defn forward-while [cursor pred]
  (loop [tc (transient cursor)]
    (if (pred (get-char tc))
      (if-let [tc' (next! tc)]
        (recur tc')
        [(persistent! tc) true])
      [(persistent! tc) false])))

(defn backward-while [cursor pred]
  (loop [tc (transient cursor)]
    (if (pred (get-char tc))
      (if-let [tc' (prev! tc)]
        (recur tc')
        [(persistent! tc) true])
      [(persistent! tc) false])))

(defn distance [from to]
  (Math/abs ^long (- (offset to) (offset from))))

(defn count-matching [cursor pred direction]
  (distance cursor (first (case direction
                            :backward (backward-while cursor pred)
                            :forward (forward-while cursor pred)))))
