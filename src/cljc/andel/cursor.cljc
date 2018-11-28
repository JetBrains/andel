(ns andel.cursor
  (:refer-clojure :exclude [next transient persistent!])
  (:require [andel.tree :as tree]
            [andel.text :as text]
            [andel.array-list :as al])
  #?(:clj (:import [andel.tree ZipperLocation Leaf])))

(defn- get-leaf-length ^long [^ZipperLocation loc]
  (assert (tree/leaf? (tree/node loc))
          "calling leaf-length on non-leaf")
  (let [^Leaf leaf (tree/node loc)
        ^String s  (.-data leaf)]
    (.length s)))

(defn- get-char-from-loc [^ZipperLocation loc offset]
  (assert (tree/leaf? (tree/node loc))
          "calling get-char-from-loc on non-leaf")
  (let [^Leaf leaf (tree/node loc)]
    (.charAt ^String (.-data leaf) offset)))


(defrecord Cursor [zipper ^long node-offset ^long text-length ^long inner-offset ^long leaf-length])

#?(:clj
   (defmacro ->cursor [{:keys [zipper node-offset
                               text-length inner-offset leaf-length]}]
     `(->Cursor ~zipper ~node-offset
                ~text-length ~inner-offset ~leaf-length)))

(defn make-cursor [text offset]
  (let [zipper      (-> text text/zipper (text/scan-to-offset offset))
        node-offset (text/node-offset zipper)
        offset      (text/offset zipper)
        text-length (text/text-length text)]
    (assert (and (<= 0 offset)
                 (< offset text-length))
            "OUT OF BOUNDS")
    (->cursor
       {:zipper zipper
        :node-offset  node-offset
        :text-length  text-length
        :inner-offset (- offset node-offset)
        :leaf-length  (get-leaf-length zipper)})))

(defn get-char [^Cursor cursor]
  (when cursor
    (let [zipper       (.-zipper cursor)
          inner-offset (.-inner-offset cursor)]
      (get-char-from-loc zipper inner-offset))))

(defn next [^Cursor cursor]
  (let [zipper       (.-zipper cursor)
        node-offset  (.-node-offset cursor)
        text-length  (.-text-length cursor)
        inner-offset (.-inner-offset cursor)
        leaf-length  (.-leaf-length cursor)]
    (cond
      (< (inc inner-offset) leaf-length)
      (->cursor
       {:zipper       zipper
        :node-offset  node-offset
        :text-length  text-length
        :inner-offset (inc inner-offset)
        :leaf-length  leaf-length})

      (< (inc (+ node-offset inner-offset)) text-length)
      (let [next-leaf (tree/next-leaf zipper)]
        (->cursor
         {:zipper       next-leaf
          :node-offset  (+ node-offset leaf-length)
          :text-length  text-length
          :inner-offset 0
          :leaf-length  (get-leaf-length next-leaf)}))

      :else nil)))

(defn prev [^Cursor cursor]
  (let [zipper       (.-zipper cursor)
        node-offset  (.-node-offset cursor)
        text-length  (.-text-length cursor)
        inner-offset (.-inner-offset cursor)
        leaf-length  (.-leaf-length cursor)]
    (cond
      (< 0 inner-offset)
      (->cursor
       {:zipper       zipper
        :node-offset  node-offset
        :text-length  text-length
        :inner-offset (dec inner-offset)
        :leaf-length  leaf-length})

      (< 0 (+ node-offset inner-offset))
      (let [prev-leaf   (tree/prev-leaf zipper)
            leaf-length (get-leaf-length prev-leaf)]
        (->cursor
         {:zipper       prev-leaf
          :node-offset  (- node-offset leaf-length)
          :text-length  text-length
          :inner-offset (dec leaf-length)
          :leaf-length  leaf-length}))

      :else nil)))

(defn offset ^long [^Cursor cursor]
  (let [node-offset  (.-node-offset cursor)
        inner-offset (.-inner-offset cursor)]
    (+ node-offset inner-offset)))

(defprotocol MutableCursor
  (next! ^MutableCursor [this])
  (prev! ^MutableCursor [this])
  (isExhausted [this])
  (getChar [this])
  (getZipper [this])
  (getOffset ^long [this])
  (getNodeOffset ^long [this])
  (getInnerOffset ^long [this])
  (getLeafLength ^long [this]))

(deftype TransientCursor [^{:volatile-mutable true} zipper
                          ^{:volatile-mutable true :tag long} node-offset
                          ^long text-length
                          ^{:volatile-mutable true :tag long} inner-offset
                          ^{:volatile-mutable true :tag long} leaf-length
                          ^{:volatile-mutable true} exhausted?]
  MutableCursor
  (next! ^TransientCursor [this]
    (cond
      (< (inc inner-offset) leaf-length)
      (do (when exhausted?
            (set! exhausted? false))
          (set! inner-offset (inc inner-offset))
          this)

      (< (inc (+ node-offset inner-offset)) text-length)
      (let [next-leaf (tree/next-leaf zipper)]
        (when exhausted?
          (set! exhausted? false))
        (set! zipper       next-leaf)
        (set! node-offset  (+ node-offset leaf-length))
        (set! inner-offset 0)
        (set! leaf-length  (get-leaf-length next-leaf))
        this)

      :else (do (set! exhausted? true)
                this)))
  (prev! ^TransientCursor [this]
    (cond
      (< 0 inner-offset)
      (do (when exhausted?
            (set! exhausted? false))
          (set! inner-offset (dec inner-offset))
          this)

      (< 0 (+ node-offset inner-offset))
      (let [prev-leaf    (tree/prev-leaf zipper)
            leaf-length' (get-leaf-length prev-leaf)]
        (when exhausted?
          (set! exhausted? false))
        (set! zipper       prev-leaf)
        (set! node-offset  (- node-offset leaf-length'))
        (set! inner-offset (dec leaf-length'))
        (set! leaf-length  leaf-length')
        this)

      :else (do (set! exhausted? true)
                this)))
  (isExhausted [_] exhausted?)
  (getZipper [_] zipper)
  (getOffset [_] (+ node-offset inner-offset))
  (getNodeOffset [_] node-offset)
  (getInnerOffset [_] inner-offset)
  (getLeafLength [_] leaf-length)
  (getChar [_] (when (not exhausted?) (get-char-from-loc zipper inner-offset))))

#?(:clj
   (defmacro ->transient-cursor [{:keys [zipper node-offset text-length
                                         inner-offset leaf-length exhausted]}]
     `(->TransientCursor ~zipper ~node-offset ~text-length
                         ~inner-offset ~leaf-length ~exhausted)))

(defn transient [^Cursor cursor]
  (->transient-cursor
   {:zipper       (.-zipper cursor)
    :node-offset  (.-node-offset cursor)
    :text-length  (.-text-length cursor)
    :inner-offset (.-inner-offset cursor)
    :leaf-length  (.-leaf-length cursor)
    :exhausted? false}))

(defn persistent! [^TransientCursor cursor]
  (->cursor
   {:zipper       (.getZipper cursor)
    :node-offset  (.getNodeOffset cursor)
    :text-length  (.text-length cursor)
    :inner-offset (.getInnerOffset cursor)
    :leaf-length  (.getLeafLength cursor)}))

(def make-transient-cursor
  (comp transient make-cursor))

;;;;;;;;;;;;;;;;;;;;;; util ;;;;;;;;;;;;;;;;;;;;;

(defn set-to-offset! [^TransientCursor t-cursor ^long offset]
  (cond (= offset (.getOffset t-cursor)) t-cursor
        (< offset (.getOffset t-cursor)) (while (< offset (.getOffset t-cursor))
                                           (prev! t-cursor))
        (> offset (.getOffset t-cursor)) (while (> offset (.getOffset t-cursor))
                                           (next! t-cursor)))
  t-cursor)

(defn move-while [^Cursor cursor pred direction]
  (let [advance (case direction
                       :forward  #(.next! ^TransientCursor %)
                       :backward #(.prev! ^TransientCursor %))
        t-cursor ^TransientCursor (transient cursor)]
    (loop []
      (cond (not (pred (.getChar t-cursor)))
            [(persistent! t-cursor) false]

            (.isExhausted ^TransientCursor (advance t-cursor))
            [(persistent! t-cursor) true]

            :else
            (recur)))))

(defn distance [^Cursor from ^Cursor to]
  (Math/abs ^Integer (- (offset to) (offset from))))

(defn count-matching [cursor pred direction]
  (distance cursor (first (move-while cursor pred direction))))
