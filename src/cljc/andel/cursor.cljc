(ns andel.cursor
  (:require [andel.tree :as tree]
            [andel.text :as text]
            [andel.array-list :as al])
  #?(:clj (:import [andel.tree ZipperLocation Leaf])))

(defn- get-leaf-length [^ZipperLocation loc]
  (assert (tree/leaf? loc)
          "calling leaf-length on non-leaf")
  (let [^Leaf leaf (tree/node loc)
        ^String s  (.-data leaf)]
    (.length s)))

(defn- get-char-from-loc [^ZipperLocation loc offset]
  (assert (tree/leaf? loc)
          "calling get-char-from-loc on non-leaf")
  (let [^Leaf leaf (tree/node loc)]
    (.charAt (.-data leaf) offset)))

;; warning: breaks zipper accumulator
(defn- left [^ZipperLocation loc]
  (when (< 0 (.-idx loc))
    (tree/z-merge loc {:idx (dec (.-idx loc))
                       :acc nil
                       :o-acc nil})))

;; warning: breaks zipper accumulator
(defn- prev-loc [^ZipperLocation loc]
  (if (tree/end? loc)
    loc
    (or
     (and (tree/branch? loc) (tree/down loc))
     (left loc)
     (loop [^ZipperLocation p loc]
       (if-let [u (tree/up p)]
         (or (left u) (recur u))
         (tree/->zipper {:ops (.-ops loc)
                         :transient? (.-transient? loc)
                         :siblings (al/into-array-list [(tree/node p)])
                         :idx 0
                         :end? true}))))))

;; warning: breaks zipper accumulator
(defn- prev-leaf [loc]
  (let [loc (prev-loc loc)]
    (if (or (tree/leaf? (tree/node loc))
            (tree/end? loc))
      loc
      (recur loc))))

(defprotocol MutableCursor
  (next! ^char [this])
  (prev! ^char [this])
  (getZipper [this])
  (getNodeOffset [this])
  (getInnerOffset [this])
  (getLeafLength [this]))

(deftype Cursor [^{:volatile-mutable true} zipper
                 ^{:volatile-mutable true} node-offset
                 text-length
                 ^{:volatile-mutable true} inner-offset
                 ^{:volatile-mutable true} leaf-length]
  MutableCursor
  (next! ^char [this]
   (cond
     (< (inc inner-offset) leaf-length)
     (do (set! inner-offset (inc inner-offset))
         (get-char-from-loc zipper inner-offset))

     (< (inc (+ node-offset inner-offset)) text-length)
     (let [next-leaf (tree/next-leaf zipper)]
       (set! zipper       next-leaf)
       (set! node-offset  (+ node-offset leaf-length))
       (set! inner-offset 0)
       (set! leaf-length  (get-leaf-length next-leaf))
       (get-char-from-loc zipper inner-offset))

     :else (char 0)))

  (prev! ^char [this]
   (cond
     (< 0 inner-offset)
     (do (set! inner-offset (dec inner-offset))
         (get-char-from-loc zipper inner-offset))

     (< 0 (+ node-offset inner-offset))
     (let [prev-leaf    (prev-leaf zipper)
           leaf-length' (get-leaf-length prev-leaf)]
       (set! zipper       prev-leaf)
       (set! node-offset  (- node-offset leaf-length'))
       (set! inner-offset (dec leaf-length'))
       (set! leaf-length  leaf-length')
       (get-char-from-loc zipper inner-offset))

     :else (char 0)))

  (getZipper [_] zipper)
  (getNodeOffset [_] node-offset)
  (getInnerOffset [_] inner-offset)
  (getLeafLength [_] leaf-length))

#?(:clj
   (defmacro ->cursor [{:keys [zipper node-offset text-length inner-offset leaf-length]}]
     `(->Cursor ~zipper ~node-offset ~text-length ~inner-offset ~leaf-length)))

(defn make-cursor [text offset]
  (let [zipper      (-> text text/zipper (text/scan-to-offset offset))
        node-offset (text/node-offset zipper)
        offset      (text/offset zipper)]
    (->cursor
     {:zipper zipper
      :node-offset  node-offset
      :text-length  (text/text-length text)
      :inner-offset (- offset node-offset)
      :leaf-length  (get-leaf-length zipper)})))

(defn get-char [^Cursor cursor]
  (when cursor
    (let [zipper       (.getZipper cursor)
          inner-offset (.getInnerOffset cursor)]
      (get-char-from-loc zipper inner-offset))))

(defn next [^Cursor cursor]
  (let [zipper       (.getZipper cursor)
        node-offset  (.getNodeOffset cursor)
        text-length  (.-text-length cursor)
        inner-offset (.getInnerOffset cursor)
        leaf-length  (.getLeafLength cursor)]
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
  (let [zipper       (.getZipper cursor)
        node-offset  (.getNodeOffset cursor)
        text-length  (.-text-length cursor)
        inner-offset (.getInnerOffset cursor)
        leaf-length  (.getLeafLength cursor)]
    (cond
      (< 0 inner-offset)
      (->cursor
       {:zipper       zipper
        :node-offset  node-offset
        :text-length  text-length
        :inner-offset (dec inner-offset)
        :leaf-length  leaf-length})

      (< 0 (+ node-offset inner-offset))
      (let [prev-leaf   (prev-leaf zipper)
            leaf-length (get-leaf-length prev-leaf)]
        (->cursor
         {:zipper       prev-leaf
          :node-offset  (- node-offset leaf-length)
          :text-length  text-length
          :inner-offset (dec leaf-length)
          :leaf-length  leaf-length}))

      :else nil)))

(defn offset [^Cursor cursor]
  (let [node-offset  (.getNodeOffset cursor)
        inner-offset (.getInnerOffset cursor)]
    (+ node-offset inner-offset)))

(comment

  (def my-cursor (-> "01234567890"
                     text/make-text
                     (make-cursor 0)))

  (.next! my-cursor)

  (.-text-length my-cursor)

  )
