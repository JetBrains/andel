(ns andel.cursor
  (:refer-clojure :exclude [next transient persistent!])
  (:require [andel.text :as text])
  (:import [andel Cursor Cursor$AbstractCursor Cursor$ImmutableCursor Cursor$TransientCursor]))

(defn char ^long [cursor]
  (.getChar ^Cursor$AbstractCursor cursor))

(defn offset ^long [cursor]
  (.getOffset ^Cursor$AbstractCursor cursor))

(defn char-offset ^long [cursor]
  (.getCharOffset ^Cursor$AbstractCursor cursor))

(defn cursor [text-tree ^long offset]
  (Cursor$ImmutableCursor/create text-tree offset))

(defn next [cursor]
  (Cursor/next cursor))

(defn prev [cursor]
  (Cursor/prev cursor))

(defn next! [cursor]
  (.next ^Cursor$TransientCursor cursor))

(defn prev! [cursor]
  (.prev ^Cursor$TransientCursor cursor))

(defn transient [cursor]
  (Cursor/toTransient cursor))

(defn persistent! [cursor]
  (Cursor/toPersistent cursor))

(defn move-to-offset! [^Cursor$TransientCursor t-cursor ^long o]
  (cond
    (= o (offset t-cursor)) t-cursor
    (< o (offset t-cursor)) (while (< o (offset t-cursor))
                              (prev! t-cursor))
    (> o (offset t-cursor)) (while (> o (offset t-cursor))
                              (next! t-cursor)))
  t-cursor)

(defn forward-while [cursor pred]
  (loop [tc (transient cursor)]
    (if (pred (char tc))
      (if-let [tc' (next! tc)]
        (recur tc')
        [(persistent! tc) true])
      [(persistent! tc) false])))

(defn backward-while [cursor pred]
  (loop [tc (transient cursor)]
    (if (pred (char tc))
      (if-let [tc' (prev! tc)]
        (recur tc')
        [(persistent! tc) true])
      [(persistent! tc) false])))

(defn distance ^long [from to]
  (Math/abs ^long (- (offset to) (offset from))))

(defn count-matching ^long [cursor pred direction]
  (distance cursor (first (case direction
                            :backward (backward-while cursor pred)
                            :forward (forward-while cursor pred)))))
