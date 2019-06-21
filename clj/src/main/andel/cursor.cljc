(ns andel.cursor
  (:refer-clojure :exclude [next transient persistent! char])
  (:import [andel Cursor Cursor$PersistentCursor Cursor$TransientCursor]))

(defn char ^long [cursor]
  (Cursor/codepoint cursor))

(defn offset ^long [cursor]
  (Cursor/offset cursor))

(defn char-offset ^long [cursor]
  (Cursor/charOffset cursor))

(defn cursor [text-tree ^long offset]
  (Cursor$PersistentCursor/createAtOffset text-tree offset))

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
  (let [last-matching (first (case direction
                            :backward (backward-while cursor pred)
                            :forward (forward-while cursor pred)))]
    (distance cursor last-matching)))
