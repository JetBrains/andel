(ns andel.cursor
  (:refer-clojure :exclude [next transient persistent!])
  (:import [andel.text Text Cursor]))

(defn codepoint ^long [^Cursor cursor]
  (.codepoint cursor))

(defn offset ^long [^Cursor cursor]
  (.offset cursor))

(defn char-offset ^long [^Cursor cursor]
  (.charOffset cursor))

(defn cursor ^Cursor [^Text text-tree ^long offset]
  (Cursor/atOffset text-tree offset false))

(defn next [^Cursor cursor]
  (assert (not (.isTransient cursor)))
  (.next cursor))

(defn prev [^Cursor cursor]
  (assert (not (.isTransient cursor)))
  (.prev cursor))

(defn next! [^Cursor cursor]
  (assert (.isTransient cursor))
  (.next cursor))

(defn prev! [^Cursor cursor]
  (assert (.isTransient cursor))
  (.prev cursor))

(defn transient [^Cursor cursor]
  (.asTransient cursor))

(defn persistent! [^Cursor cursor]
  (.asPersistent cursor))

(defn forward-while [cursor pred]
  (loop [tc (transient cursor)]
    (if (pred (codepoint tc))
      (if-let [tc' (next! tc)]
        (recur tc')
        [(persistent! tc) true])
      [(persistent! tc) false])))

(defn backward-while [cursor pred]
  (loop [tc (transient cursor)]
    (if (pred (codepoint tc))
      (if-let [tc' (prev! tc)]
        (recur tc')
        [(persistent! tc) true])
      [(persistent! tc) false])))
