(ns andel.array-list
  (:refer-clojure :exclude (get set assoc insert remove assoc! conj!))
  (:import [java.util ArrayList Collection]))

(defn conj!
   "Arity 1 added to be trancducer-friendly"
   ([a] a)
   ([^ArrayList a x] (.add a x) a))

(defn conj-all! [^ArrayList a ^Collection xs]
   (.addAll a xs)
   a)

(defn empty-array-list [] (ArrayList.))

(defn into-array-list
  ([coll]
   (into-array-list identity coll))
  ([xf coll]
   (transduce xf conj! (ArrayList.) coll)))

(defn array-list? [x]
   (instance? ArrayList x))

(defn sublist [^ArrayList x ^long from ^long to]
   (.subList x from to))

(defn get [^ArrayList a ^long i]
   (.get a i))

(defn assoc! [^ArrayList a ^long i v]
  (.set a i v)
  a)

(defn length [^ArrayList a] (.size a))

(defn ->array-list [c]
  (if (array-list? c)
    c
    (into-array-list c)))

(defn assoc [^ArrayList al ^long idx v]
  (assoc! (into-array-list al) idx v))

(defn insert! [^ArrayList al ^long idx v]
  (.add al idx v))

(defn remove! [^ArrayList al ^long idx]
  (.remove al (int idx))
  al)

(defn insert [al ^long idx x]
  (-> (into-array-list (sublist al 0 idx))
      (conj! x)
      (conj-all! (sublist al idx (count al)))))

(defn remove [al ^long idx]
  (-> (into-array-list (sublist al 0 idx))
      (conj-all! (sublist al (inc idx) (count al)))))

