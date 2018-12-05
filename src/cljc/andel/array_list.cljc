(ns andel.array-list
  (:refer-clojure :exclude (get set assoc remove assoc! conj!))
  (:import [java.util ArrayList Collection]))

(defn conj!
   "Arity 1 added to be trancducer-friendly"
   ([a] a)
   ([^ArrayList a x] (.add a x) a))

(defn conj-all! [^ArrayList a ^Collection xs]
   (.addAll a xs)
   a)

(defn into-array-list
  (^ArrayList [coll]
   (into-array-list identity coll))
  (^ArrayList [xf coll]
   (transduce xf conj! (ArrayList.) coll)))

(defn ^ArrayList array-list
  ([] (ArrayList.))
  ([e1] (doto ^ArrayList (array-list) (.add e1)))
  ([e1 e2] (doto ^ArrayList (array-list e1) (.add e2)))
  ([e1 e2 e3] (doto ^ArrayList (array-list e1 e2) (.add e3)))
  ([e1 e2 e3 e4] (doto ^ArrayList (array-list e1 e2 e3) (.add e4)))
  ([e1 e2 e3 e4 & es] (reduce conj! (array-list e1 e2 e3 e4) es)))

(defn array-list? [x]
   (instance? ArrayList x))

(defn sublist [^ArrayList x ^long from ^long to]
   (.subList x from to))

(defn get [^ArrayList a ^long i]
   (.get a i))

(defn assoc! [^ArrayList a ^long i v]
  (.set a i v)
  a)

(defn length ^long [^ArrayList a] (.size a))

(defn ->array-list [c]
  (if (array-list? c)
    c
    (into-array-list c)))

(defn assoc [^ArrayList al ^long idx v]
  (assoc! (into-array-list al) idx v))

(defn insert! [^ArrayList al ^long idx v]
  (.add al idx v)
  al)

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

