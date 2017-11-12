(ns andel.array-list
  (:refer-clojure :exclude (get set assoc insert remove assoc! conj!))
  (:import [java.util ArrayList Collection]))

#?(:clj (defn conj!
           "Arity 1 added to be trancducer-friendly"
           ([a] a)
           ([^ArrayList a x] (.add a x) a))
   :cljs (defn conj!
           "Arity 1 added to be trancducer-friendly"
           ([a] a)
           ([a x] (.push a x) a)))

#?(:clj (defn conj-all! [^ArrayList a ^Collection xs]
          (.addAll a xs)
          a)
   :cljs (defn conj-all! [a xs]
           (reduxe conj! a xs)))

#?(:clj (defn into-array-list [^Collection coll]
          (ArrayList. coll))
   :cljs (def into-array-list into-array))

#?(:clj (defn array-list? [x]
         (instance? ArrayList x))
   :cljs (def array-list? array?))

#?(:clj (defn sublist [^ArrayList x from to]
          (.subList x from to))
   :cljs (defn sublist [x from to]
           (.slice x from to)))

#?(:clj (defn get [^ArrayList a i]
          (.get a i))
   :cljs (defn get [a i] (aget a i)))

#?(:clj (defn assoc! [^ArrayList a i v]
          (.set a i v)
          a)
   :cljs (defn set [a i v]
           (aset a i v)
           a))

(defn ->array-list [c]
  (if (array-list? c)
    c
    (into-array-list c)))

(defn assoc [^ArrayList al idx v]
  (assoc! (into-array-list al) idx v))

#?(:clj (defn insert! [^ArrayList al idx v]
          (.add al idx v)))

#?(:clj (defn remove! [^ArrayList al idx]
          (.remove al (int idx))
          al))

(defn insert [al idx x]
  (-> (into-array-list (sublist al 0 idx))
      (conj! x)
      (conj-all! (sublist al idx (count al)))))

(defn remove [al idx]
  (-> (into-array-list (sublist al 0 idx))
      (conj-all! (sublist al (inc idx) (count al)))))

