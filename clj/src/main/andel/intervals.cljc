(ns ^:lean-ns andel.intervals
  (:refer-clojure :exclude [remove])
  (:import [andel.intervals Intervals Interval IntervalsIterator]))

(def empty-tree (Intervals/empty))

(defn insert [^Intervals itree intervals]
  (.addIntervals itree intervals))

(defn remove [^Intervals itree marker-ids]
  (.removeByIds itree marker-ids))

(defn expand [^Intervals itree ^long offset ^long len]
  (.expand itree offset len))

(defn collapse [^Intervals itree ^long offset ^long len]
  (.collapse itree offset len))

(def empty-iterator (IntervalsIterator/fromList []))

(defn query
  (^IntervalsIterator [itree]
   (query itree 0 Long/MAX_VALUE))
  (^IntervalsIterator [itree ^long from]
   (query itree from Long/MAX_VALUE))
  (^IntervalsIterator [^Intervals itree ^long from ^long to]
   (if itree
     (.query itree from to)
     empty-iterator)))

(defn query-reverse
  (^IntervalsIterator [itree]
   (query-reverse itree 0 Long/MAX_VALUE))
  (^IntervalsIterator [itree ^long from]
   (query-reverse itree from Long/MAX_VALUE))
  (^IntervalsIterator [^Intervals itree ^long from ^long to]
   (if itree
     (.queryReverse itree from to)
     empty-iterator)))

(defn query-all [^Intervals itree ^long from ^long to]
  (.toList (query itree from to)))

(defn ii-first [^IntervalsIterator ii]
  (when (.next ii)
    ii))

(defn- something? [it]
  (and (some? it) (not (identical? it empty-iterator))))

(defn merge-iterators
  (^IntervalsIterator [it] it)
  (^IntervalsIterator [it1 it2]
   (cond
     (and (something? it1) (something? it2)) (IntervalsIterator/merge it1 it2 IntervalsIterator/FORWARD_COMPARATOR)
     (something? it1) it1
     (something? it2) it2
     :else nil))
  (^IntervalsIterator [it1 it2 & its]
   (reduce merge-iterators (merge-iterators it1 it2) its)))

(defmacro >Interval [& {:keys [id from to greedy-left? greedy-right? attrs]}]
  `(Interval. ~id ~from ~to ~greedy-left? ~greedy-right? ~attrs))

(defn find-marker-by-id ^Interval [^Intervals itree ^long id]
  (.findById itree id))
