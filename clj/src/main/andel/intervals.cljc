(ns andel.intervals
  (:refer-clojure :exclude [remove])
  (:import [andel Intervals Intervals$Interval Intervals$IntervalsIterator Intervals$MergingIterator]))

(defn insert [itree markers]
  (Intervals/insertAll itree markers))

(defn remove [itree marker-ids]
  (Intervals/remove itree marker-ids))

(defn expand [itree offset len]
  (Intervals/expand itree offset len))

(defn collapse [itree offset len]
  (Intervals/collapse itree offset len))

(def empty-iterator
  (reify Intervals$IntervalsIterator
         (id [this] (throw (java.lang.UnsupportedOperationException.)))
         (from [this] (throw (java.lang.UnsupportedOperationException.)))
         (to [this] (throw (java.lang.UnsupportedOperationException.)))
         (closedLeft [this] (throw (java.lang.UnsupportedOperationException.)))
         (closedRight [this] (throw (java.lang.UnsupportedOperationException.)))
         (next [this] false)))

(defn query
  (^Intervals$IntervalsIterator [itree]
   (query itree 0 Intervals/MAX_VALUE))
  (^Intervals$IntervalsIterator [itree from]
   (query itree from Intervals/MAX_VALUE))
  (^Intervals$IntervalsIterator [itree from to]
   (if itree
     (Intervals/query itree from to)
     empty-iterator)))

(defn query-all [itree from to]
  (Intervals/queryAll itree from to))

(defn- something? [it]
  (and (some? it) (not (identical? it empty-iterator))))

(defn merge-iterators
  (^Intervals$IntervalsIterator [it1 it2]
   (cond
     (and (something? it1) (something? it2)) (Intervals$MergingIterator. it1 it2 Intervals$IntervalsIterator/FORWARD_COMPARATOR)
     (something? it1) it1
     (something? it2) it2
     :else nil))
  (^Intervals$IntervalsIterator [it1 it2 & its]
   (reduce merge-iterators (merge-iterators it1 it2) its)))

(defmacro >Interval [& {:keys [id from to greedy-left? greedy-right? attrs]}]
  `(andel.Intervals$Interval. ~id ~from ~to ~greedy-left? ~greedy-right? ~attrs))

(defn find-marker-by-id [itree id]
  (Intervals/getById itree id))

(defn- find-marker-linear [pred itree from-offset direction]
  (let [it (case direction
             :forward (query itree from-offset)
             :backward (Intervals/queryReverse itree 0 from-offset))
        ^clojure.lang.IFn$LLLOO pred (if (instance? clojure.lang.IFn$LLLOO pred)
                                       pred
                                       (fn [^long id ^long from ^long to data] (pred id from to data)))]
    (loop []
      (if (.next it)
        (if (.invokePrim pred
                         (.id it)
                         (.from it)
                         (.to it)
                         (.data it))
          (>Interval :id (.id it)
                     :from (.from it)
                     :to (.to it)
                     :greedy-left? (.closedLeft it)
                     :greedy-right? (.closedRight it)
                     :attrs (.data it))
          (recur))
        nil))))

(defn find-marker-linear-forward [pred itree from-offset]
  (find-marker-linear pred itree from-offset :forward))

(defn find-marker-linear-backward [pred itree from-offset]
  (find-marker-linear pred itree from-offset :backward))

(defn update-marker-attrs [itree id f]
  (let [interval (Intervals/getById itree id)
        new-data (f (.-data interval))]
    (-> itree
        (remove #{id})
        (insert [(>Interval :id id
                           :from (.-from interval)
                           :to (.-to interval)
                           :greedy-left? (.-closedLeft interval)
                           :greedy-right? (.-closedRight interval)
                           :attrs new-data)]))))

(defonce empty-tree
  (Intervals/empty 32))
