(ns andel.intervals
  (:refer-clojure :exclude [remove])
  (:import [andel Intervals Intervals$Interval Intervals$IntervalsIterator Intervals$MergingIterator]))

(defn insert [itree markers]
  (Intervals/insert itree markers))

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
     (and (something? it1) (something? it2)) (Intervals$MergingIterator. it1 it2)
     (something? it1) it1
     (something? it2) it2
     :else nil))
  (^Intervals$IntervalsIterator [it1 it2 & its]
   (reduce merge-iterators (merge-iterators it1 it2) its)))

(defmacro >Interval [& {:keys [id from to greedy-left? greedy-right? attrs]}]
  `(andel.Intervals$Interval. ~id ~from ~to ~greedy-left? ~greedy-right? ~attrs))

(defn- find-marker-loc [itree id]
  #_(let [loc (tree/scan (zipper itree) (by-id id))]
    (when-not (or (tree/end? loc)
                  (tree/node? loc))
      loc)))

(defn find-marker-by-id [itree id]
  #_(some-> (find-marker-loc itree id)
          loc->Marker))

(defn find-marker-linear-forward [pred itree from-offset]
  #_(loop [loc (tree/scan (zipper itree)
                        (by-offset from-offset))]
    (when-not (tree/end? loc)
      (assert (tree/leaf? (andel.tree/node loc)))
      (let [marker (loc->Marker loc)]
        (if (pred marker)
          marker
          (recur (tree/next-leaf loc)))))))

(defn find-marker-linear-backward [pred itree from-offset]
  #_(loop [loc (tree/prev-leaf
              (tree/scan (zipper itree)
                         (by-offset from-offset)))]
    (when-not (tree/end? loc)
      (assert (tree/leaf? (andel.tree/node loc)))
      (let [marker (loc->Marker loc)]
        (if (pred marker)
          ;; Because of prev-leaf acc might be broken which can cause
          ;; wrong from-to values in marker returned by loc->Marker.
          ;; Getting a proper marker required additional scan from root.
          (find-marker-by-id itree (.-id marker))
          (recur (tree/prev-leaf loc)))))))

(defn update-marker-attrs [itree id f]
  #_(or (some-> itree
              (find-marker-loc id)
              (update-leaf #(update % :attrs f))
              tree/root)
      itree))

(defonce empty-tree
  (Intervals. 32))
