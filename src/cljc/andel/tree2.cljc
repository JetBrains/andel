(ns andel.tree2
  (:import [andel Rope Rope$Zipper Rope$ZipperOps]))

(defn assoc-o-acc [loc o-acc]
  (let [loc' (.clone loc)]
    (set! (. loc' oacc) o-acc)
    loc'))

(defn make-node [children config-map]
  ;;todo
  )

(defn make-leaf [data zipper-ops]
  (Rope/makeLeaf data zipper-ops))

(defn metrics [n]
  (Rope/metrics n))

(defn memoize-by-ref
  [f]
  (let [^java.util.concurrent.ConcurrentHashMap mem (java.util.concurrent.ConcurrentHashMap.)]
    (fn [arg]
      (let [key (System/identityHashCode arg)]
        (if-let [res (.get mem key)]
          res
          (let [res (f arg)]
            (.put mem key res)
            res))))))

(def ->zipper-ops
  (memoize-by-ref
   (fn [{:keys [reducing-fn
                metrics-fn
                leaf-overflown?
                split-thresh
                split-leaf
                leaf-underflown?
                merge-leafs
                make-node] :as config}]
     (reify Rope$ZipperOps
            (calculateMetrics [_ data] (metrics-fn data))
            (emptyMetrics [_] (reducing-fn))
            (rf [_ m1 m2] (reducing-fn m1 m2))
            (isLeafOverflown [_ data] (leaf-overflown? data))
            (isLeafUnderflown [_ data] (leaf-underflown? data))
            (mergeLeaves [_ d1 d2] (merge-leafs d1 d2))
            (splitLeaf [_ data] (split-leaf data))
            (splitThreshold [_] split-thresh)))))

(defn zipper [tree config]
  (Rope$Zipper/zipper tree (->zipper-ops config)))

(defn transient [loc]
  loc)

(defn mark-changed [loc]
  loc)

(defn node
  "Returns the node at loc"
  [loc]
  (Rope/node loc))

(defn acc
  "Returns the acc at loc"
  [loc]
  (Rope/currentAcc loc)
  (or (.-acc loc)
      ((.-reducing-fn ^ZipperOps (.-ops loc)))))

(defn loc-acc [loc]
  (acc loc))

(defn branch?
  "Returns true if the node at loc is a branch"
  [loc]
  (Rope/isBranch loc))

(defn children
  "Returns a seq of the children of node at loc, which must be a ?"
  [loc]
  (Rope/getChildren (Rope/node loc)))

(defn fast-some [pred coll]
  (reduce (fn [_ c] (if (pred c) (reduced true) false)) false coll))

(defn root? [loc]
  (.-isRoot loc))

(defn replace
  "Replaces the node at this loc, without moving"
  [loc node]
  (Rope/replace loc node))

(defn up [loc]
  (Rope/up loc))

(defn right [loc]
  (Rope/right loc))

(defn down-forward [loc]
  (Rope/downLeft loc))

(defn down-backward [loc]
  (Rope/downRight loc))

(defn end?
  "Returns true if loc represents the end of a depth-first walk"
  [loc]
  (.-isEnd loc))

(defn root
  "Modified version of clojure.zip/root to work with balancing version of up"
  [loc]
  (Rope/root loc))

(defn next
  "Modified version of clojure.zip/next to work with balancing version of up"
  [loc]
  (Rope/next loc))

(defn skip
  "Just like next but not going down"
  [loc]
  (Rope/skip loc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn prev-leaf
  "warning: breaks zipper accumulator"
  [loc]
  (throw (java.lang.UnsupportedOperationException.)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ->fn [clj-fn]
  (reify java.util.function.Function
     (apply [_ arg] (clj-fn arg))))

(defn ->bi-fn [clj-fn]
  (reify java.util.function.BiFunction
     (apply [_ arg1 arg2] (clj-fn arg1 arg2))))

(defn edit
  "Replaces the node at this loc with the value of (f node args)"
  [loc f & args]
  (Rope/edit loc (->fn (fn [n] (apply f n args)))))

(defn next-leaf [loc]
  (Rope/nextLeaf loc))

(defn reducible [reduction]
  (reify
   clojure.lang.IReduce
   (reduce [this f] (reduction f (f)))

   clojure.lang.IReduceInit
   (reduce [this f init] (reduction f init))))

(defn scan [loc pred]
  (Rope/scan loc (->bi-fn pred)))

(defn insert-left
  "Inserts the item as the left sibling of the node at this loc, without moving"
  [loc item]
  (throw (java.lang.UnsupportedOperationException.)))

(defn remove [loc]
  (Rope/remove loc))

(defn compare-zippers [z1 z2 stop?]
  (throw (java.lang.UnsupportedOperationException.)))