;;   Copyright (c) Rich Hickey. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;;   functional hierarchical zipper, with navigation, editing and enumeration
;;   see Huet

(ns andel.tree
  (:refer-clojure :exclude (replace remove next skip transient))
  (:require [andel.array-list :as al]
            [clojure.pprint :as pp])
  #?(:cljs (:require-macros [andel.tree :refer [->zipper z-merge]])))

(defrecord Node [metrics children])
(defrecord Leaf [metrics data])
(defrecord ZipperOps [branch?
                      children
                      make-node
                      reducing-fn
                      metrics-fn
                      leaf-overflown?
                      ^long split-thresh
                      split-leaf
                      leaf-underflown?
                      merge-leafs])

(defrecord ZipperLocation [ops
                           siblings
                           ^long idx
                           changed?
                           transient?
                           acc
                           o-acc
                           pzip
                           end?
                           root?])

#?(:clj
   (do
     (defmethod print-method andel.tree.ZipperLocation [o ^java.io.Writer w]
       (.write w (str "[andel.tree.ZipperLocation@" (Integer/toHexString (hash o)) "]")))

     (defmethod pp/simple-dispatch andel.tree.ZipperLocation [o]
       (pr (str "[andel.tree.ZipperLocation@" (Integer/toHexString (hash o)) "]")))

     (defmethod print-method andel.tree.Node [o ^java.io.Writer w]
       (.write w (str "[andel.tree.Node@" (Integer/toHexString (hash o)) "]")))

     (defmethod pp/simple-dispatch andel.tree.Node [o]
       (pr (str "[andel.tree.Node@" (Integer/toHexString (hash o)) "]")))

     ))

#?(:clj
   (defmacro ->zipper [{:keys [ops siblings idx changed? transient? acc o-acc pzip end? root?]}]
     `(->ZipperLocation ~ops ~siblings ~idx ~changed? ~transient? ~acc ~o-acc ~pzip ~end? ~root?)))

#?(:clj
   (defmacro z-merge [loc props]
     (let [loc-sym (with-meta (gensym "loc") {:tag `ZipperLocation})]
       `(let [~loc-sym ~loc]
          (->zipper ~{:ops (or (:ops props) `(.-ops ~loc-sym))
                      :siblings (or (:siblings props) `(.-siblings ~loc-sym))
                      :idx (or (:idx props) `(.-idx ~loc-sym))
                      :changed? (or (:changed? props) `(.-changed? ~loc-sym))
                      :acc (or (:acc props) `(.-acc ~loc-sym))
                      :o-acc (or (:o-acc props) `(.-o-acc ~loc-sym))
                      :pzip (or (:pzip props) `(.-pzip ~loc-sym))
                      :end? (or (:end? props) `(.-end? ~loc-sym))
                      :transient? (or (:transient? props) `(.-transient? ~loc-sym))
                      :root? (or (:root? props) `(.-root? ~loc-sym))})))))

(defn assoc-o-acc [loc o-acc]
  (z-merge loc {:o-acc o-acc}))

(defn make-node [children config-map]
  ((:make-node config-map) (if (al/array-list? children)
                             children
                             (al/into-array-list children))))

(defn make-leaf [data {:keys [metrics-fn]}]
  (Leaf. (metrics-fn data)
         data))

#?(:clj (do
          (defn node? [x]
            (instance? Node x))

          (defn leaf? [x] (instance? Leaf x)))
   :cljs (do
           (defn ^boolean node? [x]
             (instance? Node x))

           (defn ^boolean leaf? [x] (instance? Leaf x))))

(defn metrics [x]
  (if (node? x)
    (.-metrics ^Node x)
    (.-metrics ^Leaf x)))

(defn zipper [tree {:keys [reducing-fn
                           metrics-fn
                           leaf-overflown?
                           split-thresh
                           split-leaf
                           leaf-underflown?
                           merge-leafs] :as config}]
  (let [make-node-fn (:make-node config)]
    (assert (some? tree))
    (->zipper {:ops (ZipperOps. node?
                                (fn [^Node x] (.-children x))
                                (fn [children] (make-node-fn (al/->array-list children)))
                                reducing-fn
                                metrics-fn
                                leaf-overflown?
                                split-thresh
                                split-leaf
                                leaf-underflown?
                                merge-leafs)
               :siblings (al/into-array-list [tree])
               :transient? false
               :idx 0
               :root? true})))

(defn transient [loc]
  (z-merge loc {:transient true}))

(defn mark-changed [loc]
  (z-merge loc {:changed? true}))

(defn node
  "Returns the node at loc"
  ^Node [^ZipperLocation loc]
  (al/get (.-siblings loc) (.-idx loc)))

(defn acc
  "Returns the acc at loc"
  [^ZipperLocation loc]
  (or (.-acc loc)
      ((.-reducing-fn ^ZipperOps (.-ops loc)))))

(defn loc-acc [loc]
  (acc loc))

(defn branch?
  "Returns true if the node at loc is a branch"
  [^ZipperLocation loc]
  ((.-branch? ^ZipperOps (.-ops loc)) (node loc)))

(defn children
  "Returns a seq of the children of node at loc, which must be a ?"
  [^ZipperLocation loc]
  ((.-children ^ZipperOps (.-ops loc)) (node loc)))

(defn fast-some [pred coll]
  (reduce (fn [_ c] (if (pred c) (reduced true) false)) false coll))

(defn nodes? [c]
  (node?
   (if (al/array-list? c)
      (al/get c 0)
      (first c))))

(defn root? [^ZipperLocation loc]
  (.-root? loc))

(defn log2 [i]
  #?(:clj (/ (java.lang.Math/log i) (java.lang.Math/log 2))
     :cljs (js/Math.log2 i))
  )

(defn ceil ^double [^double x]
  #?(:clj (java.lang.Math/ceil x)
     :cljs (js/Math.ceil x)))

(defn pow ^double [^double x ^double y]
  #?(:clj (java.lang.Math/pow x y)
     :cljs (js/Math.pow x y)))


(defn chunk-size ^long [^long c ^long thresh]
  (long (ceil (/ (double c) (pow (double 2) (ceil (log2 (/ (double c) (double thresh)))))))))

(defn split-needed? [children ^ZipperOps config]
  (let [leaf-overflown? (.-leaf-overflown? config)
        split-thresh (.-split-thresh config)]
    (fast-some (if (nodes? children)
                 (fn [^Node node] (< split-thresh (count (.-children node))))
                 (fn [^Leaf leaf] (leaf-overflown? (.-data leaf))))
               children)))

(defn split-children [children ^ZipperOps config]
  (let [leaf-overflown? (.-leaf-overflown? config)
        split-leaf (.-split-leaf config)
        split-thresh (.-split-thresh config)
        make-node-fn (.-make-node config)]
    (if (split-needed? children config)
      (reduce
       (fn [result node]
         (cond (and (node? node) (< split-thresh (count (.-children ^Node node))))

               (transduce (comp (partition-all (chunk-size (count (.-children ^Node node)) split-thresh))
                                (map make-node-fn))
                          al/conj!
                          result
                          (.-children ^Node node))

               (and (leaf? node) (leaf-overflown? (.-data ^Leaf node)))
               (reduce al/conj! result (map #(make-leaf % config) (split-leaf (.-data ^Leaf node))))

               :else (al/conj! result node)))
       (al/into-array-list [])
       children)
      children)))

(defn merge-needed? [children ^ZipperOps config]
  (let [leaf-underflown? (.-leaf-underflown? config)
        split-thresh (.-split-thresh config)
        merge-thresh (quot split-thresh 2)
        merge? (if (nodes? children)
                 #(< (count (.-children ^Node %)) merge-thresh)
                 #(leaf-underflown? (.-data ^Leaf %)))]
    (fast-some merge? children)))

(defn merge-children [children ^ZipperOps config]
  (let [leaf-underflown? (.-leaf-underflown? config)
        merge-leafs (.-merge-leafs config)
        leaf-overflown? (.-leaf-overflown? config)
        split-leaf (.-split-leaf config)
        split-thresh (.-split-thresh config)
        make-node-fn (.-make-node config)]
    (if (merge-needed? children config)
      (if (nodes? children)
        (let [merge-thresh (quot split-thresh 2)]
          (loop [i 1
                 result (al/into-array-list [])
                 ^Node left (al/get children 0)]
            (if (< i (count children))
              (let [^Node right (al/get children i)
                    left-children (.-children left)
                    right-children (.-children right)
                    left-c (count left-children)
                    right-c (count right-children)]
                (if (or (< left-c merge-thresh) (< right-c merge-thresh))
                  (if (<= split-thresh (+ left-c right-c))
                    (let [n (quot (+ left-c right-c) 2)
                          s (as-> (al/into-array-list []) al
                                  (reduce al/conj! al left-children)
                                  (reduce al/conj! al right-children))
                          children-left (al/sublist s 0 n)
                          children-right (al/sublist s n (count s))]
                      (recur (inc i)
                             (al/conj! result (make-node-fn (merge-children children-left config)))
                             (make-node-fn children-right)))
                    (recur (inc i)
                           result
                           (make-node-fn (merge-children (as-> (al/into-array-list []) a
                                                            (reduce al/conj! a left-children)
                                                            (reduce al/conj! a right-children)) config))))
                  (recur (inc i) (al/conj! result left) right)))
              (al/conj! result left))))
        (loop [i 1
               result (al/into-array-list [])
               left-data (.-data ^Leaf (al/get children 0))]
          (if (< i (count children))
            (let [^Leaf right (al/get children i)]
              (if (or (leaf-underflown? left-data) (leaf-underflown? (.-data right)))
                (let [merged-data (merge-leafs left-data (.-data right))]
                  (if (leaf-overflown? merged-data)
                    (let [[left-data right-data] (split-leaf merged-data)]
                      (recur (inc i)
                             (al/conj! result (make-leaf left-data config))
                             right-data))
                    (recur (inc i)
                           result
                           merged-data)))
                (recur (inc i)
                       (al/conj! result (make-leaf left-data config))
                       (.-data right))))
            (al/conj! result (make-leaf left-data config)))))
      children)))

(defn balance-children [children config]
  (assert (al/array-list? children))
  (-> children
      (split-children config)
      (merge-children config)))

(defn grow-tree [children ^ZipperOps config]
  (let [split-thresh (.-split-thresh config)
        make-node-fn (.-make-node config)
        balanced-children (balance-children children config)]
    (if (< (count balanced-children) split-thresh)
      (make-node-fn balanced-children)
      (recur (al/into-array-list [(make-node-fn balanced-children)]) config))))


(defn shrink-tree [^Node node]
  (let [children (.-children node)
        [c] children]
    (if (and (node? c) (= 1 (count children)))
      (recur c)
      node)))

(defn mutable? [^ZipperLocation loc]
  (and (.-changed? loc) (.-transient? loc)))

(defn replace
  "Replaces the node at this loc, without moving"
  [^ZipperLocation loc node]
  (z-merge loc {:siblings ((if (mutable? loc) al/assoc! al/assoc)
                           (.-siblings loc) (.-idx loc) node)
                :changed? true}))

(defn first-child-idx [children direction]
  (if (= :forward direction)
    0
    (dec (count children))))

(defn up [^ZipperLocation loc]
  (let [node (node loc)
        changed? (.-changed? loc)
        pzip (.-pzip loc)
        ^ZipperOps config (.-ops loc)
        make-node-fn (.-make-node config)]
    (if changed?
      (if (some? pzip)
        (let [children (.-siblings loc)]
          (replace pzip (make-node-fn (balance-children children config))))
        (->zipper {:ops config
                   :transient? (.-transient? loc)
                   :idx 0
                   :siblings (al/into-array-list [(shrink-tree (grow-tree (al/into-array-list [node]) config))])
                   :root? true}))
      pzip)))

(defn right [^ZipperLocation loc]
  (when (< (.-idx loc) (dec (count (.-siblings loc))))
    (let [reducing-fn (.-reducing-fn ^ZipperOps (.-ops loc))]
      (z-merge loc {:idx (inc (.-idx loc))
                    :acc (reducing-fn (acc loc) (metrics (node loc)))
                    :o-acc nil}))))

(defn down [^ZipperLocation loc]
  (when (branch? loc)
    (when-let [cs (children loc)]
      (->zipper
       {:siblings (al/->array-list cs)
        :idx 0
        :transient? (.-transient? loc)
        :ops (.-ops loc)
        :acc (.-acc loc)
        :pzip loc}))))

(defn end?
  "Returns true if loc represents the end of a depth-first walk"
  [^ZipperLocation loc]
  (.-end? loc))

(defn root
  "Modified version of clojure.zip/root to work with balancing version of up"
  ^Node [^ZipperLocation loc]
  (if (end? loc)
    (node loc)
    (let [p (up loc)]
      (if (some? p)
        (recur p)
        (node loc)))))

(defn next
  "Modified version of clojure.zip/next to work with balancing version of up"
  [^ZipperLocation loc]
  (if (end? loc)
    loc
    (or
     (and (branch? loc) (down loc))
     (right loc)
     (loop [^ZipperLocation p loc]
       (if-let [u (up p)]
         (or (right u) (recur u))
         (->zipper {:ops (.-ops loc)
                    :transient? (.-transient? loc)
                    :siblings (al/into-array-list [(node p)])
                    :idx 0
                    :end? true}))))))

(defn skip
  "Just like next but not going down"
  [^ZipperLocation loc]
  (if (end? loc)
    loc
    (or
     (right loc)
     (loop [^ZipperLocation p loc]
       (if-let [u (up p)]
         (or (right u) (recur u))
         (->zipper {:ops (.-ops loc)
                    :idx 0
                    :siblings (al/into-array-list [(node p)])
                    :end? true}))))))

(defn insert-right
  "Inserts the item as the right sibling of the node at this loc, without moving"
  [^ZipperLocation loc item]
  (assert (not (root? loc)) "Insert at top")
  (z-merge loc {:siblings ((if (mutable? loc) al/insert! al/insert)
                           (.-siblings loc) (inc (.-idx loc)) item)
                :changed? true}))

(defn edit
  "Replaces the node at this loc with the value of (f node args)"
  [^ZipperLocation loc f & args]
  (replace loc (apply f (node loc) args)))

(defn insert-child
  "Inserts the item as the leftmost child of the node at this loc, without moving"
  [^ZipperLocation loc item]
  (let [make-node-fn (.-make-node ^ZipperOps (.-ops loc))]
    (replace loc (make-node-fn (cons item (children loc))))))

(defn next-leaf [^ZipperLocation loc]
  (let [loc (next loc)]
    (if (or (leaf? (node loc))
            (end? loc))
      loc
      (recur loc))))

(defn reset [^ZipperLocation loc]
  (zipper (root loc)
          (.-ops loc)))

(defn reducible [reduction]
  #?(:clj (reify
            clojure.lang.IReduce
            (reduce [this f] (reduction f (f)))
            clojure.lang.IReduceInit
            (reduce [this f init] (reduction f init)))
          :cljs (reify IReduce
                  (-reduce [this f] (reduction f (f)))
                  (-reduce [this init f] (reduction f init)))))

(defn scan [^ZipperLocation loc pred]
  (if (end? loc)
    loc
    (let [reducing-fn (.-reducing-fn ^ZipperOps (.-ops loc))
          siblings (.-siblings loc)
          siblings-count (al/length siblings)
          next-loc (if (root? loc)
                     loc
                     (loop [idx (.-idx loc)
                            a (or (.-acc loc) (reducing-fn))]
                       (when (< idx siblings-count)
                         (let [n (al/get siblings idx)
                               m (metrics n)]
                           (if (pred a m)
                             (z-merge loc
                                      {:acc a
                                       :idx idx})
                             (recur (inc idx)
                                    (reducing-fn a m)))))))]
      (if (some? next-loc)
        (if (branch? next-loc)
          (recur (down next-loc) pred)
          next-loc)
        (recur (skip (up loc)) pred)))))

(defn insert-left
  "Inserts the item as the left sibling of the node at this loc, without moving"
  [^ZipperLocation loc item]
  (assert (not (root? loc)) "Insert at top")
  (let [reducing-fn (.-reducing-fn ^ZipperOps (.-ops loc))]
    (z-merge loc {:siblings ((if (mutable? loc) al/insert! al/insert) (.-siblings loc) (.-idx loc) item)
                  :idx (inc (.-idx loc))
                  :changed? true
                  :acc (reducing-fn (.-acc loc)
                                    (metrics item))})))

(defn remove [^ZipperLocation loc]
  (let [make-node-fn (.-make-node ^ZipperOps (.-ops loc))]
    (if (root? loc)
      (replace loc (make-node-fn []))
      (if (= 1 (count (.-siblings loc)))
        (recur (up loc))
        (if (< (.idx loc) (dec (count (.-siblings loc))))
          (z-merge loc
                   {:changed? true
                    :siblings ((if (mutable? loc) al/remove! al/remove)
                               (.-siblings loc) (.-idx loc))})
          (skip (z-merge loc
                         {:changed? true
                          :siblings ((if (mutable? loc) al/remove! al/remove)
                                     (.-siblings loc) (.-idx loc))
                          :idx (dec (.-idx loc))
                          :acc nil})))))))

(defn compare-zippers [z1 z2 stop?]
  (loop [z1 z1
         z2 z2]
    (if (identical? (node z1) (node z2))
      (if (or (stop? (loc-acc z1) (metrics (node z1)))
              (and (end? z1) (end? z2)))
        true
        (recur (next z1) (next z2)))
      false)))
