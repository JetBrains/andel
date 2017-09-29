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
  (:refer-clojure :exclude (replace remove next skip))
  #?(:cljs (:require-macros [andel.tree :refer [->zipper z-merge]])
     :clj (:import [java.util ArrayList Collection])))

(defrecord Node [metrics children])
(defrecord Leaf [metrics data])
(defrecord ZipperOps [branch?
                      children
                      make-node

                      reducing-fn
                      metrics-fn
                      leaf-overflown?
                      split-thresh
                      split-leaf
                      leaf-underflown?
                      merge-leafs])
(defrecord ZipperLocation [^ZipperOps ops node l r changed? acc o-acc pzip end? root?])

(defn metrics [x]
  (if (node? x)
    (.-metrics ^Node x)
    (.-metrics ^Leaf x)))

#?(:clj
   (defmacro ->zipper [{:keys [ops node l r changed? acc o-acc pzip end? root?]}]
     `(->ZipperLocation ~ops ~node ~l ~r ~changed? ~acc ~o-acc ~pzip ~end? ~root?)))

#?(:clj
   (defmacro z-merge [loc props]
     (let [loc-sym (with-meta (gensym "loc") {:tag `ZipperLocation})]
       `(let [~loc-sym ~loc]
          (->zipper ~{:ops (or (:ops props) `(.-ops ~loc-sym))
                      :node (or (:node props) `(.-node ~loc-sym))
                      :l (or (:l props) `(.-l ~loc-sym))
                      :r (or (:r props) `(.-r ~loc-sym))
                      :changed? (or (:changed? props) `(.-changed? ~loc-sym))
                      :acc (or (:acc props) `(.-acc ~loc-sym))
                      :o-acc (or (:o-acc props) `(.-o-acc ~loc-sym))
                      :pzip (or (:pzip props) `(.-pzip ~loc-sym))
                      :end? (or (:end? props) `(.-end? ~loc-sym))
                      :root? (or (:root? props) `(.-root? ~loc-sym))})))))

(defn assoc-o-acc [loc o-acc]
  (z-merge loc {:o-acc o-acc}))

#?(:clj (defn push!
           "Arity 1 added to be trancducer-friendly"
           ([a] a)
           ([^ArrayList a x] (.add a x) a))
   :cljs (defn push!
           "Arity 1 added to be trancducer-friendly"
           ([a] a)
           ([a x] (.push a x) a)))

#?(:clj (defn into-array-list [^Collection coll]
          (ArrayList. coll))
   :cljs (def into-array-list into-array))

#?(:clj (defn array-list? [x]
         (instance? ArrayList x))
   :cljs (def array-list? array?))

#?(:clj (defn sub-array-list [^ArrayList x from to]
          (.subList x from to))
   :cljs (defn sub-array-list [x from to]
           (.slice x from to)))

#?(:clj (defn al-get [^ArrayList a i]
          (.get a i))
   :cljs (defn al-get [a i] (aget a i)))

(defn make-node [children config-map]
  ((:make-node config-map) (if (array-list? children) children (into-array-list children))))

(defn make-leaf [data {:keys [metrics-fn]}]
  (Leaf. (metrics-fn data)
         data))

#?(:clj (do
          (defn node? [x]
            (instance? Node x))

          (defn leaf? [x] (not (node? x))))
   :cljs (do
           (defn ^boolean node? [x]
             (instance? Node x))

           (defn ^boolean leaf? [x] (not (node? x)))))

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
                                (fn [children] (make-node-fn (if (array-list? children)
                                                               children
                                                               (into-array-list children))))
                                reducing-fn
                                metrics-fn
                                leaf-overflown?
                                split-thresh
                                split-leaf
                                leaf-underflown?
                                merge-leafs)
               :node tree
               :root? true})))

(defn mark-changed [loc]
  (z-merge loc {:changed? true}))

(defn node
  "Returns the node at loc"
  [^ZipperLocation loc]
  (.-node loc))

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
  ((.-branch? ^ZipperOps (.-ops loc)) (.-node loc)))

(defn children
  "Returns a seq of the children of node at loc, which must be a ?"
  [^ZipperLocation loc]
  ((.-children ^ZipperOps (.-ops loc)) (.-node loc)))

(defn fast-some [pred coll]
  (reduce (fn [_ c] (if (pred c) (reduced true) false)) false coll))

(defn nodes? [c]
  (node?
   (if (array-list? c)
      (al-get c 0)
      (first c))))

(defn root? [^ZipperLocation loc]
  (.-root? loc))

(defn log2 [i]
  #?(:clj (/ (java.lang.Math/log i) (java.lang.Math/log 2))
     :cljs (js/Math.log2 i))
  )

(defn ceil [x]
  #?(:clj (java.lang.Math/ceil x)
     :cljs (js/Math.ceil x)))

(defn pow [x y]
  #?(:clj (java.lang.Math/pow x y)
     :cljs (js/Math.pow x y)))

(defn chunk-size [c thresh]
  (ceil (/ c (pow 2 (ceil (log2 (/ c thresh)))))))

(defn split-needed? [children ^ZipperOps config]
  (let [leaf-overflown? (.-leaf-overflown? config)
        split-thresh (.-split-thresh config)]
    (fast-some (if (nodes? children)
                 (fn [^Node node] (<= split-thresh (count (.-children node))))
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
          (cond (and (node? node) (<= split-thresh (count (.-children ^Node node))))

                (transduce (comp (partition-all (chunk-size (count (.-children ^Node node)) split-thresh))
                                 (map make-node-fn))
                           push!
                           result
                           (.-children ^Node node))

                (and (leaf? node) (leaf-overflown? (.-data ^Leaf node)))
                (reduce push! result (map #(make-leaf % config) (split-leaf (.-data ^Leaf node))))

                :else (push! result node)))
        (into-array-list [])
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
        make-node (.-make-node config)]
    (if (merge-needed? children config)
      (if (nodes? children)
        (let [merge-thresh (quot split-thresh 2)]
          (loop [i 1
                 result (into-array-list [])
                 ^Node left (al-get children 0)]
            (if (< i (count children))
              (let [^Node right (al-get children i)
                    left-children (.-children left)
                    right-children (.-children right)
                    left-c (count left-children)
                    right-c (count right-children)]
                (if (or (< left-c merge-thresh) (< right-c merge-thresh))
                  (if (<= split-thresh (+ left-c right-c))
                    (let [n (quot (+ left-c right-c) 2)
                          s (as-> (into-array-list []) al
                                  (reduce push! al left-children)
                                  (reduce push! al right-children))
                          children-left (sub-array-list s 0 n)
                          children-right (sub-array-list s n (count s))]
                      (recur (inc i)
                             (push! result (make-node (merge-children children-left config)))
                             (make-node children-right config)))
                    (recur (inc i)
                           result
                           (make-node (merge-children (as-> (into-array-list []) a
                                                            (reduce push! a left-children)
                                                            (reduce push! a right-children)) config) config)))
                  (recur (inc i) (push! result left) right)))
              (push! result left))))
        (loop [i 1
               result (into-array-list [])
               left-data (.-data ^Leaf (al-get children 0))]
          (if (< i (count children))
            (let [^Leaf right (al-get children i)]
              (if (or (leaf-underflown? left-data) (leaf-underflown? (.-data right)))
                (let [merged-data (merge-leafs left-data (.-data right))]
                  (if (leaf-overflown? merged-data)
                    (let [[left-data right-data] (split-leaf merged-data)]
                      (recur (inc i)
                             (push! result (make-leaf left-data config))
                              right-data))
                    (recur (inc i)
                           result
                           merged-data)))
                (recur (inc i)
                       (push! result (make-leaf left-data config))
                       (.-data right))))
            (push! result (make-leaf left-data config)))))
      children)))

(defn balance-children [children config]
  (assert (array-list? children))
  (-> children
      (split-children config)
      (merge-children config)))

(defn grow-tree [children ^ZipperOps config]
  (let [split-thresh (.-split-thresh config)
        make-node-fn (.-make-node config)
        balanced-children (balance-children children config)]
    (if (< (count balanced-children) split-thresh)
      (make-node-fn balanced-children)
      (recur (into-array-list [(make-node-fn balanced-children)]) config))))


(defn shrink-tree [^Node node]
  (let [children (.-children node)
        [c] children]
    (if (and (node? c) (= 1 (count children)))
      (recur c)
      node)))

(defn replace
  "Replaces the node at this loc, without moving"
  [^ZipperLocation loc node]
  (z-merge loc {:node node
                :changed? true}))

(defn up [^ZipperLocation loc]
  (let [node (.-node loc)
        changed? (.-changed? loc)
        pzip (.-pzip loc)
        ^ZipperOps config (.-ops loc)
        make-node-fn (.-make-node config)]
    (if changed?
      (if (some? pzip)
        (let [children (as-> (into-array-list []) a
                             (reduce push! a (.-l loc))
                             (cond-> a
                                     (some? (.-node loc)) (push! (.-node loc)))
                             (reduce push! a (.-r loc)))]
          (replace pzip (make-node-fn (balance-children children config))))
        (->zipper {:ops config
                   :node (shrink-tree (grow-tree (into-array-list [node]) config))
                   :root? true}))
      pzip)))

(defn right [^ZipperLocation loc]
  (when-let [[r & rs] (.-r loc)]
    (let [reducing-fn (.-reducing-fn ^ZipperOps (.-ops loc))
          ^Node node (.-node loc)]
      (z-merge loc {:node r
                    :l (conj (.-l loc) node)
                    :r rs
                    :acc (reducing-fn (acc loc) (.-metrics node))
                    :o-acc nil}))))

(defn down [^ZipperLocation loc]
  (when (branch? loc)
    (when-let [[c & cs] (children loc)]
      (->zipper
       {:ops (.-ops loc)
        :node c
        :l []
        :r cs
        :acc (.-acc loc)
        :pzip loc}))))

(defn end?
  "Returns true if loc represents the end of a depth-first walk"
  [^ZipperLocation loc]
  (.-end? loc))

(defn root
  "Modified version of clojure.zip/root to work with balancing version of up"
  [^ZipperLocation loc]
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
                    :node (.-node p)
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
                    :node (.-node p)
                    :end? true}))))))

(defn insert-right
  "Inserts the item as the right sibling of the node at this loc, without moving"
  [^ZipperLocation loc item]
  (assert (some? (.-r loc)) "Insert at top")
  (z-merge loc {:r (cons item (.-r loc))
                :changed? true}))

(defn edit
  "Replaces the node at this loc with the value of (f node args)"
  [^ZipperLocation loc f & args]
  (replace loc (apply f (.-node loc) args)))

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
            (reduce [this f] (reduction (f) f))
            clojure.lang.IReduceInit
            (reduce [this init f] (reduction init f)))
          :cljs (reify IReduce
                  (-reduce [this f] (reduction (f) f))
                  (-reduce [this init f] (reduction init f)))))

(defn scan [^ZipperLocation loc pred]
  (if (end? loc)
    loc
    (let [node (.-node loc)
          rights (.-r loc)
          lefts (.-l loc)
          acc (.-acc loc)
          reducing-fn (.-reducing-fn ^ZipperOps (.-ops loc))
          next-loc (if (root? loc)
                     loc
                     (loop [l (transient lefts)
                            ^Node n node
                            r rights
                            acc (or acc (reducing-fn))]
                       (when (some? n)
                         (let [m (.-metrics n)]
                           (if (pred acc m)
                             (->zipper {:ops (.-ops loc)
                                        :node n
                                        :l (persistent! l)
                                        :r (seq r)
                                        :pzip (.-pzip loc)
                                        :changed? (.-changed? loc)
                                        :acc acc})
                             (recur (conj! l n)
                                    (first r)
                                    (rest r)
                                    (reducing-fn acc m)))))))]
      (if (some? next-loc)
        (if (branch? next-loc)
          (recur (down next-loc) pred)
          next-loc)
        (recur (skip (up loc)) pred)))))

(defn insert-left
  "Inserts the item as the left sibling of the node at this loc, without moving"
  [^ZipperLocation loc item]
  (assert (some? (.-l loc)) "Insert at top")
  (let [reducing-fn (.-reducing-fn ^ZipperOps (.-ops loc))]
    (z-merge loc {:l (conj (.-l loc) item)
                  :changed? true
                  :acc (reducing-fn (.-acc loc)
                                    (metrics item))})))

(defn remove [^ZipperLocation loc]
  (let [node (.-node loc)
        make-node-fn (.-make-node ^ZipperOps (.-ops loc))]
    (if (root? loc)
      (replace loc (make-node-fn []))
      (if-let [[r & rs] (.-r loc)]
        (->zipper {:ops (.-ops loc)
                   :node r
                   :l (.-l loc)
                   :r rs
                   :pzip (.-pzip loc)
                   :changed? true
                   :acc (.-acc loc)
                   :o-acc (.-o-acc loc)})
        (if (pos? (count (.-l loc)))
          (skip (->zipper {:ops (.-ops loc)
                           :node (peek (.-l loc))
                           :l (pop (.-l loc))
                           :r (.-r loc)
                           :pzip (.-pzip loc)
                           :changed? true}))
          (recur (up loc)))))))

(defn compare-zippers [z1 z2 stop?]
  (loop [z1 z1
         z2 z2]
    (if (identical? (node z1) (node z2))
      (if (or (stop? (loc-acc z1) (metrics (node z1)))
              (and (end? z1) (end? z2)))
        true
        (recur (next z1) (next z2)))
      false)))
