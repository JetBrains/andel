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
  #?(:cljs (:require-macros [andel.tree :refer [->zipper z-merge]])))

(defonce records
  (do
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
    (defrecord ZipperLocation [ops node l r changed? acc o-acc pzip end? root?])
     :defined))

#?(:clj
   (defmacro ->zipper [{:keys [ops node l r changed? acc o-acc pzip end? root?]}]
     `(->ZipperLocation ~ops ~node ~l ~r ~changed? ~acc ~o-acc ~pzip ~end? ~root?)))

#?(:clj
   (defmacro z-merge [loc props]
     (let [loc-sym (gensym "loc")]
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

#?(:clj
    (do
      (defn array [& args] (object-array args))
      (def some-array (array 1 2 3))
      (defn array? [x]
        (= (type some-array) (type x)))))

(defn make-node [children config-map]
  ((:make-node config-map) (if (array? children) children (into-array children))))

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
                                #(.-children %)
                                (fn [children] (make-node-fn (if (array? children) children (into-array children))))
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
      ((.-reducing-fn (.-ops loc)))))

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

(defn partition-binary [s thresh]
  (let [cs (count s)]
    (if (< cs thresh)
      [s]
      (let [[left right] (split-at (quot cs 2) s)]
        (concat (partition-binary left thresh)
                (partition-binary right thresh))))))

(defn fast-some [pred coll]
  (reduce (fn [_ c] (if (pred c) (reduced true) false)) false coll))

(defn nodes? [c]
  (node?
    (if (array? c)
      (aget c 0)
      (first c))))

(defn root? [loc]
  (.-root? loc))

(defn split-needed? [children config]
  (let [leaf-overflown? (.-leaf-overflown? config)
        split-thresh (.-split-thresh config)]
    (fast-some (if (nodes? children)
                 #(<= split-thresh (count (.-children %)))
                 #(leaf-overflown? (.-data %)))
               children)))

(defn split-children [children config]
  (let [leaf-overflown? (.-leaf-overflown? config)
        split-leaf (.-split-leaf config)
        split-thresh (.-split-thresh config)
        make-node-fn (.-make-node config)]
    (if (split-needed? children config)
      (persistent!
       (reduce
        (fn [result node]
          (cond (and (node? node) (<= split-thresh (count (.-children node))))
                (reduce conj! result (map make-node-fn (partition-binary (.-children node) split-thresh)))

                (and (leaf? node) (leaf-overflown? (.-data node)))
                (reduce conj! result (map #(make-leaf % config) (split-leaf (.-data node))))

                :else (conj! result node)))
        (transient [])
        children))
      children)))

(defn merge-needed? [children config]
  (let [leaf-underflown? (.-leaf-underflown? config)
        split-thresh (.-split-thresh config)
        merge-thresh (quot split-thresh 2)
        merge? (if (nodes? children)
                 #(< (count (.-children %)) merge-thresh)
                 #(leaf-underflown? (.-data %)))]
    (fast-some merge? children)))

(defn merge-children [children config]
  (let [leaf-underflown? (.-leaf-underflown? config)
        merge-leafs (.-merge-leafs config)
        leaf-overflown? (.-leaf-overflown? config)
        split-leaf (.-split-leaf config)
        split-thresh (.-split-thresh config)
        make-node (.-make-node config)]
    (if (merge-needed? children config)
      (if (nodes? children)
        (let [merge-thresh (quot split-thresh 2)
              [result last] (reduce
                             (fn [[result left] right]
                               (let [left-children (.-children left)
                                     right-children (.-children right)
                                     left-c (count left-children)
                                     right-c (count right-children)]
                                 (if (or (< left-c merge-thresh) (< right-c merge-thresh))
                                   (if (<= split-thresh (+ left-c right-c))
                                     (let [[children-left children-right]
                                           (partition-binary (concat left-children right-children) split-thresh)]
                                       [(conj! result (make-node (merge-children children-left config)))
                                        (make-node children-right config)])
                                     [result (make-node (merge-children (concat left-children right-children) config) config)])
                                   [(conj! result left) right])))
                             [(transient []) (first children)]
                             (drop 1 children))]
          (persistent! (conj! result last)))
        (let [[result last] (reduce
                             (fn [[result left-data] right]
                               (if (or (leaf-underflown? left-data) (leaf-underflown? (.-data right)))
                                 (let [merged-data (merge-leafs left-data (.-data right))]
                                   (if (leaf-overflown? merged-data)
                                     (let [[left-data right-data] (split-leaf merged-data)]
                                       [(conj! result (make-leaf left-data config))
                                        right-data])
                                     [result merged-data]))
                                 [(conj! result (make-leaf left-data config)) (.-data right)]))
                             [(transient []) (.-data (first children))]
                             (drop 1 children))]
          (persistent! (conj! result (make-leaf last config)))))
      children)))

(defn balance-children [children config]
  (-> children
      (split-children config)
      (merge-children config)))

(defn grow-tree [children config]
  (let [split-thresh (.-split-thresh config)
        make-node-fn (.-make-node config)
        balanced-children (balance-children children config)]
    (if (< (count balanced-children) split-thresh)
      (make-node-fn balanced-children)
      (recur [(make-node-fn balanced-children)] config))))


(defn shrink-tree [node]
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

(defn up [loc]
  (let [node (.-node loc)
        changed? (.-changed? loc)
        pzip (.-pzip loc)
        config (.-ops loc)
        make-node-fn (.-make-node config)]
    (if changed?
      (if (some? pzip)
        (let [children (into (.-l loc) (cons (.-node loc) (.-r loc)))]
          (replace pzip (make-node-fn (balance-children children config))))
        (->zipper {:ops (.-ops loc)
                   :node (shrink-tree (grow-tree [node] config))
                   :root? true}))
      pzip)))

(defn right [loc]
  (when-let [[r & rs] (.-r loc)]
    (let [reducing-fn (.-reducing-fn (.-ops loc))
          node (.-node loc)]
      (z-merge loc {:node r
                    :l (conj (.-l loc) node)
                    :r rs
                    :acc (reducing-fn (acc loc) (.-metrics node))
                    :o-acc nil}))))

(defn down [loc]
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
  [loc]
  (if (end? loc)
    (node loc)
    (let [p (up loc)]
      (if (some? p)
        (recur p)
        (node loc)))))

(defn next
  "Modified version of clojure.zip/next to work with balancing version of up"
  [loc]
  (if (end? loc)
    loc
    (or
     (and (branch? loc) (down loc))
     (right loc)
     (loop [p loc]
       (if-let [u (up p)]
         (or (right u) (recur u))
         (->zipper {:ops (.-ops loc)
                    :node (.-node p)
                    :end? true}))))))

(defn skip
  "Just like next but not going down"
  [loc]
  (if (end? loc)
    loc
    (or
     (right loc)
     (loop [p loc]
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
  (let [make-node-fn (.-make-node (.-ops loc))]
    (replace loc (make-node-fn (cons item (children loc))))))

(defn next-leaf [loc]
  (let [loc (next loc)]
    (if (or (leaf? (node loc))
            (end? loc))
      loc
      (recur loc))))

(defn reset [loc]
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

(defn scan [loc pred]
  (if (end? loc)
    loc
    (let [node (.-node loc)
          rights (.-r loc)
          lefts (.-l loc)
          acc (.-acc loc)
          reducing-fn (.-reducing-fn (.-ops loc))
          next-loc (if (root? loc)
                     loc
                     (loop [l (transient lefts)
                            n node
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
  (let [reducing-fn (.-reducing-fn (.-ops loc))]
    (z-merge loc {:l (conj (.-l loc) item)
                  :changed? true
                  :acc (reducing-fn (.-acc loc)
                                    (.-metrics item))})))

(defn remove [loc]
  (let [node (.-node loc)
        make-node-fn (.-make-node (.-ops loc))]
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
      (if (or (stop? (loc-acc z1) (.-metrics (node z1)))
              (and (end? z1) (end? z2)))
        true
        (recur (next z1) (next z2)))
      false)))
