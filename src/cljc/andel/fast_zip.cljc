;;   Copyright (c) Rich Hickey. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;;   functional hierarchical zipper, with navigation, editing and enumeration
;;   see Huet

(ns ^{:doc "Functional hierarchical zipper, with navigation, editing,
  and enumeration.  See Huet"
      :author "Rich Hickey, modified by Alexander K. Hudek"}
  andel.fast-zip
  (:refer-clojure :exclude [replace remove next]))

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

(defrecord ZipperPath [l r ppath pnodes changed? acc o-acc])

(defrecord ZipperLocation [^ZipperOps ops node ^ZipperPath path])

(defn update-path [loc f]
  (ZipperLocation. (.-ops loc)
                   (.-node loc)
                   (f (.-path loc))))

(defn assoc-acc [path acc]
  (ZipperPath. (.-l path)
               (.-r path)
               (.-ppath path)
               (.-pnodes path)
               (.-changed? path)
               acc
               (.-o-acc path)))

(defn assoc-o-acc [path o-acc]
  (ZipperPath. (.-l path)
               (.-r path)
               (.-ppath path)
               (.-pnodes path)
               (.-changed? path)
               (.-acc path)
               o-acc))

(defn node
  "Returns the node at loc"
  [^ZipperLocation loc]
  (.-node loc))

(defn acc
  "Returns the acc at loc"
  [^ZipperLocation loc]
  (.-acc (.-path loc)))

(defn branch?
  "Returns true if the node at loc is a branch"
  [^ZipperLocation loc]
  ((.-branch? ^ZipperOps (.-ops loc)) (.-node loc)))

(defn children
  "Returns a seq of the children of node at loc, which must be a branch"
  [^ZipperLocation loc]
  ((.-children ^ZipperOps (.-ops loc)) (.-node loc)))

(defn make-node
  "Returns a new branch node, given an existing node and new children.
  The loc is only used to supply the constructor."
  [^ZipperLocation loc node children]
  ((.-make-node ^ZipperOps (.-ops loc)) node children))

(defn path
  "Returns a seq of nodes leading to this loc"
  [^ZipperLocation loc]
  (if-let [^ZipperPath p (.-path loc)] (.-pnodes p)))

(defn lefts
  "Returns a seq of the left siblings of this loc"
  [^ZipperLocation loc]
  (if-let [^ZipperPath p (.-path loc)] (seq (reverse (.-l p)))))

(defn rights
  "Returns a seq of the right siblings of this loc"
  [^ZipperLocation loc]
  (if-let [^ZipperPath p (.-path loc)] (.-r p)))

(defn down
  "Returns the loc of the leftmost child of the node at this loc,
  or nil if no children"
  [^ZipperLocation loc]
  (when (branch? loc)
    (when-let [[c & cs] (children loc)]
      (let [node (.-node loc), ^ZipperPath path (.-path loc)]
        (ZipperLocation.
         (.-ops loc)
         c
         (ZipperPath.
          []
          cs
          path
          (if path (conj (.-pnodes path) node) [node])
          nil
          (some-> loc (.-path) (.-acc))
          nil))))))

(defn up
  "Returns the loc of the parent of the node at this loc, or nil if at the top"
  [^ZipperLocation loc]
  (let [^ZipperPath path (.-path loc)]
    (when-let [pnodes (and path (.-pnodes path))]
      (let [pnode (peek pnodes)]
        (if (:changed? path)
          (ZipperLocation.
           (.-ops loc)
           (make-node loc pnode (concat (.-l path) (cons (.-node loc) (.-r path))))
           (if-let [^ZipperPath ppath (.-ppath path)]
             (ZipperPath. (.-l ppath)
                          (.-r ppath)
                          (.-ppath ppath)
                          (.-pnodes ppath)
                          true
                          (.-acc ppath)
                          (.-o-acc ppath))))
          (ZipperLocation.
           (.-ops loc)
           pnode
           (.-ppath path)))
        ))))

(defn root
  "zips all the way up and returns the root node, reflecting any changes."
  [^ZipperLocation loc]
  (if (#?(:clj identical? :cljs =) :end (.-path loc))
    (.-node loc)
    (let [p (up loc)]
      (if p
        (recur p)
        (.-node loc)))))

(defn right
  "Returns the loc of the right sibling of the node at this loc, or nil"
  [^ZipperLocation loc]
  (let [^ZipperPath path (.-path loc)]
    (when-let [r (and path (.-r path))]
      (ZipperLocation.
       (.-ops loc)
       (first r)
       (ZipperPath.
        (conj (.-l path) (.-node loc))
        #?(:clj (.next ^clojure.lang.ISeq r) :cljs (cljs.core/next r))
        (.-ppath path)
        (.-pnodes path)
        (:changed? path)
        (some-> loc (.-path) (.-acc))
        nil)))))

(defn rightmost
  "Returns the loc of the rightmost sibling of the node at this loc, or self"
  [^ZipperLocation loc]
  (let [^ZipperPath path (.-path loc)]
    (if-let [r (and path (.-r path))]
      (ZipperLocation.
       (.-ops loc)
       (last r)
       (ZipperPath.
        (apply conj (.-l path) (.-node loc) (butlast r))
        nil
        (.-ppath path)
        (.-pnodes path)
        (:changed? path)
        (some-> loc (.-path) (.-acc))
        nil))
      loc)))

(defn insert-left
  "Inserts the item as the left sibling of the node at this loc, without moving"
  [^ZipperLocation loc item]
  (if-let [^ZipperPath path (.-path loc)]
    (ZipperLocation.
     (.-ops loc)
     (.-node loc)
     (ZipperPath.
      (conj (.-l path) item)
      (.-r path)
      (.-ppath path)
      (.-pnodes path)
      true
      (.-acc path)
      nil))
    (throw (new #?(:clj Exception :cljs js/Error) "Insert at top"))))

(defn insert-right
  "Inserts the item as the right sibling of the node at this loc, without moving"
  [^ZipperLocation loc item]
  (if-let [^ZipperPath path (.-path loc)]
    (ZipperLocation.
     (.-ops loc)
     (.-node loc)
     (ZipperPath. (.-l path)
                  (cons item (.-r path))
                  (.-ppath path)
                  (.-pnodes path)
                  true
                  (.-acc path)
                  nil))
    (throw (new #?(:clj Exception :cljs js/Error) "Insert at top"))))

(defn replace
  "Replaces the node at this loc, without moving"
  [^ZipperLocation loc node]
  (ZipperLocation.
   (.-ops loc)
   node
   (if-let [^ZipperPath path (.-path loc)]
     (assoc path :changed? true)
     #_(ZipperPath. (.-l path) (.-r path) (.-ppath path) (.-pnodes path) true (.-acc path))))
  )

(defn insert-child
  "Inserts the item as the leftmost child of the node at this loc, without moving"
  [^ZipperLocation loc item]
  (replace loc (make-node loc (.-node loc) (cons item (children loc)))))

(defn append-child
  "Inserts the item as the rightmost child of the node at this loc, without moving"
  [^ZipperLocation loc item]
  (replace loc (make-node loc (.-node loc) (concat (children loc) [item]))))

(defn end?
  "Returns true if loc represents the end of a depth-first walk"
  [^ZipperLocation loc]
  (#?(:clj identical? :cljs =) :end (.-path loc)))

(defn remove
  "Removes the node at loc, returning the loc that would have preceded it in a depth-first walk."
  [^ZipperLocation loc]
  (if-let [^ZipperPath path (.-path loc)]
    (if (pos? (count (.-l path)))
      (loop [loc (ZipperLocation.
                  (.-ops loc)
                  (peek (.-l path))
                  (ZipperPath. (pop (.-l path))
                               (.-r path)
                               (.-ppath path)
                               (.-pnodes path)
                               true
                               (.-acc path)
                               (.-o-acc path)))
             ]
        (if-let [child (and (branch? loc) (down loc))]
          (recur (rightmost child))
          loc))
      (ZipperLocation.
       (.-ops loc)
       (make-node loc (peek (.-pnodes path)) (.-r path))
       (if-let [^ZipperPath ppath (.-ppath path)]
         (if ppath (ZipperPath.
                    (.-l ppath)
                    (.-r ppath)
                    (.-ppath ppath)
                    (.-pnodes ppath)
                    true
                    (.-acc path)
                    (.-o-acc path)))))
      )
    (throw (new #?(:clj Exception :cljs js/Error) "Remove at top"))))

#_(defn remove
  "Removes the node at loc, returning the loc that would have preceded
  it in a depth-first walk."
  {:added "1.0"}
  [loc]
    (let [[node {l :l, ppath :ppath, pnodes :pnodes, rs :r, :as path}] loc]
      (if (nil? path)
        (throw (new Exception "Remove at top"))
        (if (pos? (count l))
          (loop [loc (with-meta [(peek l) (assoc path :l (pop l) :changed? true)] (meta loc))]
            (if-let [child (and (branch? loc) (down loc))]
              (recur (rightmost child))
              loc))
          (with-meta [(make-node loc (peek pnodes) rs)
                      (and ppath (assoc ppath :changed? true))]
                     (meta loc))))))

(defn edit
  "Replaces the node at this loc with the value of (f node args)"
  [^ZipperLocation loc f & args]
  (replace loc (apply f (.-node loc) args)))
