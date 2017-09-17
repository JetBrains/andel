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
  (:refer-clojure :exclude [replace remove next])
  #?(:cljs (:require-macros [andel.fast-zip :refer [zipper z-merge]])))

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

(defrecord ZipperLocation [^ZipperOps ops node l r changed? acc o-acc pzip end? root?])

#?(:clj
   (defmacro zipper [{:keys [ops node l r changed? acc o-acc pzip end? root?]}]
     `(->ZipperLocation ~ops ~node ~l ~r ~changed? ~acc ~o-acc ~pzip ~end? ~root?)))

#?(:clj
   (defmacro z-merge [loc props]
     (let [loc-sym (gensym "loc")]
       `(let [~loc-sym ~loc]
          (zipper ~{:ops (or (:ops props) `(.-ops ~loc-sym))
                    :node (or (:node props) `(.-node ~loc-sym))
                    :l (or (:l props) `(.-l ~loc-sym))
                    :r (or (:r props) `(.-r ~loc-sym))
                    :changed? (or (:changed? props) `(.-changed? ~loc-sym))
                    :acc (or (:acc props) `(.-acc ~loc-sym))
                    :o-acc (or (:o-acc props) `(.-o-acc ~loc-sym))
                    :pzip (or (:pzip props) `(.-pzip ~loc-sym))
                    :end? (or (:end? props) `(.-end? ~loc-sym))
                    :root? (or (:root? props) `(.-root? ~loc-sym))})))))

(defn assoc-acc [loc acc]
  (z-merge loc {:acc acc}))

(defn assoc-o-acc [loc o-acc]
  (z-merge loc {:o-acc o-acc}))

(defn mark-changed [loc]
  (z-merge loc {:changed? true}))

(defn node
  "Returns the node at loc"
  [^ZipperLocation loc]
  (.-node loc))

(defn acc
  "Returns the acc at loc"
  [^ZipperLocation loc]
  (.-acc loc))

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

(defn lefts [^ZipperLocation loc]
  (seq (.-l loc)))

(defn rights [^ZipperLocation loc]
  (.-r loc))

(defn down
  "Returns the loc of the leftmost child of the node at this loc, or nil if no children"
  [^ZipperLocation loc]
  (when (branch? loc)
    (when-let [[c & cs] (children loc)]
      (zipper
       {:ops (.-ops loc)
        :node c
        :l []
        :r cs
        :changed? nil
        :acc (.-acc loc)
        :o-acc nil
        :pzip loc}))))

(defn up
  "Returns the loc of the parent of the node at this loc, or nil if at the top"
  [^ZipperLocation loc]
  (when-let [pzip (.-pzip loc)]
    (if (.-changed? loc)
      (z-merge pzip
               {:node (make-node loc (.-node pzip) (into (.-l loc) (cons (.-node loc) (.-r loc))))
                :changed? true})
      pzip)))

(defn right
  "Returns the loc of the right sibling of the node at this loc, or nil"
  [^ZipperLocation loc]
  (when-let [r (.-r loc)]
    (z-merge loc {:node (first r)
                  :l (conj (.-l loc) (.-node loc))
                  :r (clojure.core/next r)
                  :acc nil
                  :o-acc nil})))

(defn rightmost
  "Returns the loc of the rightmost sibling of the node at this loc, or self"
  [^ZipperLocation loc]
  (if-let [r (.-r loc)]
    (zipper {:ops (.-ops loc)
             :node (last r)
             :l (apply conj (.-l loc) (.-node loc) (butlast r))
             :r nil
             :pzip (.-pzip loc)
             :changed? (.-changed? loc)
             :acc nil
             :o-acc nil})
    loc))

(defn insert-left
  "Inserts the item as the left sibling of the node at this loc, without moving"
  [^ZipperLocation loc item]
  (assert (some? (.-l loc)) "Insert at top")
  (z-merge loc {:l (conj (.-l loc) item)
                :changed? true}))

(defn insert-right
  "Inserts the item as the right sibling of the node at this loc, without moving"
  [^ZipperLocation loc item]
  (assert (some? (.-r loc)) "Insert at top")
  (z-merge loc {:r (cons item (.-r loc))
                :changed? true}))

(defn replace
  "Replaces the node at this loc, without moving"
  [^ZipperLocation loc node]
  (z-merge loc {:node node
                :changed? true}))

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
  (.-end? loc))

(defn root
  "zips all the way up and returns the root node, reflecting any changes."
  [^ZipperLocation loc]
  (if (end? loc)
    (.-node loc)
    (let [p (up loc)]
      (if (some? p)
        (recur p)
        (.-node loc)))))

(defn remove
  "Removes the node at loc, returning the loc that would have preceded it in a depth-first walk."
  [^ZipperLocation loc]
  (if-let [pzip (.-pzip loc)]
    (if (pos? (count (.-l loc)))
      (loop [loc (zipper {:ops (.-ops loc)
                          :node (peek (.-l loc))
                          :l (pop (.-l loc))
                          :r (.-r loc)
                          :pzip (.-pzip loc)
                          :changed? true
                          :acc (.-acc loc)
                          :o-acc (.-o-acc loc)})]
        (if-let [child (and (branch? loc) (down loc))]
          (recur (rightmost child))
          loc))
      (zipper {:ops (.-ops loc)
               :node (make-node loc (.-node (.-pzip loc)) (.-r loc))
               :l (.-l pzip)
               :r (.-r pzip)
               :pzip (.-pzip pzip)
               :changed? true
               :acc (.-acc pzip)
               :o-acc (.-o-acc pzip)}))
    (throw (new #?(:clj Exception :cljs js/Error) "Remove at top"))))

(defn edit
  "Replaces the node at this loc with the value of (f node args)"
  [^ZipperLocation loc f & args]
  (replace loc (apply f (.-node loc) args)))
