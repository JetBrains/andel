(ns andel.tree
  (:require [clojure.zip :as z])
  (:require [andel.fast-zip :as fz])
  (:refer-clojure :exclude (replace remove next)))

(defrecord Node [metrics children])

(defrecord Leaf [metrics data])

(defn make-node [children {::keys [reducing-fn]}]
  (Node. (reduce (fn [acc x] (reducing-fn acc (:metrics x))) (reducing-fn) children)
         (into-array children)))

(defn make-leaf [data {::keys [metrics-fn]}]
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

(defn zipper [tree {::keys [reducing-fn
                            metrics-fn
                            leaf-overflown?
                            split-thresh
                            split-leaf
                            leaf-underflown?
                            merge-leafs] :as config}]
  ^{:zip/branch? node?
    :zip/children :children
    :zip/make-node (fn [node children] (make-node children config))
    ::reducing-fn reducing-fn
    ::metrics-fn metrics-fn
    ::leaf-overflown? leaf-overflown?
    ::split-thresh split-thresh
    ::split-leaf split-leaf
    ::leaf-underflown? leaf-underflown?
    ::merge-leafs merge-leafs}
  [tree nil])

(defn partition-binary [s thresh]
  (let [cs (count s)]
    (if (< cs thresh)
      [s]
      (let [[left right] (split-at (quot cs 2) s)]
        (concat (partition-binary left thresh)
                (partition-binary right thresh))))))

(defn split-node [node {::keys [split-thresh] :as config}]
  (assert (node? node))
  (let [children (:children node)]
    (if (< (count children) split-thresh)
      [node]
      (map #(make-node % config)
           (partition-binary children split-thresh)))))

(defn fast-some [pred coll]
  (reduce (fn [_ c] (if (pred c) (reduced true) false)) false coll))

(defn nodes? [c]
  (node? (first c)))

(defn split-needed? [children {::keys [leaf-overflown? split-thresh] :as config}]
  (fast-some (if (nodes? children)
               #(<= split-thresh (count (:children %)))
               #(leaf-overflown? (:data %)))
             children))

(defn split-children [children {::keys [leaf-overflown? split-leaf split-thresh] :as config}]
  (if (split-needed? children config)
    (persistent!
     (reduce
      (fn [result node]
        (cond (and (node? node) (<= split-thresh (count (:children node))))
              (reduce conj! result (map #(make-node % config)
                                        (partition-binary (:children node) split-thresh)))

              (and (leaf? node) (leaf-overflown? (:data node)))
              (reduce conj! result (map #(make-leaf % config) (split-leaf (:data node))))

              :else (conj! result node)))
      (transient [])
      children))
    children))

(defn loc-acc [[_ {::keys [acc]} :as loc]]
  (or acc
      ((::reducing-fn (meta loc)))))

(defn merge-needed? [children {::keys [leaf-underflown? split-thresh]}]
  (fast-some (if (nodes? children)
               (let [merge-thresh (quot split-thresh 2)]
                 #(< (count (:children %)) merge-thresh))
               #(leaf-underflown? (:data %)))
             children))

(defn merge-children [children {::keys [leaf-underflown? merge-leafs leaf-overflown? split-leaf split-thresh] :as config}]
  (if (merge-needed? children config)
    (if (nodes? children)
      (let [merge-thresh (quot split-thresh 2)
            [result last] (reduce
                           (fn [[result left] right]
                             (let [left-children (:children left)
                                   right-children (:children right)
                                   left-c (count left-children)
                                   right-c (count right-children)]
                               (if (or (< left-c merge-thresh) (< right-c merge-thresh))
                                 (if (<= split-thresh (+ left-c right-c))
                                   (let [[children-left children-right]
                                         (partition-binary (concat left-children right-children) split-thresh)]
                                     [(conj! result (make-node (merge-children children-left config) config))
                                      (make-node children-right config)])
                                   [result (make-node (merge-children (concat left-children right-children) config) config)])
                                 [(conj! result left) right])))
                           [(transient []) (first children)]
                           (drop 1 children))]
        (persistent! (conj! result last)))
      (let [[result last] (reduce
                             (fn [[result left-data] right]
                               (if (or (leaf-underflown? left-data) (leaf-underflown? (:data right)))
                                 (let [merged-data (merge-leafs left-data (:data right))]
                                   (if (leaf-overflown? merged-data)
                                     (let [[left-data right-data] (split-leaf merged-data)]
                                       [(conj! result (make-leaf left-data config))
                                        right-data])
                                     [result merged-data]))
                                 [(conj! result (make-leaf left-data config)) (:data right)]))
                             [(transient []) (:data (first children))]
                             (drop 1 children))]
        (persistent! (conj! result (make-leaf last config)))))
    children))

(defn balance-children [children config]
  (-> children
      (split-children config)
      (merge-children config)))

(defn grow-tree [children {::keys [split-thresh] :as config}]
  (let [balanced-children (balance-children children config)]
    (if (< (count balanced-children) split-thresh)
      (make-node balanced-children config)
      (recur [(make-node balanced-children config)] config))))


(defn shrink-tree [{[c :as children] :children :as node}]
  (if (and (node? c) (= 1 (count children)))
    (recur c)
    node))

(defn up [[node {:keys [changed?] :as path} :as loc]]
  (if changed?
    (let [config (meta loc)]
      (if-let [parent (z/up loc)]
        (z/replace parent (make-node (balance-children (z/children parent) config) config))
        (with-meta [(shrink-tree (grow-tree [node] config)) nil] (meta loc))))
    (z/up loc)))

(defn right [[node {::keys [acc]} :as loc]]
  (when-let [r (z/right loc)]
    (let [{::keys [reducing-fn]} (meta loc)
          acc' (reducing-fn (or acc (reducing-fn)) (:metrics node))]
      (update r 1 assoc ::acc acc'))))

(defn down [[_ {::keys [acc]} :as loc]]
  (some-> (z/down loc)
          (update 1 assoc ::acc acc)))

(defn root
  "Modified version of clojure.zip/root to work with balancing version of up"
  [loc]
  (if (= :end (loc 1))
    (z/node loc)
    (let [p (up loc)]
      (if p
        (recur p)
        (z/node loc)))))


(def node z/node)
(defn branch? [loc]
  (node? (node loc)))


(defn next
  "Modified version of clojure.zip/next to work with balancing version of up"
  [loc]
  (if (= :end (loc 1))
    loc
    (or
     (and (branch? loc) (down loc))
     (right loc)
     (loop [p loc]
       (if-let [u (up p)]
         (or (right u) (recur u))
         [(node p) :end])))))

(defn skip
  "Just like next but not going down"
  [loc]
  (if (= :end (loc 1))
    loc
    (or
     (right loc)
     (loop [p loc]
       (if-let [u (up p)]
         (or (right u) (recur u))
         [(node p) :end])))))


(def insert-right z/insert-right)
(def children z/children)
(defn end? [[_ p]] (keyword? p))
(def edit z/edit)
(def replace z/replace)
(def insert-child z/insert-child)

(defn next-leaf [loc]
  (let [loc (next loc)]
    (if (or (leaf? (node loc))
            (end? loc))
      loc
      (recur loc))))

(defn root? [[_ ctx]]
  (nil? ctx))

(defn reset [loc]
  (zipper (root loc)
          (meta loc)))

(defn scan [[node {rights :r lefts :l acc ::acc :as path} :as loc] pred]
  (if (end? loc)
    loc
    (let [{::keys [reducing-fn]} (meta loc)
          next-loc (if (root? loc)
                     loc
                     (loop [l (transient lefts)
                            [n & r] (cons node rights)
                            acc (or acc (reducing-fn))]
                       (when (some? n)
                         (let [m (:metrics n)
                               acc' (reducing-fn acc m)]
                           (if (pred acc m)
                             (with-meta [n (-> path
                                               (transient)
                                               (assoc! :l (persistent! l))
                                               (assoc! :r (seq r))
                                               (assoc! ::acc acc)
                                               (persistent!))] (meta loc))
                             (recur (conj! l n) r acc'))))))]

      (if (some? next-loc)
        (if (branch? next-loc)
          (recur (down next-loc) pred)
          next-loc)
        (recur (skip loc) pred)))))

(defn insert-left [loc x]
  (let [{::keys [reducing-fn]} (meta loc)]
    (-> loc
        (z/insert-left x)
        (update-in [1 ::acc] reducing-fn (:metrics x)))))

(defn remove [[node {[left] :l [right] :r :as path} :as loc]]
  (if (some? right)
    (with-meta
      [right (-> path
                 (update :r (fn [r] (seq (drop 1 r))))
                 (assoc :changed? true))]
      (meta loc))
    (if (some? left)
      (next (z/remove loc))
      (if (root? loc)
        (replace loc (make-node [] (meta loc)))
        (recur (z/up loc))))))
