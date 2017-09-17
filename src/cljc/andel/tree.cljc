(ns andel.tree
  (:require [clojure.zip :as z]
            [andel.fast-zip :as fz])
  (:refer-clojure :exclude (replace remove next)))

(defrecord Node [metrics children])

(defrecord Leaf [metrics data])

#?(:clj
    (do
      (defn array [& args] (object-array args))
      (def some-array (array 1 2 3))
      (defn array? [x]
        (= (type some-array) (type x)))))

(defn make-node [children {:keys [reducing-fn]}]
  (let [children (if (array? children) children (into-array children))]
    (Node. (reduce (fn [acc x] (reducing-fn acc (:metrics x))) (reducing-fn) children)
           children)))

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
  (fz/zipper {:ops (fz/->ZipperOps node?
                                   #(.-children %)
                                   (fn [node children] (make-node children config))
                                   reducing-fn
                                   metrics-fn
                                   leaf-overflown?
                                   split-thresh
                                   split-leaf
                                   leaf-underflown?
                                   merge-leafs)
              :node tree
              :root? true}))

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
        split-thresh (.-split-thresh config)]
    (if (split-needed? children config)
      (persistent!
       (reduce
        (fn [result node]
          (cond (and (node? node) (<= split-thresh (count (.-children node))))
                (reduce conj! result (map #(make-node % config)
                                          (partition-binary (.-children node) split-thresh)))

                (and (leaf? node) (leaf-overflown? (.-data node)))
                (reduce conj! result (map #(make-leaf % config) (split-leaf (.-data node))))

                :else (conj! result node)))
        (transient [])
        children))
      children)))

(defn loc-acc [loc]
  (or (.-acc loc)
      ((.-reducing-fn (.-ops loc)))))

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
        split-thresh (.-split-thresh config)]
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
                                       [(conj! result (make-node (merge-children children-left config) config))
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
        balanced-children (balance-children children config)]
    (if (< (count balanced-children) split-thresh)
      (make-node balanced-children config)
      (recur [(make-node balanced-children config)] config))))


(defn shrink-tree [node]
  (let [children (.-children node)
        [c] children]
    (if (and (node? c) (= 1 (count children)))
      (recur c)
      node)))

(defn up [loc]
  (let [node (.-node loc)
        changed? (.-changed? loc)]
    (if changed?
      (let [config (.-ops loc)]
        (if-let [parent (fz/up loc)]
          (fz/replace parent (make-node (balance-children (fz/children parent) config) config))
          (fz/zipper {:ops (.-ops loc)
                      :node (shrink-tree (grow-tree [node] config))
                      :root? true})))
      (fz/up loc))))

(defn right [loc]
  (when-let [r (fz/right loc)]
    (let [node (.-node loc)
          reducing-fn (.-reducing-fn (.-ops loc))
          right-acc (reducing-fn (or (.-acc loc) (reducing-fn))
                            (.-metrics node))]
      (fz/assoc-acc r right-acc))))

(defn down [loc]
  (let [acc (.-acc loc)]
    (some-> (fz/down loc)
            (fz/assoc-acc acc))))

(def end? fz/end?)

(defn root
  "Modified version of clojure.zip/root to work with balancing version of up"
  [loc]
  (if (end? loc)
    (fz/node loc)
    (let [p (up loc)]
      (if (some? p)
        (recur p)
        (fz/node loc)))))


(def node fz/node)
(defn branch? [loc]
  (node? (node loc)))

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
         (fz/zipper {:ops (.-ops loc)
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
         (fz/zipper {:ops (.-ops loc)
                     :node (.-node p)
                     :end? true}))))))

(def insert-right fz/insert-right)
(def children fz/children)
(def edit fz/edit)
(def replace fz/replace)
(def insert-child fz/insert-child)

(defn next-leaf [loc]
  (let [loc (next loc)]
    (if (or (leaf? (node loc))
            (end? loc))
      loc
      (recur loc))))

(defn reset [loc]
  (zipper (root loc)
          (.-ops loc)))

(defn push! [a x]
  (.push a x)
  a)

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
                             (fz/zipper {:ops (.-ops loc)
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
        (recur (skip loc) pred)))))

(defn insert-left [loc x]
  (let [reducing-fn (.-reducing-fn (.-ops loc))
        loc' (fz/insert-left loc x)]
    (fz/assoc-acc loc' (reducing-fn (.-acc loc') (.-metrics x)))))

(defn remove [loc]
  (let [node (.-node loc)
        [left] (.-l loc)
        [right] (.-r loc)]
    (if (some? right)
      (fz/zipper {:ops (.-ops loc)
                  :node right
                  :l (.-l loc)
                  :r (seq (drop 1 (.-r loc)))
                  :pzip (.-pzip loc)
                  :changed? true
                  :acc (.-acc loc)
                  :o-acc (.-o-acc loc)})
      (if (some? left)
        (next (fz/remove loc))
        (if (root? loc)
          (replace loc (make-node [] (.-ops loc)))
          (recur (fz/up loc)))))))

(defn compare-zippers [z1 z2 stop?]
  (loop [z1 z1
         z2 z2]
    (if (identical? (node z1) (node z2))
      (if (or (stop? (loc-acc z1) (.-metrics (node z1)))
              (and (end? z1) (end? z2)))
        true
        (recur (next z1) (next z2)))
      false)))
