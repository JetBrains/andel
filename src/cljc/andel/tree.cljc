(ns andel.tree
  (:require [clojure.zip :as z]
            [andel.fast-zip :as fz])
  (:refer-clojure :exclude (replace remove next)))

(defrecord Node [metrics children])

(defrecord Leaf [metrics data])

(defn make-node [children {:keys [reducing-fn]}]
  (Node. (reduce (fn [acc x] (reducing-fn acc (:metrics x))) (reducing-fn) children)
         (into-array children)))

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

(def root-path (fz/->ZipperPath nil nil :root nil nil nil nil))

(defn zipper [tree {:keys [reducing-fn
                            metrics-fn
                            leaf-overflown?
                            split-thresh
                            split-leaf
                            leaf-underflown?
                            merge-leafs] :as config}]
  (fz/->ZipperLocation
   (fz/->ZipperOps node?
                   #(.-children %)
                   (fn [node children] (make-node children config))
                   reducing-fn
                   metrics-fn
                   leaf-overflown?
                   split-thresh
                   split-leaf
                   leaf-underflown?
                   merge-leafs)
   tree
   root-path))

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
  (node? (first c)))

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
  (let [path (.-path loc)
        acc (.-acc path)]
    (or acc
        ((.-reducing-fn (.-ops loc))))))

(defn merge-needed? [children config]
  (let [leaf-underflown? (.-leaf-underflown? config)
        split-thresh (.-split-thresh config)]
    (fast-some (if (nodes? children)
                 (let [merge-thresh (quot split-thresh 2)]
                   #(< (count (.-children %)) merge-thresh))
                 #(leaf-underflown? (.-data %)))
               children)))

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
        path (.-path loc)
        changed? (some-> path .-changed?)]
    (if changed?
      (let [config (.-ops loc)]
        (if-let [parent (fz/up loc)]
          (fz/replace parent (make-node (balance-children (fz/children parent) config) config))
          (fz/->ZipperLocation
           (.-ops loc)
           (shrink-tree (grow-tree [node] config))
           root-path)))
      (fz/up loc))))

(defn right [loc]
  (let [node (.-node loc)
        path (.-path loc)
        acc (.-acc path)]
    (when-let [r (fz/right loc)]
      (let [reducing-fn (.-reducing-fn (.-ops loc))
            acc' (reducing-fn (or acc (reducing-fn)) (.-metrics node))]
        (fz/update-path r #(fz/assoc-acc % acc'))
        #_(update r :path assoc :acc acc')))))

(defn down [loc]
  (let [path (.-path loc)
        acc (some-> path .-acc)]
    (some-> (fz/down loc)
            (fz/update-path #(fz/assoc-acc % acc)))))

(defn end? [loc]
  (keyword? (.-path loc)))

(defn root
  "Modified version of clojure.zip/root to work with balancing version of up"
  [loc]
  (if (end? loc)
    (fz/node loc)
    (let [p (up loc)]
      (if p
        (recur p)
        (fz/node loc)))))


(def node fz/node)
(defn branch? [loc]
  (node? (node loc)))

(defn next
  "Modified version of clojure.zip/next to work with balancing version of up"
  [loc]
  (let [path (.-path loc)]
    (if (end? loc)
      loc
      (or
       (and (branch? loc) (down loc))
       (right loc)
       (loop [p loc]
         (if-let [u (up p)]
           (or (right u) (recur u))
           (fz/->ZipperLocation (.-ops loc) (.-node p) :end)))))))

(defn skip
  "Just like next but not going down"
  [loc]
  (let [path (.-path loc)]
    (if (end? loc)
      loc
      (or
       (right loc)
       (loop [p loc]
         (if-let [u (up p)]
           (or (right u) (recur u))
           (fz/->ZipperLocation (.-ops loc) (.-node p) :end)))))))


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

(defn root? [loc]
  (= :root (.-ppath (.-path loc)))
  #_(nil? (.-path loc)))

(defn reset [loc]
  (zipper (root loc)
          (.-ops loc)))

(defn push! [a x]
  (.push a x)
  a)

(defn scan [loc pred]
  (if (end? loc)
    loc
    (let [node (.-node loc)
          path (.-path loc)
          rights (some-> path .-r)
          lefts (some-> path .-l)
          acc (some-> path .-acc)
          reducing-fn (.-reducing-fn (.-ops loc))
          next-loc (if (root? loc)
                     loc
                     (loop [l (transient lefts)
                            n node
                            r rights
                            acc (or acc (reducing-fn))]
                       (when (some? n)
                         (let [m (.-metrics n)
                               acc' (reducing-fn acc m)]
                           (if (pred acc m)
                             (fz/->ZipperLocation
                              (.-ops loc)
                              n
                              (fz/->ZipperPath (persistent! l)
                                               (seq r)
                                               (.-ppath path)
                                               (.-pnodes path)
                                               (.-changed? path)
                                               acc
                                               nil))
                             (recur (conj! l n)
                                    (first r)
                                    (rest r)
                                    acc'))))))]
      (if (some? next-loc)
        (if (branch? next-loc)
          (recur (down next-loc) pred)
          next-loc)
        (recur (skip loc) pred)))))



(comment

  (let [[n & r] (cons 1 nil)]
    n)

  )

(defn insert-left [loc x]
  (let [reducing-fn (.-reducing-fn (.-ops loc))]
    (-> loc
        (fz/insert-left x)
        (fz/update-path (fn [p] (fz/assoc-acc p (reducing-fn (.-acc p) (.-metrics x))))))))

(defn remove [loc]
  (let [node (.-node loc)
        path (.-path loc)
        [left] (some-> path .-l)
        [right] (some-> path .-r)]
    (if (some? right)
      (fz/->ZipperLocation (.-ops loc)
                           right
                           (-> path
                               (update :r (fn [r] (seq (drop 1 r))))
                               (assoc :changed? true)))
      (if (some? left)
        (next (fz/remove loc))
        (if (root? loc)
          (replace loc (make-node [] (.-ops loc)))
          (recur (fz/up loc)))))))
