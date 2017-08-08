(ns andel.intervals
  (:require [andel.tree :as tree]))


(def plus-infinity #?(:cljs js/Number.POSITIVE_INFINITY
                      :clj Integer/MAX_VALUE #_100000 #_Double/POSITIVE_INFINITY))

(defrecord Interval [offset length rightest])

(defn reducing-fn
  ([] nil)
  ([{l-offset :offset l-rightest :rightest l-length :length :as left}
    {r-offset :offset r-rightest :rightest r-length :length :as right}]
     (cond (nil? left)
           right
           (nil? right)
           left
           :else
           (map->Interval {:offset l-offset
                           :rightest (+ l-rightest r-offset r-rightest)
                           ;; left border of rightest interval in subtree relative to offset
                           :length (max l-length (+ l-rightest r-offset r-length))}))))


(def tree-config {::tree/reducing-fn reducing-fn
                  ::tree/metrics-fn identity
                  ::tree/leaf-overflown? (constantly false)
                  ::tree/split-thresh 4
                  ::tree/leaf-underflown? (constantly false)})

(defn zipper [it]
  (tree/zipper it tree-config))

(defn root [loc] (tree/root loc))

(defn by-offset [offset]
  (fn [acc m]
    (let [m (reducing-fn acc m)]
      (< offset (+ (:offset m) (:rightest m))))))

(defn from-to [loc]
  (let [m (:metrics (tree/node loc))
        rightest (or (:rightest (tree/loc-acc loc)) 0)
        from (+ (:offset m) rightest)
        length (:length m)]
    {:from from
     :to (+ from length)}))

(defn update-leaf [loc f]
  (assert (tree/leaf? (tree/node loc)) "update-leaf should recieve list")
  (tree/edit loc
             (fn [{:keys [data] :as leaf}]
               (let [fixed-interval (f data)]
                 (assoc leaf
                        :metrics fixed-interval
                        :data fixed-interval)))))

(defn update-leaf-offset [loc f]
  (update-leaf loc (fn [data] (update data :offset f))))

(defn update-leaf-length [loc f]
  (update-leaf loc (fn [data] (update data :length f))))

(defn tree->intervals [tr]
  (loop [loc (zipper tr)
         acc []]
    (cond (tree/end? loc)
          (->> acc
               (drop 1)       ;; drop left sentinel
               (drop-last 1)  ;; drop right sentinel
               (vec))
          
          (tree/leaf? (tree/node loc))
          (recur (tree/next loc) (conj acc (from-to loc)))
          
          :else
          (recur (tree/next loc) acc))))

(defn intersect [a b]
  (let [[fst snd] (if (< (:from a) (:from b)) [a b] [b a])
        fst-len (- (:to fst) (:from fst))
        snd-len (- (:to snd) (:from snd))]
    (if (or (= fst-len 0) (= snd-len 0))
      false
      (< (:from snd) (:to fst)))))

(defn scan-intersect [loc interval]
  (tree/scan loc
             (fn [acc-metrics node-metrics]
               (let [metrics (reducing-fn acc-metrics node-metrics)
                     rightest (or (:rightest acc-metrics) 0)
                     offset (:offset node-metrics)
                     length (:length node-metrics)
                     from (+ offset rightest)]
                 (intersect {:from from
                             :to (+ from length)}
                            interval)))))

(defn make-leaf [offset length]
  (tree/make-leaf (map->Interval {:offset offset
                                  :length length
                                  :rightest 0})
                  tree-config))

(defn intervals->tree [intervals]
  (-> (map #(tree/make-leaf % tree-config) intervals)
      (tree/make-node tree-config)
      (zipper)
      (assoc-in [1 :changed?] true)
      (root)))

(defn make-interval-tree []
  (intervals->tree [(map->Interval {:offset   0
                                    :length   0
                                    :rightest 0})
                    (map->Interval {:offset   plus-infinity
                                    :length   0
                                    :rightest 0})]))

(defn insert-one [loc {:keys [from to] :as interval}]
  (let [r-sibling-loc (tree/scan loc (by-offset from))
        r-offset (-> r-sibling-loc tree/node :metrics :offset)
        {r-from :from r-to :to} (from-to r-sibling-loc)
        len (- to from)
        new-r-offset (- r-from from)
        offset (- r-offset new-r-offset)]
    (-> r-sibling-loc
        (tree/insert-left (make-leaf offset len))
        (update-leaf-offset (constantly new-r-offset)))))

(defn add-intervals [itree intervals]
  (root (reduce insert-one (zipper itree) intervals)))

(defn next-changed [loc offset]
  (tree/scan loc
             (fn [acc-metrics node-metrics]
               (let [metrics (reducing-fn acc-metrics node-metrics)
                     rightest (or (:rightest acc-metrics) 0)
                     node-offset (:offset node-metrics)
                     length (:length node-metrics)
                     from (+ node-offset rightest)
                     to (+ from length)]
                 (or (and (<= from offset)
                          (<= offset to))
                  
                     (< offset (+ (:offset metrics) (:rightest metrics))))))))

(defn unpack-leaf [loc]
  (assert (tree/leaf? loc))
  (:data (tree/node loc)))

(defn remove-leaf [loc]
  (let [{:keys [offset length]} (unpack-leaf loc)
        {:keys [from to]} (from-to loc)]
    (-> loc
        tree/remove
        ((fn [loc]
           (if-not (tree/leaf? (tree/node loc))
             (tree/next-leaf loc)
             loc)))
        (update-leaf-offset #(+ % offset)))))

;; tree -> offset -> size -> [tree acc]
(defn collect-with-remove [itree offset size]
  (loop [loc (zipper itree)
         acc []]
    (let [new-loc (next-changed loc offset)]
      (if (tree/end? new-loc)
        [(tree/root new-loc) acc]
        (let [{:keys [from to] :as from-to} (from-to new-loc)]
          (if (< offset from)
            [(tree/root (update-leaf-offset new-loc #(+ % size))) acc]
            (recur (remove-leaf new-loc) (conj acc from-to))))))))

(defn process-interval [{:keys [from to] :as interval} offset size]
  (cond
    (and (<= from offset)
         (<= offset to))
    (assoc interval :to (+ to size))

    (< offset from)
    (assoc interval
           :to (+ to size)
           :from (+ from size))

    :else
    interval))

(defn type-in [itree offset size]
  (let [[itree' intervals] (collect-with-remove itree offset size)
        intervals' (sort-by :offset (map #(process-interval % offset size) intervals))]
    (add-intervals itree' intervals')))

(comment

  (-> (make-interval-tree)
      (add-intervals [{:from 4 :to 10} {:from 6 :to 15} {:from 9 :to 12}])
      (type-in 4 3)
      (tree->intervals)
      #_(collect-with-remove 5 3)
      #_((fn [[tree coll]] [(tree->intervals tree) coll])))

  )

(+ 2 2)
  
  

#_(defn type-in [itree offset size]
  (let [start-loc (scan-intersect (zipper itree) {:from offset :to (inc offset)})]
    (loop [loc start-loc]
      (let [{loc-from :from loc-to :to :as loc-from-to} (from-to loc)]
        (cond
          (tree/node? (tree/node loc))
          (recur (tree/next loc))

          (< offset loc-from)
          (root (update-offset loc #(+ size %)))
          
          :else
          (recur (tree/next
                  (if (< offset loc-to)
                    (update-length loc #(+ size %))
                    loc))))))))

(defn query-intervals
  ([itree from to]
   (query-intervals itree {:from from :to to}))
  ([itree {:keys [from to] :as interval}]
   (loop [loc (zipper itree)
          markers []]
     (cond 
       (tree/end? loc)
       markers

       (tree/leaf? (tree/node loc))
       (recur (scan-intersect (tree/next loc) interval)
              (conj markers (from-to loc)))

       :else
       (recur (scan-intersect loc interval)
              markers)))))
