(ns andel.intervals
  (:require [andel.tree :as tree]))


(def plus-infinity #?(:cljs js/Number.POSITIVE_INFINITY
                      :clj Integer/MAX_VALUE #_100000 #_Double/POSITIVE_INFINITY))

(defrecord Interval [offset length rightest greedy-left? greedy-right?])

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
     :to (+ from length)
     :greedy-left? (:greedy-left? (:data (tree/node loc)))
     :greedy-right? (:greedy-right? (:data (tree/node loc)))}))

(defn update-leaf [loc f]
  (assert (tree/leaf? (tree/node loc)) "update-leaf should recieve leaf")
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


(defn make-leaf [offset length greedy-left? greedy-right?]
  (tree/make-leaf (map->Interval {:offset offset
                                  :length length
                                  :rightest 0
                                  :greedy-left? greedy-left?
                                  :greedy-right? greedy-right?})
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
                                    :rightest 0
                                    :greedy-left? false
                                    :greedy-right? false})
                    (map->Interval {:offset   plus-infinity
                                    :length   0
                                    :rightest 0
                                    :greedy-left? false
                                    :greedy-right? false})]))

(defn insert-one [loc {:keys [from to greedy-left? greedy-right?] :as interval}]
  (let [r-sibling-loc (tree/scan loc (by-offset from))
        r-offset (-> r-sibling-loc tree/node :metrics :offset)
        {r-from :from r-to :to} (from-to r-sibling-loc)
        len (- to from)
        new-r-offset (- r-from from)
        offset (- r-offset new-r-offset)]
    (-> r-sibling-loc
        (tree/insert-left (make-leaf offset len greedy-left? greedy-right?))
        (update-leaf-offset (constantly new-r-offset)))))

(defn add-intervals [itree intervals]
  (root (reduce insert-one (zipper itree) intervals)))

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

(defn process-interval [{:keys [from to greedy-left? greedy-right?] :as interval} offset size]
  (cond
    (and greedy-left?
         (= offset from))
    (assoc interval :to (+ to size))
    
    (and greedy-right?
         (= offset to))
    (assoc interval :to (+ to size))
    
    (and (< from offset)
         (< offset to))
    (assoc interval :to (+ to size))
    
    (<= offset from)
    (assoc interval
           :to (+ to size)
           :from (+ from size))
    
    :else
    interval))

(defn type-in [itree [offset size]]  
  (let [[itree' intervals] (collect-with-remove itree offset size)
        intervals' (sort-by :from (map #(process-interval % offset size) intervals))]
    (add-intervals itree' intervals')))

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

;;    query-and-loc :: loc -> {:from, :to} -> [[marker], loc]
(defn query-and-loc [loc {:keys [from to] :as interval}]
  (loop [loc loc
         markers []
         next-loc nil]
    (cond
      (or (tree/end? loc))
      [markers next-loc]

      (tree/leaf? (tree/node loc))
      (let [{loc-to :to :as marker} (from-to loc)]
        (recur (scan-intersect (tree/next loc) interval)
               (conj markers marker)
               (or next-loc (if (<= to loc-to) loc))))

      :else
      (recur (scan-intersect loc interval)
             markers
             (or next-loc loc)))))
