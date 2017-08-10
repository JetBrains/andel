(ns andel.intervals
  (:require [andel.tree :as tree]))


(def plus-infinity #?(:cljs 1000000000.0 #_js/Number.POSITIVE_INFINITY
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
           (Interval. l-offset ;; offset
                      (max l-length (+ l-rightest r-offset r-length)) ;; length
                      (+ l-rightest r-offset r-rightest) ;;rightest
                      ;; left border of rightest interval in subtree relative to offset
                      nil
                      nil))))


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

(defn offset->tree-basis [offset]
  (inc offset))

(defn interval->tree-basis [interval]
  (-> interval 
      (update :from offset->tree-basis)
      (update :to offset->tree-basis)))

(defn tree-basis->offset [offset]
  (dec offset))

(defn tree-basis->interval [interval]
  (-> interval
      (update :from tree-basis->offset)
      (update :to tree-basis->offset)))

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
               (map tree-basis->interval)
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

(defn intersect-inclusive [a b]
  (let [[fst snd] (if (< (:from a) (:from b)) [a b] [b a])]
    (<= (:from snd) (:to fst))))

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
  (tree/make-leaf (Interval. offset ;;offset
                             length ;;length
                             0      ;;rightest
                             greedy-left? ;; greedy-left?
                             greedy-right?  ;; greedy-right?
                             )
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
  (->> intervals
       (map interval->tree-basis)
       (reduce insert-one (zipper itree))
       root))

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
  (let [offset (offset->tree-basis offset)
        [itree' intervals] (collect-with-remove itree offset size)
        intervals' (sort-by :from (map #(process-interval % offset size) intervals))]
    (->> intervals'
         (reduce insert-one (zipper itree'))
         root)))


(defn collect-with-remove-changed [itree offset size]
  (let [changed? (fn [acc-metrics node-metrics]
                   (let [metrics (reducing-fn acc-metrics node-metrics)
                         rightest (or (:rightest acc-metrics) 0)
                         node-offset (:offset node-metrics)
                         length (:length node-metrics)
                         from (+ node-offset rightest)
                         to (+ from length)]
                     (or (intersect-inclusive {:from from :to to} {:from offset :to (+ offset size)})
                         
                         (< (+ offset size) (+ (:offset metrics) (:rightest metrics))))))]
    (loop [loc (zipper itree)
           acc []]
      (let [new-loc (tree/scan loc changed?)]
        (if (tree/end? new-loc)
          [(tree/root new-loc) acc]
          (let [{:keys [from to] :as from-to} (from-to new-loc)]
            (if (< (+ offset size) from)
              [(tree/root (update-leaf-offset new-loc #(- % size))) acc]
              (recur (remove-leaf new-loc) (conj acc from-to)))))))))

(defn process-single-interval-deletion [interval offset length]
  (let [update-point (fn [point offset length] (if (< offset point)
                                                 (max offset (- point length))
                                                 point))]
    (-> interval
        (update :from update-point offset length)
        (update :to update-point offset length))))

(defn delete-range [itree [offset size]]
  (let [offset (offset->tree-basis offset)
        [itree' intervals] (collect-with-remove-changed itree offset size)
        intervals' (sort-by :from (map #(process-single-interval-deletion % offset size) intervals))]
    (->> intervals'
         (reduce insert-one (zipper itree'))
         root)))

(defn query-intervals
  ([itree from to]
   (query-intervals itree {:from from :to to}))
  ([itree interval]
   (let [{:keys [from to] :as interval} (interval->tree-basis interval)]
     (loop [loc (zipper itree)
            markers []]
       (cond 
         (tree/end? loc)
         (map tree-basis->interval markers)

         (tree/leaf? (tree/node loc))
         (recur (scan-intersect (tree/next loc) interval)
                (conj markers (from-to loc)))

         :else
         (recur (scan-intersect loc interval)
                markers))))))
