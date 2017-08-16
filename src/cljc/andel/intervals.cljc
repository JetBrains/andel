(ns andel.intervals
  (:require [andel.tree :as tree]))


(def plus-infinity #?(:cljs 1000000000.0 #_js/Number.POSITIVE_INFINITY
                      :clj Integer/MAX_VALUE #_100000 #_Double/POSITIVE_INFINITY))

(defrecord IntervalNode [offset length rightest])

(defrecord IntervalLeaf [offset length rightest greedy-left? greedy-right?])
(defrecord Marker [from to greedy-left? greedy-right?])

(defn reducing-fn
  ([] nil)
  ([left right]
   (cond (nil? left)
         right
         (nil? right)
         left
         :else
         (let [l-offset (.-offset left)
               l-length (.-length left)
               l-rightest (.-rightest left)
               r-offset (.-offset right)
               r-rightest (.-rightest right)
               r-length (.-length right)]
           (IntervalNode. l-offset ;; offset
                          (max l-length (+ l-rightest r-offset r-length)) ;; length
                          (+ l-rightest r-offset r-rightest) ;;rightest
                          )))))

(defn metrics-fn [{:keys [offset length rightest]}]
  (->IntervalNode offset length rightest))

(def tree-config {::tree/reducing-fn reducing-fn
                  ::tree/metrics-fn metrics-fn
                  ::tree/leaf-overflown? (constantly false)
                  ::tree/split-thresh 16
                  ::tree/leaf-underflown? (constantly false)})

(defn zipper [it]
  (tree/zipper it tree-config))

(defn root [loc] (tree/root loc))

(defn by-offset [offset]
  (fn [acc m]
    (let [m (reducing-fn acc m)]
      (< offset (+ (:offset m) (:rightest m))))))

(defn loc->metrics [loc]
  (:metrics (tree/node loc)))

(defn loc->data [loc]
  (assert (tree/leaf? loc))
  (:data (tree/node loc)))

(defn loc->Marker [loc]
  (let [metrics (loc->metrics loc)
        leaf-data (loc->data loc)
        rightest (or (some-> (tree/loc-acc loc) (.-rightest)) 0)
        from (+ (.-offset metrics) rightest)
        length (.-length metrics)]
    (Marker.  from
              (+ from length)
              (.-greedy-left? leaf-data)
              (.-greedy-right? leaf-data))))

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
          (recur (tree/next loc) (conj acc (loc->Marker loc)))

          :else
          (recur (tree/next loc) acc))))

(defn intersect [a b]
  (let [[fst snd] (if (< (.-from a) (.-from b)) [a b] [b a])
        fst-len (- (.-to fst) (.-from fst))
        snd-len (- (.-to snd) (.-from snd))]
    (if (or (identical? fst-len 0) (identical? snd-len 0))
      false
      (< (.-from snd) (.-to fst)))))

(defn intersect-inclusive [a b]
  (let [[fst snd] (if (< (:from a) (:from b)) [a b] [b a])]
    (<= (:from snd) (:to fst))))

(defn scan-intersect [loc interval]
  (tree/scan loc
             (fn [acc-metrics node-metrics]
               (let [metrics (reducing-fn acc-metrics node-metrics)
                     rightest (or (some-> acc-metrics (.-rightest)) 0)
                     offset (.-offset node-metrics)
                     length (.-length node-metrics)
                     from (+ offset rightest)]
                 (intersect (Marker. from (+ from length) nil nil)
                            interval)))))


(defn make-leaf [offset length greedy-left? greedy-right?]
  (tree/make-leaf (->IntervalLeaf offset length 0 greedy-left? greedy-right?)
                  tree-config))

(defn intervals->tree [intervals]
  (-> (map #(tree/make-leaf % tree-config) intervals)
      (tree/make-node tree-config)
      (zipper)
      (assoc-in [1 :changed?] true)
      (root)))

(defn make-interval-tree []
  (intervals->tree [(->IntervalLeaf 0 0 0 false false) ;; left sentinel
                    (->IntervalLeaf plus-infinity 0 0 false false)] ;; right sentinel
                   ))

(defn insert-one [loc {:keys [from to greedy-left? greedy-right?] :as interval}]
  (let [r-sibling-loc (tree/scan loc (by-offset from))
        r-offset (-> r-sibling-loc tree/node :metrics :offset)
        {r-from :from r-to :to} (loc->Marker r-sibling-loc)
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

(defn remove-leaf [loc]
  (let [{:keys [offset length]} (loc->data loc)
        {:keys [from to]} (loc->Marker loc)]
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
        (let [{:keys [from to] :as from-to} (loc->Marker new-loc)]
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
          (let [{:keys [from to] :as from-to} (loc->Marker new-loc)]
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
   (let [interval (map->Marker (interval->tree-basis interval))
         from (.-from interval)
         to (.-to interval)]
     (loop [loc (zipper itree)
            markers (transient [])]
       (cond
         (tree/end? loc)
         (persistent! markers)

         (tree/leaf? (tree/node loc))
         (recur (scan-intersect (tree/next loc) interval)
                (conj! markers (tree-basis->interval (loc->Marker loc))))

         :else
         (recur (scan-intersect loc interval)
                markers))))))
