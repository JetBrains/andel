(ns andel.intervals
  (:require [andel.tree :as tree]))


(def plus-infinity #?(:cljs 1000000000.0 #_js/Number.POSITIVE_INFINITY
                      :clj Integer/MAX_VALUE #_100000 #_Double/POSITIVE_INFINITY))

(defrecord IntervalNode [offset length rightest])
;; todo: g-l g-r bg fg -> marker-attrs
(defrecord IntervalLeaf [offset length rightest greedy-left? greedy-right? background foreground])
(defrecord Marker [from to greedy-left? greedy-right? background foreground])

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
           (IntervalNode. l-offset
                          (max l-length (+ l-rightest r-offset r-length))
                          (+ l-rightest r-offset r-rightest))))))

(defn metrics-fn [leaf]
  (let [offset (.-offset leaf)
        length (.-length leaf)
        rightest (.-rightest leaf)]
    (IntervalNode. offset length rightest)))

(def tree-config {:reducing-fn reducing-fn
                  :metrics-fn metrics-fn
                  :leaf-overflown? (constantly false)
                  :split-thresh 16
                  :leaf-underflown? (constantly false)})

(defn zipper [it]
  (tree/zipper it tree-config))

(defn root [loc] (tree/root loc))

(defn mark-changed [loc]
  (update loc :path assoc :changed? true))

(defn by-offset [offset]
  (fn [acc m]
    (let [m (reducing-fn acc m)]
      (< offset (+ (.-offset m) (.-rightest m))))))

(defn loc->metrics [loc]
  (.-metrics (tree/node loc)))

(defn loc->data [loc]
  (assert (tree/leaf? loc))
  (.-data (tree/node loc)))

(defn loc->Marker [loc]
  (let [metrics (loc->metrics loc)
        leaf-data (loc->data loc)
        rightest (or (some-> (tree/loc-acc loc) (.-rightest)) 0)
        from (+ (.-offset metrics) rightest)
        length (.-length metrics)]
    (Marker. from
             (+ from length)
             (.-greedy-left? leaf-data)
             (.-greedy-right? leaf-data)
             (.-background leaf-data)
             (.-foreground leaf-data))))

(defn offset->tree-basis [offset]
  (inc offset))

(defn interval->tree-basis [interval]
  (let [from (.-from interval)
        to (.-to interval)
        g-l? (.-greedy-left? interval)
        g-r? (.-greedy-right? interval)
        bg (.-background interval)
        fg (.-foreground interval)]
    (Marker. (offset->tree-basis from) (offset->tree-basis to) g-l? g-r? bg fg)))

(defn tree-basis->offset [offset]
  (dec offset))

(defn tree-basis->interval [interval]
  (let [from (.-from interval)
        to (.-to interval)
        g-l? (.-greedy-left? interval)
        g-r? (.-greedy-right? interval)
        bg (.-background interval)
        fg (.-foreground interval)]
    (Marker. (tree-basis->offset from) (tree-basis->offset to) g-l? g-r? bg fg)))

(defn update-leaf [loc f]
  (assert (tree/leaf? (tree/node loc)) "update-leaf should recieve leaf")
  (tree/edit loc
             (fn [leaf]
               (let [data (.-data leaf)
                     fixed-interval (f data)]
                 (tree/->Leaf fixed-interval fixed-interval)))))

(defn update-leaf-offset [loc f]
  (update-leaf loc (fn [data] (let [offset (.-offset data)
                                    length (.-length data)
                                    rightest (.-rightest data)
                                    g-l? (.-greedy-left? data)
                                    g-r? (.-greedy-right? data)
                                    bg (.-background data)
                                    fg (.-foreground data)]
                                (IntervalLeaf. (f offset) length rightest g-l? g-r? bg fg)))))

(defn update-leaf-length [loc f]
  (update-leaf loc (fn [data] (let [offset (.-offset data)
                                    length (.-length data)
                                    rightest (.-rightest data)
                                    g-l? (.-greedy-left? data)
                                    g-r? (.-greedy-right? data)
                                    bg (.-background data)
                                    fg (.-foreground data)]
                                (IntervalLeaf. offset (f length) rightest g-l? g-r? bg fg)))))

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
  (let [[fst snd] (if (< (.-from a) (.-from b)) [a b] [b a])]
    (<= (.-from snd) (.-to fst))))

(defn scan-intersect [loc interval]
  (tree/scan loc
             (fn [acc-metrics node-metrics]
               (let [metrics (reducing-fn acc-metrics node-metrics)
                     rightest (or (some-> acc-metrics (.-rightest)) 0)
                     offset (.-offset node-metrics)
                     length (.-length node-metrics)
                     from (+ offset rightest)]
                 (intersect (Marker. from (+ from length) nil nil nil nil)
                            interval)))))


(defn make-leaf [offset length greedy-left? greedy-right? background foreground]
  (tree/make-leaf (IntervalLeaf. offset length 0 greedy-left? greedy-right? background foreground)
                  tree-config))

(defn intervals->tree [intervals]
  (-> (map #(tree/make-leaf % tree-config) intervals)
      (tree/make-node tree-config)
      (zipper)
      (mark-changed)
      (root)))

(defn make-interval-tree []
  (intervals->tree [(IntervalLeaf. 0 0 0 false false nil nil) ;; left sentinel
                    (IntervalLeaf. plus-infinity 0 0 false false nil nil)] ;; right sentinel
                   ))

(defn insert-one [loc interval]
  (let [from (.-from interval)
        to (.-to interval)
        greedy-left? (.-greedy-left? interval)
        greedy-right? (.-greedy-right? interval)
        bg (.-background interval)
        fg (.-foreground interval)
        r-sibling-loc (tree/scan loc (by-offset from))
        r-offset (-> r-sibling-loc tree/node .-metrics .-offset)
        r-sibling-marker (loc->Marker r-sibling-loc)
        r-from (.-from r-sibling-marker)
        r-to (.-to r-sibling-marker)
        len (- to from)
        new-r-offset (- r-from from)
        offset (- r-offset new-r-offset)]
    (-> r-sibling-loc
        (tree/insert-left (make-leaf offset len greedy-left? greedy-right? bg fg))
        (update-leaf-offset (constantly new-r-offset)))))

(defn add-intervals [itree intervals]
  (->> intervals
       (map interval->tree-basis)
       (reduce insert-one (zipper itree))
       root))

(defn remove-leaf [loc]
  (let [data (loc->data loc)
        offset (.-offset data)
        length (.-length data)
        marker (loc->Marker loc)
        from (.-from marker)
        to (.-to marker)]
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
                     rightest (or (some->  acc-metrics .-rightest) 0)
                     node-offset (.-offset node-metrics)
                     length (.-length node-metrics)
                     from (+ node-offset rightest)
                     to (+ from length)]
                 (or (and (<= from offset)
                          (<= offset to))

                     (< offset (+ (.-offset metrics) (.-rightest metrics))))))))

;; tree -> offset -> size -> [tree acc]
(defn collect-with-remove [itree offset size]
  (loop [loc (zipper itree)
         acc (transient [])]
    (let [new-loc (next-changed loc offset)]
      (if (tree/end? new-loc)
        [(tree/root new-loc) acc]
        (let [from-to (loc->Marker new-loc)
              from (.-from from-to)
              to (.-to from-to)]
          (if (< offset from)
            [(tree/root (update-leaf-offset new-loc #(+ % size))) (persistent! acc)]
            (recur (remove-leaf new-loc) (conj! acc from-to))))))))

(defn process-interval [interval offset size]
  (let [from (.-from interval)
        to (.-to interval)
        greedy-left? (.-greedy-left? interval)
        greedy-right? (.-greedy-right? interval)]
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
      interval)))

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
                         rightest (or (some-> acc-metrics .-rightest) 0)
                         node-offset (.-offset node-metrics)
                         length (.-length node-metrics)
                         from (+ node-offset rightest)
                         to (+ from length)]
                     (or (intersect-inclusive (Marker. from to nil nil nil nil) (Marker. offset (+ offset size) nil nil nil nil))
                         (< (+ offset size) (+ (.-offset metrics) (.-rightest metrics))))))]
    (loop [loc (zipper itree)
           acc (transient [])]
      (let [new-loc (tree/scan loc changed?)]
        (if (tree/end? new-loc)
          [(tree/root new-loc) acc]
          (let [from-to (loc->Marker new-loc)
                from (.-from from-to)
                to (.-to from-to)]
            (if (< (+ offset size) from)
              [(tree/root (update-leaf-offset new-loc #(- % size))) (persistent! acc)]
              (recur (remove-leaf new-loc) (conj! acc from-to)))))))))

(defn process-single-interval-deletion [interval offset length]
  (let [from (.-from interval)
        to (.-to interval)
        g-l? (.-greedy-left? interval)
        g-r? (.-greedy-right? interval)
        bg (.-background interval)
        fg (.-foreground interval)
        update-point (fn [point offset length] (if (< offset point)
                                                 (max offset (- point length))
                                                 point))]
    (Marker. (update-point from offset length)
             (update-point to offset length)
             g-l?
             g-r?
             bg
             fg)))

(defn delete-range [itree [offset size]]
  (let [offset (offset->tree-basis offset)
        [itree' intervals] (collect-with-remove-changed itree offset size)
        intervals' (sort-by :from (map #(process-single-interval-deletion % offset size) intervals))]
    (->> intervals'
         (reduce insert-one (zipper itree'))
         root)))

(defn query-intervals [itree interval]
  (let [interval (interval->tree-basis (map->Marker interval))
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
               markers)))))
