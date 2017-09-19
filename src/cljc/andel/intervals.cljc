(ns andel.intervals
  (:require [andel.tree :as tree]))

(def plus-infinity
  #?(:cljs 1000000000.0 #_js/Number.POSITIVE_INFINITY
     :clj Integer/MAX_VALUE #_100000 #_Double/POSITIVE_INFINITY))

(defrecord Attrs [background foreground layer])

(defrecord Data
  [offset
   length
   rightest
   greedy-left?
   greedy-right?
   attrs])

(defrecord Marker
  [from
   to
   greedy-left?
   greedy-right?
   attrs])

(defn reducing-fn
  ([] nil)
  ([left right]
   (cond (nil? left)
     right
     (nil? right)
     left
     :else
     (let [l-offset   (.-offset left)
           l-length   (.-length left)
           l-rightest (.-rightest left)
           r-offset   (.-offset right)
           r-rightest (.-rightest right)
           r-length   (.-length right)]
       (Data. l-offset
              (max l-length (+ l-rightest r-offset r-length))
              (+ l-rightest r-offset r-rightest)
              nil nil nil)))))

(defn marker-from [loc]
  (assert (tree/leaf? loc))
  (let [acc  (tree/loc-acc loc)
        node (tree/node loc)
        acc' (reducing-fn acc (.-metrics node))]
    (+ (.-offset acc') (.-rightest acc'))))

(defn marker-to [loc]
  (+ (marker-from loc)
     (-> (tree/node loc) (.-data) (.-length))))

(def tree-config
  {:reducing-fn      reducing-fn
   :metrics-fn       identity
   :leaf-overflown?  (constantly false)
   :split-thresh     32
   :leaf-underflown? (constantly false)})

(defn zipper [it]
  (tree/zipper it tree-config))

(defn root [loc] (tree/root loc))

(defn by-offset [offset]
  (fn [acc m]
    (let [m (reducing-fn acc m)]
      (< offset (+ (.-offset m) (.-rightest m))))))

(defn offset->tree-basis [offset]
  (inc offset))

(defn tree-basis->offset [offset]
  (dec offset))

(defn loc->Marker [loc]
  (let [node     (tree/node loc)
        metrics  (.-metrics node)
        data     (.-data node)
        length   (.-length metrics)
        from     (marker-from loc)]
    (Marker. (tree-basis->offset from)
             (tree-basis->offset (+ from length))
             (.-greedy-left? data)
             (.-greedy-right? data)
             (.-attrs data))))

(defn loc->tree-marker
  "Same as loc->Marker but offsets are in tree basis"
  [loc]
  (let [node     (tree/node loc)
        metrics  (.-metrics node)
        data     (.-data node)
        length   (.-length metrics)
        from     (marker-from loc)]
    (Marker. from
             (+ from length)
             (.-greedy-left? data)
             (.-greedy-right? data)
             (.-attrs data))))


(defn update-leaf [loc f]
  (assert (tree/leaf? (tree/node loc)) "update-leaf should recieve leaf")
  (let [metrics-fn (.-metrics-fn (.-ops loc))]
    (tree/edit loc
               (fn [leaf]
                 (let [data' (f (.-data leaf))]
                   (tree/->Leaf (metrics-fn data') data'))))))

(defn update-leaf-offset [loc f]
  (update-leaf loc
               (fn [data]
                 (let [offset   (.-offset data)
                       length   (.-length data)
                       rightest (.-rightest data)
                       g-l?     (.-greedy-left? data)
                       g-r?     (.-greedy-right? data)
                       attrs    (.-attrs data)]
                   (Data. (f offset) length rightest g-l? g-r? attrs)))))

(defn update-leaf-length [loc f]
  (update-leaf loc
               (fn [data]
                 (Data. (.-offset data)
                          (f (.-length data))
                          (.-rightest data)
                          (.-greedy-left? data)
                          (.-greedy-right? data)
                          (.-attrs data)))))

(defn tree->intervals [tr]
  (loop [loc (zipper tr)
         acc []]
    (cond (tree/end? loc)
      (->> acc
           (drop 1) ;; drop left sentinel
           (drop-last 1) ;; drop right sentinel
           (vec))

      (tree/leaf? (tree/node loc))
      (recur (tree/next loc) (conj acc (loc->Marker loc)))

      :else
      (recur (tree/next loc) acc))))

(defn intersects? [from1 to1 from2 to2]
  (let [to-fst (if (< from1 from2) to1 to2)
        from-snd   (max from1 from2)
        len1       (- to1 from1)
        len2       (- to2 from2)]
    (if (or (identical? len1 0) (identical? len2 0))
      false
      (< from-snd to-fst))))

(defn intersects-inclusive? [from1 to1 from2 to2]
  (let [to-fst (if (< from1 from2) to1 to2)
        from-snd   (max from1 from2)
        len1       (- to1 from1)
        len2       (- to2 from2)]
    (<= from-snd to-fst)))

(defn by-intersect [from to]
  (fn [acc-metrics node-metrics]
    (let [rightest (or (some-> acc-metrics (.-rightest)) 0)
          offset   (.-offset node-metrics)
          length   (.-length node-metrics)
          loc-from     (+ offset rightest)]
      (intersects-inclusive? loc-from (+ loc-from length)
                             from to))))

(defn make-leaf [offset length greedy-left? greedy-right? attrs]
  (tree/make-leaf (Data. offset length 0 greedy-left? greedy-right? attrs) tree-config))

(defn make-interval-tree []
  (let [sentinels [(tree/make-leaf (Data. 0 0 0 false false nil) tree-config)
                   (tree/make-leaf (Data. plus-infinity 0 0 false false nil) tree-config)]]
    (tree/make-node sentinels tree-config)))

(defn insert-one
  ([loc from to greedy-left? greedy-right? attrs]
   (let [r-sibling-loc    (tree/scan loc (by-offset from))
         r-offset         (-> r-sibling-loc tree/node (.-metrics) (.-offset))
         r-from           (marker-from r-sibling-loc)
         r-to             (marker-to r-sibling-loc)
         len              (- to from)
         new-r-offset     (- r-from from)
         offset           (- r-offset new-r-offset)]
     (-> r-sibling-loc
         (tree/insert-left (make-leaf offset len greedy-left? greedy-right? attrs))
         (update-leaf-offset (constantly new-r-offset)))))
  ([loc marker]
   (let [from             (.-from marker)
         to               (.-to marker)
         greedy-left?     (.-greedy-left? marker)
         greedy-right?    (.-greedy-right? marker)
         attrs            (.-attrs marker)]
     (insert-one loc from to greedy-left? greedy-right? attrs))))

(defn add-markers [itree markers]
  (root
   (reduce
    (fn [loc m]
      (insert-one loc
                  (offset->tree-basis (.-from m))
                  (offset->tree-basis (.-to m))
                  (.-greedy-left? m)
                  (.-greedy-right? m)
                  (.-attrs m)))
    (zipper itree)
    markers)))

(defn remove-leaf [loc]
  (let [data   (.-data (tree/node loc))
        offset (.-offset data)
        length (.-length data)
        from   (marker-from loc)
        to     (marker-to loc)]
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
               (let [metrics     (reducing-fn acc-metrics node-metrics)
                     rightest    (or (some-> acc-metrics (.-rightest)) 0)
                     node-offset (.-offset node-metrics)
                     length      (.-length node-metrics)
                     from        (+ node-offset rightest)
                     to          (+ from length)]
                 (or
                  (and (<= from offset)
                       (<= offset to))

                  (< offset (+ (.-offset metrics) (.-rightest metrics))))))))

;; tree -> offset -> size -> [tree acc]
(defn collect-with-remove [itree offset size]
  (loop [loc (zipper itree)
         acc (transient [])]
    (let [new-loc (next-changed loc offset)]
      (if (tree/end? new-loc)
        [(tree/root new-loc) acc]
        (let [from    (marker-from new-loc)
              to      (marker-to new-loc)]
          (if (< offset from)
            [(tree/root (update-leaf-offset new-loc #(+ % size))) (persistent! acc)]
            (recur (remove-leaf new-loc) (conj! acc (loc->tree-marker new-loc)))))))))

(defn process-interval [marker offset size]
  (let [from          (.-from marker)
        to            (.-to marker)
        greedy-left?  (.-greedy-left? marker)
        greedy-right? (.-greedy-right? marker)
        [from to] (cond
                    (and greedy-left?
                         (= offset from))
                    [from (+ to size)]

                    (and greedy-right?
                         (= offset to))
                    [from (+ to size)]

                    (and (< from offset)
                         (< offset to))
                    [from (+ to size)]

                    (<= offset from)
                    [(+ from size) (+ to size)]

                    :else
                    [from to])]
    (Marker. from
             to
             greedy-left?
             greedy-right?
             (.-attrs marker))))

(defn type-in [itree [offset size]]
  (let [offset             (offset->tree-basis offset)
        [itree' intervals] (collect-with-remove itree offset size)
        intervals'         (sort-by :from (map #(process-interval % offset size) intervals))]
    (->> intervals'
         (reduce insert-one (zipper itree'))
         root)))

(defn collect-with-remove-changed [itree offset size]
  (let [changed? (fn [acc-metrics node-metrics]
                   (let [metrics     (reducing-fn acc-metrics node-metrics)
                         rightest    (or (some-> acc-metrics .-rightest) 0)
                         node-offset (.-offset node-metrics)
                         length      (.-length node-metrics)
                         from        (+ node-offset rightest)
                         to          (+ from length)]
                     (or
                      (intersects-inclusive? from to
                                             offset (+ offset size))
                      (< (+ offset size) (+ (.-offset metrics) (.-rightest metrics))))))]
    (loop [loc (zipper itree)
           acc (transient [])]
      (let [new-loc (tree/scan loc changed?)]
        (if (tree/end? new-loc)
          [(tree/root new-loc) acc]
          (let [from    (marker-from new-loc)
                to      (marker-to new-loc)]
            (if (< (+ offset size) from)
              [(tree/root (update-leaf-offset new-loc #(- % size))) (persistent! acc)]
              (recur (remove-leaf new-loc) (conj! acc (loc->tree-marker new-loc))))))))))

(defn process-single-interval-deletion [marker offset length]
  (let [from         (.-from marker)
        to           (.-to marker)
        g-l?         (.-greedy-left? marker)
        g-r?         (.-greedy-right? marker)
        update-point (fn [point offset length]
                       (if (< offset point)
                         (max offset (- point length))
                         point))]
    (Marker. (update-point from offset length)
             (update-point to offset length)
             g-l?
             g-r?
             (.-attrs marker))))

(defn delete-range [itree [offset size]]
  (let [offset             (offset->tree-basis offset)
        [itree' intervals] (collect-with-remove-changed itree offset size)
        intervals'         (sort-by :from (map #(process-single-interval-deletion % offset size) intervals))]
    (->> intervals'
         (reduce insert-one (zipper itree'))
         root)))

(defn xquery-intervals [loc from to]
  (let [from           (offset->tree-basis from)
        to             (offset->tree-basis to)
        my-intersects? (by-intersect from to)
        overscans?     (by-offset to)
        stop?          (fn [acc metrics]
                         (or (my-intersects? acc metrics)
                             (overscans? acc metrics)))]
    (tree/reducible
     (fn [f init]
       (loop [loc loc
              s   init]
         (cond
           (or (tree/end? loc) (< to (marker-from loc)))
           s

           (tree/leaf? (tree/node loc))
           (recur (tree/scan (tree/next loc) stop?)
             (f s (loc->Marker loc)))

           (intersects? (marker-from loc) (marker-to loc)
                        from to)
           (recur loc (f s (loc->Marker loc)))

           :else
           (recur (tree/scan loc stop?) s)))))))

(defn query-intervals [loc from to]
  (into [] (xquery-intervals loc from to)))
