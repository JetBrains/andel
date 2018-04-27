(ns andel.intervals
  (:require [andel.tree :as tree]
            [clojure.data.int-map :as i]
            [andel.array-list :as al]
            [andel.utils])
  (:import [andel.tree Leaf Node ZipperLocation ZipperOps]))

(def plus-infinity
  #?(:cljs 1000000000.0 #_js/Number.POSITIVE_INFINITY
     :clj Integer/MAX_VALUE #_100000 #_Double/POSITIVE_INFINITY))


(defonce records
  (do
    (defrecord Attrs [^long id background foreground layer])

    (defrecord Data
        [^long offset
         ^long length
         ^long rightest

         marker-ids
         greedy-left?
         greedy-right?
         attrs])

    (defrecord Marker
        [^long from
         ^long to
         greedy-left?
         greedy-right?
         attrs])
    :defined))

(defn reducing-fn
  ([] nil)
  ([^Data left ^Data right]
   (cond
     (nil? left) right
     (nil? right) left
     :else
     (Data. (.-offset left)
            (max (.-length left) (+ (.-rightest left) (.-offset right) (.-length right)))
            (+ (.-rightest left) (.-offset right) (.-rightest right))
            nil
            nil nil nil))))

(defn location-from [^ZipperLocation loc]
  (let [^Data acc  (tree/loc-acc loc)
        node (tree/node loc)
        ^Data metrics (tree/metrics node)]
    (+ (or (some-> acc (.-rightest)) 0) (.-offset ^Data metrics))))

(defn location-to [^ZipperLocation loc]
  (+ (location-from loc)
     (.-length ^Data (tree/metrics (tree/node loc)))))


(def tree-config
  {:reducing-fn      reducing-fn
   :metrics-fn       identity
   :make-node (fn [children]
                (if (empty? children)
                  (tree/->Node (Data. 0 0 0 (i/int-set) nil nil nil) [])
                  (let [marker-ids (if (tree/node? (first children))
                                     (transduce (map (fn [c] (.-marker-ids ^Data (tree/metrics c)))) (completing i/union) (i/int-set) children)
                                     (into (i/int-set) (map (fn [^Leaf c] (.-id ^Attrs (.-attrs ^Data (.-data c))))) children))
                        ^Data data (reduce (fn [acc x] (reducing-fn acc (tree/metrics x))) (reducing-fn) children)]
                    (tree/->Node (Data. (.-offset data)
                                        (.-length data)
                                        (.-rightest data)
                                        marker-ids nil nil nil)
                                 children))))
   :leaf-overflown?  (constantly false)
   :split-thresh     32
   :leaf-underflown? (constantly false)})

(defn zipper [it]
  (tree/zipper it tree-config))

(defn root [loc] (tree/root loc))

(defn by-offset [offset]
  (fn [acc m]
    (let [^Data m (reducing-fn acc m)]
      (< offset (+ (.-offset m) (.-rightest m))))))

(defn offset->tree-basis [offset]
  (inc offset))

(defn tree-basis->offset [offset]
  (dec offset))

(defn loc->Marker [loc]
  (let [^Leaf node (tree/node loc)
        ^Data metrics (tree/metrics node)
        ^Data data (.-data node)
        length (.-length metrics)
        from (location-from loc)]
    (Marker. (tree-basis->offset from)
             (tree-basis->offset (+ from length))
             (.-greedy-left? data)
             (.-greedy-right? data)
             (.-attrs data))))

(defn loc->tree-marker
  "Same as loc->Marker but offsets are in tree basis"
  [loc]
  (let [^Leaf node (tree/node loc)
        ^Data metrics (.-metrics node)
        ^Data data (.-data node)
        length (.-length metrics)
        from (location-from loc)]
    (Marker. from
             (+ from length)
             (.-greedy-left? data)
             (.-greedy-right? data)
             (.-attrs data))))


(defn update-leaf [^ZipperLocation loc f]
  (assert (tree/leaf? (tree/node loc)) "update-leaf should recieve leaf")
  (let [metrics-fn (.-metrics-fn ^ZipperOps (.-ops loc))]
    (tree/edit loc
               (fn [^Leaf leaf]
                 (let [data' (f (.-data leaf))]
                   (tree/->Leaf (metrics-fn data') data'))))))

(defn update-leaf-offset [loc f]
  (update-leaf loc
               (fn [^Data data]
                 (Data. (f (.-offset data))
                        (.-length data)
                        (.-rightest data)
                        (.-marker-ids data)
                        (.-greedy-left? data)
                        (.-greedy-right? data)
                        (.-attrs data)))))

(defn update-leaf-length [loc f]
  (update-leaf loc
               (fn [^Data data]
                 (Data. (.-offset data)
                        (f (.-length data))
                        (.-rightest data)
                        (.-marker-ids data)
                        (.-greedy-left? data)
                        (.-greedy-right? data)
                        (.-attrs data)))))

(defn intersects? [from1 to1 from2 to2]
  (let [to-fst (if (< from1 from2) to1 to2)
        from-snd   (max from1 from2)
        len1       (- to1 from1)
        len2       (- to2 from2)]
    (if (or (= len1 0) (= len2 0))
      false
      (< from-snd to-fst))))

(defn intersects-inclusive? [from1 to1 from2 to2]
  (if (< from1 from2)
    (<= from2 to1)
    (<= from1 to2)))

(defn by-intersect [from to]
  (fn [^Data acc-metrics ^Data node-metrics]
    (let [rightest (or (some-> acc-metrics (.-rightest)) 0)
          offset   (.-offset node-metrics)
          length   (.-length node-metrics)
          loc-from     (+ offset rightest)]
      (intersects-inclusive? loc-from (+ loc-from length)
                             from to))))

(defn make-leaf [offset length greedy-left? greedy-right? attrs]
  (tree/make-leaf (Data. offset length 0 nil greedy-left? greedy-right? attrs) tree-config))

(defn make-interval-tree []
  (let [sentinels [(tree/make-leaf (Data. 0 0 0 nil false false (Attrs. -1 nil nil nil)) tree-config)
                   (tree/make-leaf (Data. plus-infinity 0 0 nil false false (Attrs. -2 nil nil nil)) tree-config)]]
    (tree/make-node sentinels tree-config)))

(defn insert-one
  ([loc from to greedy-left? greedy-right? attrs]
   (let [r-sibling-loc    (tree/scan loc (by-offset from))
         ^Data r-metrics  (-> r-sibling-loc tree/node (tree/metrics))
         r-offset         (.-offset r-metrics)
         r-from           (location-from r-sibling-loc)
         r-to             (location-to r-sibling-loc)
         len              (- to from)
         new-r-offset     (- r-from from)
         offset           (- r-offset new-r-offset)]
     (-> r-sibling-loc
         (tree/insert-left (make-leaf offset len greedy-left? greedy-right? attrs))
         (update-leaf-offset (constantly new-r-offset)))))
  ([loc ^Marker marker]
   (let [from             (.-from marker)
         to               (.-to marker)
         greedy-left?     (.-greedy-left? marker)
         greedy-right?    (.-greedy-right? marker)
         attrs            (.-attrs marker)]
     (insert-one loc from to greedy-left? greedy-right? attrs))))

(defn add-markers [itree markers]
  (root
    (reduce
      (fn [loc ^Marker m]
        (insert-one loc
                    (offset->tree-basis (.-from m))
                    (offset->tree-basis (.-to m))
                    (.-greedy-left? m)
                    (.-greedy-right? m)
                    (.-attrs m)))
      (tree/transient (zipper itree))
      markers)))

(defn remove-leaf [loc]
  (let [^Data data (.-data ^Leaf (tree/node loc))
        offset (.-offset data)
        length (.-length data)
        from   (location-from loc)
        to     (location-to loc)]
    (-> loc
        tree/remove
        ((fn [loc]
           (if-not (tree/leaf? (tree/node loc))
             (tree/next-leaf loc)
             loc)))
        (update-leaf-offset #(+ % offset)))))

;; tree -> offset -> size -> [tree acc]
(defn collect-with-remove [itree end-offset delta pred]
  (loop [loc (zipper itree)
         acc (transient [])]
    (let [new-loc (tree/scan loc pred)]
      (if (tree/end? new-loc)
        [(tree/root new-loc) (persistent! acc)]
        (let [from    (location-from new-loc)
              to      (location-to new-loc)]
          (if (< end-offset from)
            [(tree/root (update-leaf-offset new-loc #(+ % delta))) (persistent! acc)]
            (recur (remove-leaf new-loc) (conj! acc (loc->tree-marker new-loc)))))))))

(defn process-single-interval-typing [^Marker marker offset size]
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

(defn contains-offset-pred [offset]
  (fn [^Data acc-metrics ^Data node-metrics]
    (let [^Data metrics     (reducing-fn acc-metrics node-metrics)
          rightest    (or (some-> acc-metrics (.-rightest)) 0)
          node-offset (.-offset node-metrics)
          length      (.-length node-metrics)
          from        (+ node-offset rightest)
          to          (+ from length)]
      (or
       (<= from offset to)
       (< offset (+ (.-offset metrics) (.-rightest metrics)))))))

(defn type-in [itree offset size]
  (let [offset             (offset->tree-basis offset)
        [itree' intervals] (collect-with-remove itree offset size (contains-offset-pred offset))
        intervals'         (sort-by :from (map #(process-single-interval-typing % offset size) intervals))]
    (->> intervals'
         (reduce insert-one (zipper itree'))
         root)))

(defn intersects-pred [i-from i-to]
  (fn [^Data acc-metrics ^Data node-metrics]
    (let [^Data metrics     (reducing-fn acc-metrics node-metrics)
          rightest    (or (some-> acc-metrics (.-rightest)) 0)
          node-offset (.-offset node-metrics)
          length      (.-length node-metrics)
          from        (+ node-offset rightest)
          to          (+ from length)]
      (or
       (<= from i-from to)
       (<= i-from from i-to)
       (< i-to (+ (.-offset metrics) (.-rightest metrics)))))))

(defn process-single-interval-deletion [^Marker marker offset length]
  (let [from         (.-from marker)
        to           (.-to marker)
        g-l?         (.-greedy-left? marker)
        g-r?         (.-greedy-right? marker)
        update-point (fn [point offset length]
                       (if (< offset point)
                         (max offset (- point length))
                         point))
        from' (update-point from offset length)
        to' (update-point to offset length)]
    (when (or (< 0 (- to' from')) g-l? g-r?)
      (Marker. from'
               to'
               g-l?
               g-r?
               (.-attrs marker)))))

(defn delete-range [itree offset size]
  (let [offset             (offset->tree-basis offset)
        [itree' intervals] (collect-with-remove itree (+ offset size) (- size) (intersects-pred offset (+ offset size)))
        intervals'         (sort-by :from
                                    (into []
                                          (comp
                                           (map #(process-single-interval-deletion % offset size))
                                           (filter some?))
                                          intervals))]
    (->> intervals'
         (reduce insert-one (zipper itree'))
         root)))

(defn xquery-all [tree]
  (tree/reducible
      (fn [f init]
        (loop [loc (tree/next-leaf (zipper tree))
               acc init]
          (if (tree/end? loc)
            acc
            (let [start (location-from loc)]
              (if (or (= start 0) (= start plus-infinity)) ;; skip sentinels
                (recur (tree/next-leaf loc) acc)
                (recur (tree/next-leaf loc) (f acc (loc->Marker loc))))))))))

(defn tree->intervals [tree] (into [] (xquery-all tree)))

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
            (or (tree/end? loc) (< to (location-from loc)))
            s

            (tree/leaf? (tree/node loc))
            (recur (tree/scan (tree/next loc) stop?)
                   (if (intersects-inclusive? (location-from loc) (location-to loc)
                                              from to)
                     (f s (loc->Marker loc))
                     s))

            :else
            (recur (tree/scan loc stop?) s)))))))

(defn query-intervals [loc from to]
  (into [] (xquery-intervals loc from to)))


(defn- gc-leafs [^ZipperLocation loc bias deleted?]
  (let [children (tree/children loc)
        len (count children)
        ^ZipperOps ops (.-ops loc)
        make-node-fn (.-make-node ops)]
    (loop [bias bias
           i 0
           changed? false
           res (al/into-array-list [])]
      (if (< i len)
        (let [^Leaf n (al/get children i)
              ^Data data (.-data n)]
          (cond
            (deleted? (.-id ^Attrs (.-attrs data)))
            (recur (+ bias (-> data (.-offset)))
                   (inc i)
                   true
                   res)

            (< 0 bias)
            (recur 0
                   (inc i)
                   true
                   (al/conj! res (tree/make-leaf
                                  (Data. (+ (.-offset data) bias)
                                         (.-length data)
                                         (.-rightest data)
                                         (.-marker-ids data)
                                         (.-greedy-left? data)
                                         (.-greedy-right? data)
                                         (.-attrs data))
                                  ops)))

            :else
            (recur bias
                   (inc i)
                   changed?
                   (al/conj! res n))))
        (if (empty? res)
           [(tree/remove loc) bias]
           [(tree/skip (tree/replace loc (make-node-fn res))) bias])))))

(defn gc [itree deleted-markers]
  (loop [loc (zipper itree)
         bias 0]
      (cond
        (tree/end? loc)
        (tree/root loc)

        (tree/branch? loc)
        (let [marker-ids (-> loc (tree/node) ^Data (tree/metrics) (.-marker-ids))]
          (if (or (andel.utils/sets-intersect? marker-ids deleted-markers)
                  (< 0 bias))
            (recur (tree/next-leaf loc) bias)
            (recur (tree/skip loc) bias)))

        :else (let [[loc' bias] (gc-leafs (tree/up loc) bias deleted-markers)]
                (recur loc' (long bias))))))

(comment
  (def markers [(Marker. 0 1 false false (Attrs. 1 nil nil nil))
                (Marker. 1 3 false false (Attrs. 2 nil nil nil))
                (Marker. 2 4 false false (Attrs. 3 nil nil nil))
                (Marker. 2 3 false false (Attrs. 7 nil nil nil))
                (Marker. 3 5 false false (Attrs. 4 nil nil nil))
                (Marker. 4 6 false false (Attrs. 5 nil nil nil))
                (Marker. 5 7 false false (Attrs. 6 nil nil nil))
                (Marker. 5 7 false false (Attrs. 8 nil nil nil))
                (Marker. 5 7 false false (Attrs. 9 nil nil nil))
                (Marker. 5 7 false false (Attrs. 10 nil nil nil))
                (Marker. 5 7 false false (Attrs. 11 nil nil nil))
                (Marker. 5 7 false false (Attrs. 12 nil nil nil))
                (Marker. 5 7 false false (Attrs. 13 nil nil nil))])

  (def tt (-> (make-interval-tree)
              (add-markers markers)
              (gc (i/int-set [1 2 3 4 5 6 7 8 9 10 11 12 13]))))

  (def remains (query-intervals (zipper tt) 0 8))

  {:deleted #{1 4}
   :before markers
   :after remains}
  )

(defprotocol Lexer
  (lexemes [this from to])
  (update-text [this new-text offset added-len removed-len])
  (token-type [this offset]))

(defmulti create-lexer (constantly :idea))
(defmethod create-lexer :default [& args] nil)

(defonce empty-tree (make-interval-tree))
