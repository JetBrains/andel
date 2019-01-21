(ns andel.intervals
  (:require [andel.tree :as tree]
            [clojure.data.int-map :as i]
            [andel.array-list :as al]
            [andel.utils])
  (:import [andel.tree Leaf Node ZipperLocation ZipperOps]))

(def plus-infinity
  #?(:cljs 1000000000.0 #_js/Number.POSITIVE_INFINITY
     :clj Integer/MAX_VALUE #_100000 #_Double/POSITIVE_INFINITY))

(defrecord Data
  [^long offset
   ^long length
   ^long rightest
   ^long id
   marker-ids
   ^boolean greedy-left?
   ^boolean greedy-right?
   attrs])

(defmacro >Data [& {:keys [offset length rightest marker-ids greedy-left? greedy-right? attrs id]}]
  `(Data. ~offset ~length ~rightest ~id ~marker-ids ~greedy-left? ~greedy-right? ~attrs))

(defrecord Marker
  [^long id
   ^long from
   ^long to
   ^boolean greedy-left?
   ^boolean greedy-right?
   attrs])

(defmacro >Marker [& {:keys [from to greedy-left? greedy-right? attrs id]}]
  `(Marker. ~id
            ~from
            ~to
            ~greedy-left?
            ~greedy-right?
            ~attrs))

(defn reducing-fn
  ([] nil)
  ([^Data left ^Data right]
   (cond
     (nil? left) right
     (nil? right) left
     :else
     (>Data :offset (.-offset left)
            :length (max (.-length left) (+ (.-rightest left) (.-offset right) (.-length right)))
            :rightest (+ (.-rightest left) (.-offset right) (.-rightest right))
            :marker-ids nil
            :greedy-left? false
            :greedy-right? false
            :attrs nil
            :id -5))))

(defn location-from ^long [^ZipperLocation loc]
  (let [^Data acc  (tree/loc-acc loc)
        node (tree/node loc)
        ^Data metrics (tree/metrics node)]
    (if acc
      (+ (.-offset metrics) (.-rightest acc))
      (.-offset metrics))))

(defn location-to ^long [^ZipperLocation loc]
  (+ (location-from loc)
     (.-length ^Data (tree/metrics (tree/node loc)))))

(def tree-config
  {:reducing-fn reducing-fn
   :metrics-fn identity
   :make-node (fn [children]
                (if (empty? children)
                  (tree/->Node (>Data :offset 0
                                      :length 0
                                      :rightest 0
                                      :marker-ids nil
                                      :greedy-left? false
                                      :greedy-right? false
                                      :attrs nil
                                      :id -1) [])
                  (let [marker-ids (if (tree/node? (first children))
                                     (transduce (map (fn [c] (.-marker-ids ^Data (tree/metrics c))))
                                                (completing i/union)
                                                (i/int-set)
                                                children)
                                     (into (i/int-set)
                                           (map (fn [^Leaf c] (.-id ^Data (.-data c))))
                                           children))
                        ^Data data (reduce (fn [acc x] (reducing-fn acc (tree/metrics x))) (reducing-fn) children)]
                    (tree/->Node (>Data :offset (.-offset data)
                                        :length (.-length data)
                                        :rightest (.-rightest data)
                                        :marker-ids marker-ids
                                        :greedy-left? false
                                        :greedy-right? false
                                        :attrs nil
                                        :id -1)
                                 children))))
   :leaf-overflown? (constantly false)
   :split-thresh 32
   :leaf-underflown? (constantly false)})

(defn zipper [it]
  (tree/zipper it tree-config))

(defn root [loc] (tree/root loc))

(defn by-offset [^long offset]
  (fn [acc m]
    (let [^Data m (reducing-fn acc m)]
      (< offset (+ (.-offset m) (.-rightest m))))))

(defn offset->tree-basis ^long [^long offset]
  (inc offset))

(defn tree-basis->offset [^long offset]
  (dec offset))

(defn loc->Marker ^Marker [loc]
  (let [^Leaf node (tree/node loc)
        ^Data metrics (tree/metrics node)
        ^Data data (.-data node)
        length (.-length metrics)
        from (location-from loc)]
    (>Marker :id (.-id data)
             :from (tree-basis->offset from)
             :to (tree-basis->offset (+ from length))
             :greedy-left? (.-greedy-left? data)
             :greedy-right? (.-greedy-right? data)
             :attrs (.-attrs data))))

(defn loc->tree-marker
  "Same as loc->Marker but offsets are in tree basis"
  [loc]
  (let [^Leaf node (tree/node loc)
        ^Data metrics (.-metrics node)
        ^Data data (.-data node)
        length (.-length metrics)
        from (location-from loc)]
    (>Marker :id (.-id data)
             :from from
             :to (+ from length)
             :greedy-left? (.-greedy-left? data)
             :greedy-right? (.-greedy-right? data)
             :attrs (.-attrs data))))

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
                 (>Data :offset (f (.-offset data))
                        :length (.-length data)
                        :rightest (.-rightest data)
                        :marker-ids (.-marker-ids data)
                        :greedy-left? (.-greedy-left? data)
                        :greedy-right? (.-greedy-right? data)
                        :attrs (.-attrs data)
                        :id (.-id data)))))

(defn update-leaf-length [loc f]
  (update-leaf loc
               (fn [^Data data]
                 (>Data :offset (.-offset data)
                        :length (f (.-length data))
                        :rightest (.-rightest data)
                        :marker-ids (.-marker-ids data)
                        :greedy-left? (.-greedy-left? data)
                        :greedy-right? (.-greedy-right? data)
                        :attrs (.-attrs data)
                        :id (.-id data)))))

(defn intersects? [^long from1 ^long to1 ^long from2 ^long to2]
  (let [to-fst (if (< from1 from2) to1 to2)
        from-snd   (max from1 from2)
        len1       (- to1 from1)
        len2       (- to2 from2)]
    (if (or (= len1 0) (= len2 0))
      false
      (< from-snd to-fst))))

(defn intersects-inclusive? [^long from1 ^long to1 ^long from2 ^long to2]
  (if (< from1 from2)
    (<= from2 to1)
    (<= from1 to2)))

(defn by-intersect [^long from ^long to]
  (fn [^Data acc-metrics ^Data node-metrics]
    (let [rightest (if acc-metrics (.-rightest acc-metrics) 0)
          offset   (.-offset node-metrics)
          length   (.-length node-metrics)
          loc-from (+ offset rightest)
          loc-to   (+ loc-from length)]
      (intersects-inclusive? loc-from loc-to from to))))

(defn insert-one
  ([loc id from to greedy-left? greedy-right? attrs]
   (let [from (long from)
         to (long to)
         r-sibling-loc    (tree/scan loc (by-offset from))
         ^Data r-metrics  (-> r-sibling-loc tree/node (tree/metrics))
         r-offset         (.-offset r-metrics)
         r-from           (location-from r-sibling-loc)
         r-to             (location-to r-sibling-loc)
         len              (- to from)
         new-r-offset     (- r-from from)
         offset           (- r-offset new-r-offset)]
     (-> r-sibling-loc
         (tree/insert-left (tree/make-leaf
                            (>Data :id id
                                   :offset offset
                                   :length len
                                   :rightest 0
                                   :marker-ids nil
                                   :greedy-left? greedy-left?
                                   :greedy-right? greedy-right?
                                   :attrs attrs) tree-config))
         (update-leaf-offset (constantly new-r-offset)))))
  ([loc ^Marker marker]
   (let [from             (.-from marker)
         to               (.-to marker)
         greedy-left?     (.-greedy-left? marker)
         greedy-right?    (.-greedy-right? marker)
         attrs            (.-attrs marker)]
     (insert-one loc (.-id marker) from to greedy-left? greedy-right? attrs))))

(defn add-markers [itree markers]
  (root
    (reduce
      (fn [loc ^Marker m]
        (insert-one loc
                    (.-id m)
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
        (update-leaf-offset (fn [^long x] (+ x offset))))))

;; tree -> offset -> size -> [tree acc]
(defn collect-with-remove [itree ^long end-offset ^long delta pred]
  (loop [loc (zipper itree)
         acc (transient [])]
    (let [new-loc (tree/scan loc pred)]
      (if (tree/end? new-loc)
        [(tree/root new-loc) (persistent! acc)]
        (let [from    (location-from new-loc)
              to      (location-to new-loc)]
          (if (< end-offset from)
            [(tree/root (update-leaf-offset new-loc (fn [^long x] (+ x delta)))) (persistent! acc)]
            (recur (remove-leaf new-loc) (conj! acc (loc->tree-marker new-loc)))))))))

(defn update-marker-with-typing [^Marker marker ^long offset ^long size]
  (let [from          (.-from marker)
        to            (.-to marker)
        greedy-left?  (.-greedy-left? marker)
        greedy-right? (.-greedy-right? marker)
        new-range (cond
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

                    :else nil)]
    (if-let [[from to] new-range]
      (>Marker :id (.-id marker)
               :from from
               :to to
               :greedy-left? greedy-left?
               :greedy-right? greedy-right?
               :attrs (.-attrs marker))
      marker)))

(defn contains-offset-pred [^long offset]
  (fn [^Data acc-metrics ^Data node-metrics]
    (let [^Data metrics (reducing-fn acc-metrics node-metrics)
          rightest    (if acc-metrics (.-rightest acc-metrics) 0)
          node-offset (.-offset node-metrics)
          length      (.-length node-metrics)
          from        (+ node-offset rightest)
          to          (+ from length)]
      (or
       (and (<= from offset) (<= offset to))
       (< offset (+ (.-offset metrics) (.-rightest metrics)))))))

(defn type-in [itree offset size]
  (let [offset             (offset->tree-basis offset)
        [itree' intervals] (collect-with-remove itree offset size (contains-offset-pred offset))
        intervals'         (sort-by #(.-from ^Marker %) (map #(update-marker-with-typing % offset size) intervals))]
    (root (reduce insert-one (zipper itree') intervals'))))

(defn intersects-pred [^long i-from ^long i-to]
  (fn [^Data acc-metrics ^Data node-metrics]
    (let [^Data metrics     (reducing-fn acc-metrics node-metrics)
          rightest    (if acc-metrics (.-rightest acc-metrics) 0)
          node-offset (.-offset node-metrics)
          length      (.-length node-metrics)
          from        (+ node-offset rightest)
          to          (+ from length)]
      (or
       (<= from i-from to)
       (<= i-from from i-to)
       (< i-to (+ (.-offset metrics) (.-rightest metrics)))))))

(defn- shift-offset ^long [^long offset ^long edit-offset ^long length]
  (if (< edit-offset offset)
    (max edit-offset (- offset length))
    offset))

(defn process-single-interval-deletion [^Marker marker ^long offset ^long length]
  (let [from         (.-from marker)
        to           (.-to marker)
        g-l?         (.-greedy-left? marker)
        g-r?         (.-greedy-right? marker)
        from' (shift-offset from offset length)
        to' (shift-offset to offset length)]
    (when (or (< 0 (- to' from')) g-l? g-r?)
      (>Marker :id (.-id marker)
               :from from'
               :to to'
               :greedy-left? g-l?
               :greedy-right? g-r?
               :attrs (.-attrs marker)))))

(defn delete-range [itree ^long offset ^long size]
  (let [offset             (offset->tree-basis offset)
        [itree' intervals] (collect-with-remove itree (+ offset size) (- size) (intersects-pred offset (+ offset size)))
        intervals'         (sort-by #(.-from ^Marker %)
                                    (into []
                                          (comp
                                           (map #(process-single-interval-deletion % offset size))
                                           (filter some?))
                                          intervals))]
    (root (reduce insert-one (zipper itree') intervals'))))

(defn scan-to-range-start [loc ^long from ^long to]
  (let [intersects? (by-intersect from to)
        overscans? (by-offset to)
        marker-zipper-pred (fn [acc metrics] (or (intersects? acc metrics) (overscans? acc metrics)))]
    (tree/scan loc marker-zipper-pred)))

(defn xquery-all [tree]
  (tree/reducible
      (fn [f init]
        (loop [loc (tree/next-leaf (zipper tree))
               acc init]
          (if (reduced? acc)
            @acc
            (let [start (location-from loc)
                  next-loc (tree/next-leaf loc)]
              (cond (= start 0) (recur next-loc acc)
                    (tree/end? next-loc) acc ;; skip sentinel
                    :else (recur next-loc (f acc (loc->Marker loc))))))))))

(deftype IntervalsIterator [^:volatile-mutable loc ^long from ^long to stop?]
  java.util.Iterator
  (hasNext [this]
    (boolean (and (not (tree/end? loc)) (not (< to (location-from loc))))))
  (next [this]
    (let [current (loc->Marker loc)
          loc' (tree/scan (tree/next loc) stop?)]
      (set! loc loc')
      current))
  (remove [this]
    (throw (java.lang.UnsupportedOperationException.))))

(defn iterator [loc ^long from ^long to]
  (assert (<= from to))
  (let [from (offset->tree-basis from)
        to (offset->tree-basis to)
        overlaps? (by-intersect from to)
        overruns? (by-offset to)
        stop? (fn [acc metrics]
                (or (overlaps? acc metrics)
                    (overruns? acc metrics)))
        start-loc (if (and (tree/leaf? loc)
                           (intersects-inclusive? (location-from loc) (location-to loc) from to))
                    loc
                    (tree/scan loc stop?))]
    (IntervalsIterator. start-loc from to stop?)))

(defn xquery-intervals [loc ^long from ^long to]
  (tree/reducible
    (fn [f init]
      (let [^java.util.Iterator iterator (iterator loc from to)]
        (loop [s init]
          (cond
            (reduced? s) @s
            (.hasNext iterator) (recur (f s (.next iterator)))
            :else s))))))

(defn query-intervals [loc from to]
  (into [] (xquery-intervals loc from to)))

(defn- gc-leafs [^ZipperLocation loc ^long bias deleted?]
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
            (deleted? (.-id data))
            (recur (+ bias (-> data (.-offset)))
                   (inc i)
                   true
                   res)

            (< 0 bias)
            (recur 0
                   (inc i)
                   true
                   (al/conj! res (tree/make-leaf
                                  (>Data :id (.-id data)
                                         :offset (+ (.-offset data) bias)
                                         :length (.-length data)
                                         :rightest (.-rightest data)
                                         :marker-ids (.-marker-ids data)
                                         :greedy-left? (.-greedy-left? data)
                                         :greedy-right? (.-greedy-right? data)
                                         :attrs (.-attrs data))
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

(defn by-id [^long id]
  (fn [acc ^Data m]
    (or (contains? (.-marker-ids m) id)
        (= id (.-id m)))))

(defn- find-marker-loc [itree id]
  (let [loc (tree/scan (zipper itree) (by-id id))]
    (when-not (or (tree/end? loc)
                  (tree/node? loc))
      loc)))

(defn find-marker-by-id [itree id]
  (some-> (find-marker-loc itree id)
          loc->Marker))

(defn find-marker-linear-forward ^Marker [pred itree from-offset]
  (loop [loc (tree/scan (zipper itree)
                        (by-offset from-offset))]
    (when-not (tree/end? loc)
      (assert (tree/leaf? (andel.tree/node loc)))
      (let [marker (loc->Marker loc)]
        (if (pred marker)
          marker
          (recur (tree/next-leaf loc)))))))

(defn find-marker-linear-backward ^Marker [pred itree from-offset]
  (loop [loc (tree/prev-leaf
              (tree/scan (zipper itree)
                         (by-offset from-offset)))]
    (when-not (tree/end? loc)
      (assert (tree/leaf? (andel.tree/node loc)))
      (let [marker (loc->Marker loc)]
        (if (pred marker)
          ;; Because of prev-leaf acc might be broken which can cause
          ;; wrong from-to values in marker returned by loc->Marker.
          ;; Getting a proper marker required additional scan from root.
          (find-marker-by-id itree (.-id marker))
          (recur (tree/prev-leaf loc)))))))

(defn update-marker-attrs [itree id f]
  (or (some-> itree
              (find-marker-loc id)
              (update-leaf #(update % :attrs f))
              tree/root)
      itree))

(defprotocol Lexer
  (lexemes [this from to])
  (lexemes-hash [this from to])
  (update-text [this text-tree offset])
  (token-type [this offset])
  (token-type-at-char [this ^long char-offset]))

(defmulti create-lexer (constantly :idea))
(defmethod create-lexer :default [& args] nil)

(defonce empty-tree
  (let [sentinels [(tree/make-leaf (>Data :id -1
                                          :offset 0
                                          :length 0
                                          :rightest 0
                                          :marker-ids nil
                                          :greedy-left? false
                                          :greedy-right? false
                                          :attrs nil) tree-config)
                   (tree/make-leaf (>Data :id -1
                                          :offset plus-infinity
                                          :length 0
                                          :rightest 0
                                          :marker-ids nil
                                          :greedy-left? false
                                          :greedy-right? false
                                          :attrs nil) tree-config)]]
    (tree/make-node sentinels tree-config)))
