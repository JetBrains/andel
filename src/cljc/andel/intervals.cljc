(ns andel.intervals
  (:require [andel.tree :as tree]))


(def plus-infinity #?(:cljs js/Number.POSITIVE_INFINITY
                      :clj 100500 #_Double/POSITIVE_INFINITY))

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
                           :rightest (+ l-rightest r-offset r-rightest) ;; left border of rightest interval in subtree relative to offset
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
  (fn [m]
    (< offset (:rightest m))))

(defn from-to [loc]
  "Last node position relative to accumulated offset"
  (let [m (:metrics (tree/node loc))
        {:keys [offset rightest]} (reducing-fn (tree/loc-acc loc) m)
        from (+ offset rightest)
        length (:length m)]
    {:from from
     :to (+ from length)}))

#_(defn fix-offset [loc offset']
  (tree/edit
   loc
   (fn [{:keys [metrics data] :as leaf}]
     (let [fixed-interval (assoc data :offset offset')]
       (assoc leaf
              :metrics fixed-interval
              :data fixed-interval)))))

#_(defn from-to->intervals [is]
  (first (reduce (fn [[res base] [from to]]
                   [(conj res (Interval. (- from base)
                                         (- to from)
                                         nil))
                    to])
                 [[] 0]
                 is)))

(defn tree->intervals [tr]
  (loop [loc (zipper tr)
         acc []]
    (cond (tree/end? loc)
          acc

          (tree/leaf? loc)
          (recur (tree/next loc)
                 (let [data (get-in loc [0 :data])]
                   (if data
                     (conj acc data)
                     acc)))
          :else
          (recur (tree/next loc)
                 acc))))

(defn intervals->from-to [is]
  (first (reduce (fn [[res base] {:keys [offset len end-pos]}]
                   [(conj res [(+ offset base)
                               (+ offset base len)])
                    (+ offset base len)])
                 [[] 0]
                 is)))

(def tree->from-to
  (comp intervals->from-to tree->intervals))

(defn scan-to-offset [tr offset]
  (tree/scan (zipper tr)
             (by-offset offset)))

(defn make-leaf [offset length]
  (tree/make-leaf (map->Interval {:offset offset
                                  :length length
                                  0})
                  tree-config))

(defn insert-one [r-sibling-loc interval]
  (let [r-offset-old (:offset (:metrics (tree/node r-sibling-loc)))
        {r-from :from r-to :to} (from-to r-sibling-loc)
        {i-from :from i-to :to} interval
        i-len (- i-to i-from)
        new-r-offset (- r-from i-to)
        i-offset (- r-offset-old new-r-offset i-len)]
    (-> r-sibling-loc
        (tree/insert-left (make-leaf i-offset i-len))
        (fix-offset new-r-offset))))

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

(defn add-interval [itree {:keys [from to] :as interval}]
  (let [loc (scan-to-offset itree from)]
    (root (insert-one loc interval))))

(defn query-markers [itree from to]
  (let [start-loc (scan-to-offset itree from)]
    (loop [loc start-loc
           markers []]
      (let [{loc-from :from loc-to :to}  (from-to loc)]
        (cond
          (< to loc-from)
          markers

          (tree/leaf? loc)
          (recur (tree/next loc)
                 (conj markers {:from loc-from :to loc-to}))

          :else
          (recur (tree/next loc)
                  markers))))))

(comment
  (-> [[1 10] [3 6] [12 15]]
      (tree->from-to))
  )

;(defn insert [it is]
;  (->> is
;       (reduce (fn [{:keys [loc updated-base]} i]
;                 (if (some? updated-base)
;                   (let [next-leaf (tree/next loc)
;                         next-leaf-from-to (from-to next-leaf)
;                         loc-fixed (if (<from-to next-leaf-from-to i)
;                                     (fix-offset next-leaf
;                                                 (- () updated-base)))]))
;                (let [insert-loc (tree/scan loc (by-offset (:from i)))]
;                  (insert-one insert-loc i)))
;              {:loc (zipper it)
;               :prev-to nil})
;       (:loc)
;       (root)))

;(insert-bulk tr [{:from 3 :to 5} {:from 5 :to 6}])
