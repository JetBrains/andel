(ns andel.intervals
  (:require [andel.tree :as tree]))

(defrecord Interval [offset len end-pos])

(def plus-infinity #?(:cljs js/Number.POSITIVE_INFINITY
                      :clj Double/POSITIVE_INFINITY))

(defn reducing-fn
  ([] (Interval. nil 0 0))
  ([acc {:keys [offset len] :as i}]
     (let [acc-offset (or (:offset acc) offset)
           end-pos' (+ (:end-pos acc) offset len)
           len' (max (:len acc) (- (+ (:end-pos acc) offset len) acc-offset))]
     (Interval. acc-offset len' end-pos'))))

(def tree-config {::tree/reducing-fn reducing-fn
                  ::tree/metrics-fn identity
                  ::tree/leaf-overflown? (constantly false)
                  ::tree/split-thresh 4
                  ::tree/leaf-underflown? (constantly false)})

(defn zipper [it]
  (tree/zipper it tree-config))

(defn root [loc] (tree/root loc))

(defn by-offset [i]
  #(< i (:end-pos %)))

(defn from-to [loc]
  (let [m (:metrics (tree/node loc))
        {:keys [end-pos]} (reducing-fn (tree/loc-acc loc) m)]
    {:from (- end-pos (:len m))
     :to end-pos}))

(defn make-leaf [left-sibling-pos i]
  (tree/make-leaf (Interval. (- (:from i) left-sibling-pos)
                             (- (:to i) (:from i))
                             nil)
                  tree-config))

(defn fix-offset [loc offset']
  (tree/edit
   loc
   (fn [{:keys [metrics data] :as leaf}]
     (let [fixed-interval (assoc data :offset offset')]
       (assoc leaf
              :metrics fixed-interval
              :data fixed-interval)))))

(defn from-to->intervals [is]
  (first (reduce (fn [[res base] [from to]]
                   [(conj res (Interval. (- from base)
                                         (- to from)
                                         nil))
                    to])
                 [[] 0]
                 is)))

(defn intervals->tree [is]
  (-> (map #(tree/make-leaf % tree-config) is)
      (tree/make-node tree-config)
      (zipper)
      (assoc-in [1 :changed?] true)
      (root)))

(def from-to->tree
  (comp intervals->tree from-to->intervals))

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

(defn insert-one [loc i]
  (let [loc-from-to (from-to loc)]
      (let [left-sibling-pos (:end-pos (tree/loc-acc loc))]
        (-> loc
            (tree/insert-left (make-leaf left-sibling-pos i))
            (fix-offset (- (:from loc-from-to) (:to i)))))))

(defn make-empty-interval-tree []
  (intervals->tree [(Interval. plus-infinity 0 plus-infinity)]))

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
