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
    (< offset (+ (:offset m) (:rightest m)))))

;; ondefined for non leaf nodes
(defn from-to [loc]
  "Last node position relative to accumulated offset"
  (let [m (:metrics (tree/node loc))
        {:keys [offset rightest]} (reducing-fn (tree/loc-acc loc) m)
        from (+ offset rightest)
        length (:length m)]
    {:from from
     :to (+ from length)}))

(defn fix-offset [loc offset']
  (tree/edit
   loc
   (fn [{:keys [metrics data] :as leaf}]
     (let [fixed-interval (assoc data :offset offset')]
       (assoc leaf
              :metrics fixed-interval
              :data fixed-interval)))))

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

(defn scan-to-offset [tr offset]
  (tree/scan (zipper tr)
             (by-offset offset)))

(defn scan-to-end [tr offset]
  (tree/scan (zipper tr)
             (fn [m]
               (< offset (+ (:offset m) (:length m))))))

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
        (fix-offset new-r-offset))))

(defn add-intervals [itree intervals]
  (root (reduce insert-one (zipper itree) intervals)))

;; add rightest right to monoid
(defn query-intervals [itree from to]
  (let [start-loc (scan-to-end itree from)]
    (loop [loc start-loc
           markers []]
      (let [{loc-from :from loc-to :to :as loc-from-to} (from-to loc)]
        (cond
          (tree/node? (tree/node loc))
          (recur (tree/next loc)
                 markers)

          (< to loc-from)
          markers
          
          :else
          (recur (tree/next loc)
                 (if (< from loc-to)
                   (conj markers loc-from-to)
                   markers)))))))
