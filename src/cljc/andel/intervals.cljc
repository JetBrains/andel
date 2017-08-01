(ns andel.intervals
  (:require [andel.tree :as tree]))


(def plus-infinity #?(:cljs js/Number.POSITIVE_INFINITY
                      :clj Integer/MAX_VALUE #_100000 #_Double/POSITIVE_INFINITY))

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
  (fn [acc m]
    (let [m (reducing-fn acc m)]
      (< offset (+ (:offset m) (:rightest m))))))

;; ondefined for non leaf nodes
(defn from-to [loc]
  "Last node position relative to accumulated offset"
  (let [m (:metrics (tree/node loc))
        rightest (or (:rightest (tree/loc-acc loc)) 0)
        from (+ (:offset m) rightest)
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

(defn update-offset [loc f]
  (tree/edit
   loc
   (fn [{:keys [metrics data] :as leaf}]
     (let [fixed-interval (update data :offset f)]
       (assoc leaf
              :metrics fixed-interval
              :data fixed-interval)))))

(defn update-length [loc f]
  (tree/edit
   loc
   (fn [{:keys [metrics data] :as leaf}]
     (let [fixed-interval (update data :length f)]
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
               (let [rightest (or (:rightest acc-metrics) 0)
                     offset (:offset node-metrics)
                     length (:length node-metrics)
                     from (+ offset rightest)]
                 (intersect {:from from
                             :to (+ from length)}
                            interval)))))

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

(defn type-in [itree offset size]
  (assert (<= 0 offset) "Offset must be not negative")
  (let [start-loc (scan-intersect (zipper itree) offset)]
    (loop [loc start-loc]
      (let [{loc-from :from loc-to :to :as loc-from-to} (from-to loc)]
        (cond
          (tree/node? (tree/node loc))
          (recur (tree/next loc))

          (< offset loc-from)
          (root (update-offset loc #(+ size %)))
          
          :else
          (recur (tree/next
                  (if (< offset loc-to)
                    (update-length loc #(+ size %))
                    loc))))))))

;; add rightest right to monoid
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


(comment
  (-> (make-interval-tree)
      (add-intervals [{:from 0 :to 1000} {:from 1 :to 2} {:from 4 :to 8} {:from 16 :to 32} {:from 64 :to 128}
                      {:from 256 :to 512} {:from 700 :to 800}])
      (query-intervals 600 900)
      )

  )
