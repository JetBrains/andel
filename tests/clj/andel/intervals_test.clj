(ns andel.intervals-test
  (:require [clojure.test.check.generators :as g]
            [clojure.test.check.properties :as prop]
            [clojure.test.check :as tc]
            [clojure.test :refer :all])
  (:import [andel Intervals Intervals$Interval]))

(defn assert-ids-are-sane [intervals tree]
  (doseq [{:keys [id] :as expected} intervals]
    (let [i (Intervals/getById tree id)
          actual {:from (.-from i)
                  :to (.-to i)
                  :greedy-left? (.closedLeft i)
                  :greedy-right? (.closedRight i)
                  :id (.-id i)
                  :data (.-data i)}]
      (assert (= expected actual)
              {:actual   actual
               :expected expected}))))

(defn tree->intervals [tree]
  (let [it (Intervals/query tree 0 (/ Long/MAX_VALUE 4))]
    (loop [r []]
      (if (.next it)
        (recur (conj r {:id (.id it)
                        :from (.from it)
                        :greedy-left? (.closedLeft it)
                        :greedy-right? (.closedRight it)
                        :to (.to it)
                        :data (.data it)}))
        r))))

(defn intervals-bulk-gen
  ([] (intervals-bulk-gen (g/sized (fn [size] (g/return (range size))))))
  ([ids-gen]
   (g/bind
    ids-gen
    (fn [ids]
      (let [cnt (count ids)]
        (g/let [a (g/vector (g/large-integer* {:min 0 :max 10000}) cnt)
                b (g/vector (g/large-integer* {:min 0 :max 10000}) cnt)
                g-l? (g/vector g/boolean cnt)
                g-r? (g/vector g/boolean cnt)]
          (->> (mapv (fn [a b g-l? g-r? id]
                       {:id id
                        :from (min a b)
                        :to (max a b)
                        :greedy-left? (or (= a b) g-l?)
                        :greedy-right? g-r?
                        :data nil})
                     a b g-l? g-r? ids)
               (sort-by :from)
               (into []))))))))

(defn ->interval [{:keys [id from to data greedy-left? greedy-right?]}]
  (Intervals$Interval. id from to (boolean greedy-left?) (boolean greedy-right?) data))

(def empty-tree (Intervals. 4))

(def bulk-insertion-prop
  (prop/for-all [intervals (intervals-bulk-gen)]
    (let [tree (Intervals/insert empty-tree (into [] (map ->interval) intervals))]
      (assert-ids-are-sane intervals tree)
      (= (tree->intervals tree) intervals))))

(def bulk-of-bulks-prop
  (prop/for-all [bulk-of-bulks (g/fmap
                                (fn [[intervals nums]]
                                  (loop [intervals intervals
                                         [n & nums] nums
                                         result []]
                                    (if (or (nil? n) (<= (count intervals) n))
                                      (conj result (into [] intervals))
                                      (recur
                                        (drop n intervals)
                                        nums
                                        (conj result (into [] (take n) intervals))))))
                                (g/tuple (intervals-bulk-gen)
                                         (g/vector g/s-pos-int)))]
    (let [itree (reduce
                 (fn [t bulk]
                   (Intervals/insert t (into [] (map ->interval) bulk)))
                 empty-tree
                 bulk-of-bulks)]
      (= (sort-by :from (mapcat identity bulk-of-bulks))
         (tree->intervals itree)))))

(defn naive-type-in [intervals [offset length]]
  (if (< length 0)
    (into []
          (comp (map (fn [interval]
                       (let [update-point (fn [point offset length] (if (< offset point)
                                                                      (max offset (+ point length))
                                                                      point))]
                         (-> interval
                             (update :from update-point offset length)
                             (update :to update-point offset length)))))
                (remove (fn [marker]
                          (and (= (:from marker) (:to marker))
                               (not (:greedy-left? marker))
                               (not (:greedy-right? marker))))))
          intervals)
    (mapv (fn [{:keys [from to greedy-left? greedy-right?] :as interval}]
            (cond
              (and greedy-left?
                   (= offset from))
              (assoc interval :to (+ to length))

              (and greedy-right?
                   (= offset to))
              (assoc interval :to (+ to length))

              (and (< from offset)
                   (< offset to))
              (assoc interval :to (+ to length))

              (<= offset from)
              (assoc interval
                     :to (+ to length)
                     :from (+ from length))

              :else
              interval))
          intervals)))

(defn type-in [tree [offset size]]
  (if (< size 0)
    (Intervals/collapse tree offset (Math/abs size))
    (Intervals/expand tree offset size)))

(def type-in-prop
  (prop/for-all [[intervals typings] (g/bind (intervals-bulk-gen)
                                       (fn [bulk]
                                         (let [max-val (transduce (map :to) max 0 bulk)]
                                           (g/tuple (g/return bulk)
                                                    (g/vector (g/tuple (g/large-integer* {:min 0 :max max-val})
                                                                       g/int))))))]
    (let [tree (Intervals/insert empty-tree (into [] (map ->interval) intervals))]
      (= (reduce naive-type-in intervals typings)
         (tree->intervals (reduce type-in tree typings))))))

(deftest intervals-test
  (is (:result (tc/quick-check 1000 bulk-insertion-prop)))
  (is (:result (tc/quick-check 1000 bulk-of-bulks-prop)))
  (is (:result (tc/quick-check 1000 type-in-prop))))

(defn intersects? [^long from1 ^long to1 ^long from2 ^long to2]
  (if (<= from1 from2)
    (< from2 to1)
    (< from1 to2)))

(defn play-query [model {:keys [from to]}]
  (vec (filter (fn [m] (intersects? (:from m) (:to m) from to)) model)))

(defn query-gen [max-val]
  (g/fmap (fn [[x y]]
            {:from (min x y)
             :to   (max x y)})
          (g/tuple (g/large-integer* {:min 0 :max max-val})
                   (g/large-integer* {:min 0 :max max-val}))))

(def bulk-and-queries-gen
  (g/bind (intervals-bulk-gen)
          (fn [bulk] (g/tuple (g/return bulk)
                              (g/vector (query-gen (->> bulk
                                                        (map :to)
                                                        (apply max 0))))))))

(defn insert-step-gen [[ids generated]]
  (let [next-id (inc (reduce max 0 ids))]
    (g/bind
     (g/not-empty
      (g/sized (fn [size] (g/return (set (range next-id (+ next-id size)))))))
     (fn [new-ids]
       (g/fmap
        (fn [bulk]
          [(clojure.set/union ids new-ids)
           (conj generated [:insert bulk])])
        (intervals-bulk-gen (g/return new-ids)))))))

(defn delete-step-gen [[ids generated]]
  (g/fmap
   (fn [ids-to-delete]
     [(clojure.set/difference ids ids-to-delete)
      (conj generated [:delete ids-to-delete])])
   (g/not-empty (g/set (g/elements ids)))))

(defn type-in-step-gen [[ids generated]]
  (g/fmap
    (fn [typings]
      [ids
       (conj generated [:type-in typings])])
   (g/not-empty
    (g/vector (g/tuple (g/large-integer* {:min 0 :max 10000})
                       g/int)))))

(defn my-gen [s-gen]
  (g/bind s-gen
          (fn [[ids generated :as state]]
            (g/sized
             (fn [size]
               (if (= 0 size)
                 (g/return state)
                 (let [next-gen (g/one-of (concat [(insert-step-gen state)]
                                                  (when (seq ids)
                                                    [(delete-step-gen state)
                                                     (type-in-step-gen state)])))]
                   (g/resize (dec size) (my-gen next-gen)))))))))

(defn naive-play-op [intervals-vec [op arg]]
  (case op
    :insert (sort-by :from (concat intervals-vec arg))
    :delete (into [] (remove (fn [i] (contains? arg (:id i)))) intervals-vec)
    :type-in (reduce naive-type-in intervals-vec arg)))

(defn play-op [tree [op arg]]
  (case op
    :insert (Intervals/insert tree (into [] (map ->interval) arg))
    :delete (Intervals/remove tree arg)
    :type-in (reduce
               (fn [tree [from len]]
                 (if (< 0 len)
                   (Intervals/expand tree from len)
                   (Intervals/collapse tree from (- len))))
              tree
              arg)))

(tc/quick-check
 10
 (prop/for-all [[_ ops] (my-gen (g/return [#{} []]))]
               (let [naive (reduce naive-play-op [] ops)
                     tree  (reduce play-op empty-tree ops)]
                 (assert-ids-are-sane naive tree)

                 ;;todo assert intervals are ordered
                 (= (set naive)
                    (set (tree->intervals tree))))))


(comment

  (defn np [node]
    (if (instance? andel.Intervals$Node node)
      (let [^andel.Intervals$Node node node]
        {:starts (.-starts node)
         :ends (.-ends node)
         :ids (.-ids node)
         :children (mapv np (.-children node))})
      (str node)))

  (defn tp [^Intervals tree]
    {:open-root (np (.-openRoot tree))
     :closed-root (np (.-closedRoot tree))
     :mapping (.-parentsMap tree)})

  (def fail
    [[#{1 4 6 3 2 5}
            [[:insert
              [{:id 3,
                :from 0,
                :to 0,
                :greedy-left? true,
                :greedy-right? false,
                :data nil}
               {:id 2,
                :from 0,
                :to 0,
                :greedy-left? true,
                :greedy-right? false,
                :data nil}
               {:id 1,
                :from 1,
                :to 1,
                :greedy-left? true,
                :greedy-right? true,
                :data nil}]]
             [:insert
              [{:id 4,
                :from 0,
                :to 0,
                :greedy-left? true,
                :greedy-right? false,
                :data nil}
               {:id 5,
                :from 0,
                :to 1,
                :greedy-left? false,
                :greedy-right? false,
                :data nil}]]
             [:insert
              [{:id 6,
                :from 0,
                :to 0,
                :greedy-left? true,
                :greedy-right? false,
                :data nil}]]
             [:type-in [[0 -1]]]]]])

  (let [[[_ ops]] fail
        naive (reduce naive-play-op [] ops)
        tree (reduce play-op empty-tree ops)
        actual (tree->intervals tree)]
    {:success? (= (set naive) (set actual))
     :naive naive
     :actual actual
     :diff (clojure.data/diff (set naive) (set actual))
     :tree (tp tree)})


  (let [sample #_(sample (map (fn [i] {:from i :to (* 2 i) :data (str i "cm")}) (range 25)))
        (sample [{:from 0 :to 1 :id 0 :greedy-left? false :greedy-right? true :data nil}
;                 {:from 0 :to 1 :id 1 :greedy-left? true :greedy-right? false :data nil}
 ;                {:from 0 :to 1 :id 2 :greedy-left? true :greedy-right? false :data nil}
                 ;{:from 1 :to 2 :id 1 :greedy-left? false :greedy-right? true :data nil}
                 ;{:from 2 :to 4 :id 2 :greedy-left? true :greedy-right? true :data nil}
                 ;{:from 3 :to 6 :id 3 :greedy-left? true :greedy-right? false :data nil}
                 ;{:from 4 :to 8 :id 4 :greedy-left? true :greedy-right? false :data nil}
                 ;{:from 5 :to 10 :id 5 :greedy-left? true :greedy-right? true :data nil}
                 ;{:from 6 :to 12 :id 6 :greedy-left? false :greedy-right? true :data nil}
                 ;{:from 7 :to 14 :id 7 :greedy-left? false :greedy-right? true :data nil}
                 ])
        t (-> (Intervals. 4)
              (Intervals/insert sample)
              #_(Intervals/remove [3 4 5])
              #_(Intervals/expand 1 1)
              )
        ;it (Intervals/query t 2 4)
        ]
    (tp t)
    (tree->intervals t)
    #_(loop [r []]
      (if (.next it)
        (recur (conj r {:from (.from it) :to (.to it) :gl? (.closedLeft it) :gr? (.closedRight it) :id (.id it) :data (.data it)}))
        r))
    )

   (Math/abs (rem -1 2))



  *e
  )

