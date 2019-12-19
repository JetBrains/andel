(ns andel.intervals-test
  (:require [clojure.test.check.generators :as g]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.rose-tree :as rose]
            [clojure.test.check :as tc]
            [clojure.test :refer :all])
  (:import [andel Intervals Intervals$Interval Intervals$Zipper]))

(defn tree->intervals [tree]
  (let [it (Intervals/query tree 0 Intervals/MAX_VALUE)]
    (loop [r []]
      (if (.next it)
        (recur (conj r {:id (.id it)
                        :from (.from it)
                        :greedy-left? (.closedLeft it)
                        :greedy-right? (.closedRight it)
                        :to (.to it)
                        :data (.data it)}))
        r))))

(defn ->interval [{:keys [id from to data greedy-left? greedy-right?]}]
  (Intervals$Interval. id from to (boolean greedy-left?) (boolean greedy-right?) data))

(def empty-tree (Intervals/empty 4))

(defn naive-type-in [intervals [offset length]]
  (if (< 0 length)
    (into []
          (map (fn [{:keys [from to greedy-left? greedy-right?] :as interval}]
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
              interval)))
          intervals)
    (let [length (- length)]
      (into []
            (comp (remove
                   (fn [{:keys [from to greedy-left? greedy-right?]}]
                     (and (< offset from) (< to (+ offset length)))))
                  (map (fn [interval]
                         (let [update-point (fn [point] (if (< offset point)
                                                          (max offset (- point length))
                                                          point))]
                           (-> interval
                               (update :from update-point)
                               (update :to update-point)))))
                  (remove (fn [marker]
                            (and (= (:from marker) (:to marker))
                                 (not (and (:greedy-left? marker)
                                           (:greedy-right? marker)))))))
            intervals))))

(defn intervals-bulk-gen
  ([] (intervals-bulk-gen 1))
  ([next-id & {:keys [allow-empty? greedy-left-override greedy-right-override]}]
   (g/bind
    (g/fmap (fn [size] (range next-id (+ next-id size))) (if allow-empty? g/pos-int g/s-pos-int))
    (fn [ids]
      (let [cnt (count ids)]
        (g/let [froms (g/vector (g/large-integer* {:min 0 :max 10000}) cnt)
                lens (g/vector (g/large-integer* {:min 0 :max 100}) cnt)
                g-l? (g/vector g/boolean cnt)
                g-r? (g/vector g/boolean cnt)]
          (mapv (fn [from len g-l? g-r? id]
                  {:id id
                   :from from
                   :to (+ from len)
                   :greedy-left? (if (some? greedy-left-override)
                                   greedy-left-override
                                   (or (= len 0) g-l?))
                   :greedy-right? (if (some? greedy-right-override)
                                    greedy-right-override
                                    (or (= len 0) g-r?))
                   :data nil})
                (sort froms) lens g-l? g-r? ids)))))))

(defn insert-step-gen [{:keys [ids next-id] :as state}]
  (g/fmap
   (fn [new-intervals]
     [(assoc state
                   :ids (clojure.set/union ids
                                           (into #{} (map :id) new-intervals))
                   :next-id (+ next-id (count new-intervals)))
      [:insert new-intervals]])
   (intervals-bulk-gen next-id)))

(defn delete-step-gen [{:keys [ids] :as state}]
  (g/fmap
   (fn [ids-to-delete]
     [(assoc state :ids (clojure.set/difference ids ids-to-delete))
      [:delete ids-to-delete]])
    (g/not-empty (g/set (g/elements ids)))))

(defn type-in-step-gen [state]
  (g/fmap
   (fn [typings] [state [:type-in typings]])
   (g/tuple (g/large-integer* {:min 0 :max 10000})
            (g/such-that (fn [i] (not= 0 i)) g/int))))

(defn recursive-intervals [s-gen]
  (g/bind s-gen
          (fn [{:keys [ids generated] :as state}]
            (g/sized
             (fn [size]
               (if (= 0 size)
                 (g/return state)
                 (let [next-gen (g/frequency (concat [[1 (insert-step-gen state)]]
                                                     (when (seq ids)
                                                       [[1 (delete-step-gen state)]
                                                        [3 (type-in-step-gen state)]])))]
                   (g/resize (dec size) (recursive-intervals next-gen)))))))))

 (def tree-actions-generator
    (let [op-gen (fn [state]
                   (g/frequency (concat [[1 (insert-step-gen state)]]
                                        (when (seq (:ids state))

                                          [[2 (delete-step-gen state)]
                                           [4 (type-in-step-gen state)]]))))]
    (clojure.test.check.generators.Generator.
     (fn [rnd size]
       (loop [state {:ids #{} :next-id 0}
              r []
              size size]
         (if (= 0 size)
           (rose/shrink (fn [& roses] (into [] (map second) roses)) r)
           (let [next-rose (g/call-gen (op-gen state) rnd size)
                 [state']  (rose/root next-rose)]
             (recur state' (conj r next-rose) (dec size)))))))))

(defn naive-play-op [intervals-vec [op arg]]
  (case op
    :insert (sort-by :from (concat intervals-vec arg))
    :delete (into [] (remove (fn [i] (contains? arg (:id i)))) intervals-vec)
    :type-in (naive-type-in intervals-vec arg)))

(defn play-op [tree [op arg]]
  (case op
    :insert (Intervals/insertAll tree (into [] (map ->interval) arg))
    :delete (Intervals/remove tree arg)
    :type-in (let [[from len] arg]
               (if (< 0 len)
                 (Intervals/expand tree from len)
                 (Intervals/collapse tree from (- len))))))

(defn naive-and-tree-are-same [naive tree]
  (let [actual (tree->intervals tree)]
    (doseq [{:keys [id] :as expected} naive]
      (assert (= (Intervals/getById tree id) (->interval expected))
              {:actual (Intervals/getById tree id)
               :expected (->interval expected)}))
    (let [sorted (sort-by :from actual)]
      (assert (= actual sorted)
              {:actual actual
               :expected sorted}))
    (= (set naive)
       (set actual))))

(def intervals-prop
  (prop/for-all [ops tree-actions-generator]
                 (let [naive (reduce naive-play-op [] ops)
                       tree  (reduce play-op empty-tree ops)]
                   (naive-and-tree-are-same naive tree))))



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


(deftest intervals-test
  ;; TODO check query api
  (is (:result (tc/quick-check 5000 intervals-prop :max-size 100))))

(comment

  (def one-pass-reconciliation-prop
    (let [setup (g/let [initial (intervals-bulk-gen 1
                                                    :greedy-left-override false
                                                    :greedy-right-override false
                                                    :allow-empty? true)
                        additional (intervals-bulk-gen 10000
                                                       :greedy-left-override false
                                                       :greedy-right-override false
                                                       :allow-empty? true)
                        subset (g/fmap (fn [shuffled]
                                         (take (quot (count shuffled) 2) shuffled))
                                       (g/shuffle initial))]
                  [initial (sort (fn [a b]
                                      (let [c (Long/compare (:from a) (:from b))]
                                        (if (= c 0)
                                          (Long/compare (:id a) (:id b))
                                          c)))
                                    (concat subset additional))])]
      (prop/for-all [[initial updated] setup]
        (let [naive updated
              tree (Intervals/insertAll empty-tree (into [] (map ->interval) initial))
              editing-context (andel.intervals.Intervals$EditingContext. (.-nextInnerId tree)
                                                               (.-maxChildren tree)
                                                               (.linear (.-parentsMap tree)))
              ;; when reconciling specific range you should start iteration from (Intervals$Zipper/nextIntersection)
              ;; if there is no intersection just insert all new intervals from the root
              ;; each time after calling next you should check if next marker is beyond given range => zipper is off limit


              from 0
              to Long/MAX_VALUE

              z (Intervals$Zipper/create (.-openRoot tree) editing-context true)
              has-next? (Intervals$Zipper/hasNext z)
              new-root (loop [z (if has-next? (Intervals$Zipper/next z) z)
                              [c & rest :as ints] updated
                              zipper-off-limit? (not has-next?)]
                         (let [what-should-i-do? (if zipper-off-limit?
                                                   (if (nil? c) :return :insert)
                                                   (if (nil? c)
                                                     :remove
                                                     (if (= (:id c) (Intervals$Zipper/id z))
                                                       :skip
                                                       (if (< (:from c) (Intervals$Zipper/from z))
                                                         :insert
                                                         :remove))))]
                           (case what-should-i-do?
                             :return (Intervals$Zipper/root z)
                             :insert (let [{:keys [id from to attrs]} c]
                                       (recur
                                         (Intervals$Zipper/insert z id from to false false attrs)
                                         rest
                                         zipper-off-limit?))
                             :skip (let [has-next? (Intervals$Zipper/hasNext z)]
                                     (recur (if has-next? (Intervals$Zipper/next z) z) rest (not has-next?)))
                             :remove (let [z' (Intervals$Zipper/remove z)
                                           has-next? (Intervals$Zipper/hasNext z')]
                                       (recur (if has-next? (Intervals$Zipper/next z') z') ints (not has-next?))))))
              tree' (andel.intervals.Intervals. (.-maxChildren tree)
                                      new-root
                                      (.-closedRoot tree)
                                      (.forked (.-parentsMap editing-context))
                                      (.-nextId editing-context))]
          (naive-and-tree-are-same naive tree')))))

  (tc/quick-check 100 one-pass-reconciliation-prop :max-size 100)

  (let [initial []
        tree (Intervals/insertAll empty-tree (into [] (map ->interval) initial))
        editing-context (andel.intervals.Intervals$EditingContext. (.-nextInnerId tree)
                                                         (.-maxChildren tree)
                                                         (.linear (.-parentsMap tree)))
        z (Intervals$Zipper/create (.-openRoot tree) editing-context true)
        off-limit? (not (Intervals$Zipper/hasNext z))
        c {:id 10000, :from 0, :to 0, :greedy-left? false, :greedy-right? false, :data nil}]
     (let [{:keys [id from to attrs]} c]
       (np (Intervals$Zipper/root (Intervals$Zipper/insert z id from to false false attrs)))))

  )

(comment

  (g/sample tree-actions-generator 100)

  (defn np [node]
    (if (instance? andel.intervals.Intervals$Node node)
      (let [^andel.intervals.Intervals$Node node node]
        {:starts (.-starts node)
         :ends (.-ends node)
         :ids (.-ids node)
         :children (mapv np (.-children node))})
      (str node)))

  (defn tp [^Intervals tree]
    {:open-root (np (.-openRoot tree))
     :closed-root (np (.-closedRoot tree))
     :mapping (.-parentsMap tree)})

  (def t (reduce play-op empty-tree (first fail)))

  (Intervals/insert t [(->interval {:id 24,
                          :from 0,
                          :to 1,
                          :greedy-left? true,
                          :greedy-right? false,
                          :data nil})
                       ])

  (let [[ops] fail
        naive (reduce naive-play-op [] ops)
        tree (reduce play-op empty-tree ops)
        actual (tree->intervals tree)]
    {:success? (= (set naive) (set actual))
     :naive naive
     :actual actual
     :diff-naive-vs-actual (clojure.data/diff (set naive) (set actual))
     :tree (tp tree)})

  )

