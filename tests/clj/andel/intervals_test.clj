(ns andel.intervals-test
  (:require [clojure.test.check.generators :as g]
            [clojure.test.check.properties :as prop]
            [clojure.test.check :as tc]
            [clojure.test :refer :all])
  (:import [andel Intervals Intervals$Interval]))

(def sort-intervals [intervals]
  (sort-by :from intervals))

(defn tree->intervals [tree]
  (let [it (Intervals/query tree 0 Long/MAX_VALUE)]
    (loop [r []]
      (if (.next it)
        (recur (conj r {:from (.from it)
                        :to (.to it)
                        :id (.id it)
                        :data (.data it)}))
        r))))

(def intervals-bulk-gen
  (g/bind
   (g/large-integer* {:min 1 :max 1000})
   (fn [cnt]
     (g/let [a (g/vector (g/large-integer* {:min 0 :max 10000}) cnt)
             b (g/vector (g/large-integer* {:min 0 :max 10000}) cnt)
             g-l? (g/vector g/boolean cnt)
             g-r? (g/vector g/boolean cnt)
             ids (g/return (vec (range cnt)))]
       (->> (mapv (fn [a b g-l? g-r? id]
                    {:id id
                     :from (min a b)
                     :to (max a b)
                     ;      :greedy-left? g-l?
                     ;     :greedy-right? g-r?
                     :data nil})
                  a b g-l? g-r? ids)
            (sort-by :to))))))

(defn ->interval [{:keys [id from to data]}]
  (Intervals$Interval. 0 from to data))

(def bulk-insertion-prop
  (prop/for-all [bulk intervals-bulk-gen]
    (let [tree (Intervals/insert (Intervals. 4)
                                 (into [] (map ->interval) bulk))]
      (= (tree->intervals tree) bulk))))

(deftest intervals-test
  (is (:result (tc/quick-check 1000 bulk-insertion-prop))))

;(deftest multiple-bulk-insertion
;  (is (:result (tc/quick-check 100
;                               (prop/for-all [bulk-bulk (g/vector intervals-bulk-gen)]
;                                             (let [itree (reduce add-markers
;                                                                 empty-tree
;                                                                 bulk-bulk)]
;                                               (= (set (mapcat vec bulk-bulk))
;                                                  (set (tree->intervals itree)))))))))

;; [interval] -> [offset size] -> [interval]

(defn play-type-in [intervals [offset size]]
  (mapv (fn [{:keys [from to greedy-left? greedy-right?] :as interval}]
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
            interval))
        intervals))

(def bulk-offset-size-gen
  (g/bind intervals-bulk-gen
          (fn [bulk]
            (let [max-val (transduce (map :to) max 0 bulk)]
              (g/tuple (g/return bulk)
                       (g/vector (g/tuple (g/large-integer* {:min 0 :max max-val})
                                          (g/large-integer* {:min 0 :max 10000}))))))))

(defn bulk->tree [bulk]
  (-> (Intervals. 32)
      (Intervals/insert (into [] (map ->interval)
                              bulk))))


(deftest type-in-positive-test
  (is
   (:result
    (tc/quick-check
     1000
     (prop/for-all [[bulk qs] bulk-offset-size-gen]
                   (= (set (reduce play-type-in bulk qs))
                      (set (tree->intervals
                            (reduce (fn [t [o s]] (Intervals/expand t o s))
                                    (bulk->tree bulk) qs)))))))))

(defn drop-dead-markers [markers]
  (remove (fn [marker]
            (and (= (:from marker) (:to marker)) (not (:greedy-left? marker)) (not (:greedy-right? marker))))
          markers))

(defn play-delete-range [model [offset length]]
  (map (fn [interval]
         (let [update-point (fn [point offset length] (if (< offset point)
                                                       (max offset (- point length))
                                                       point))]
           (-> interval
               (update :from update-point offset length)
               (update :to update-point offset length))))
       model))

(deftest test-delete-range
  (is (:result
       (tc/quick-check 1000
                       (prop/for-all [[bulk qs] bulk-offset-size-gen]
                                     (= (set (drop-dead-markers (reduce play-delete-range bulk qs)))
                                        (set (drop-dead-markers (tree->intervals
                                                                 (reduce (fn [t [o s]] (Intervals/collapse t o s)) (bulk->tree bulk) qs))))))))))

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
  (g/bind intervals-bulk-gen
          (fn [bulk] (g/tuple (g/return bulk)
                              (g/vector (query-gen (->> bulk
                                                        (map :to)
                                                        (apply max 0))))))))

(let [[bulk queries] [[{:from 0, :to 1, :data nil} {:from 0, :to 0, :data nil}]
                      [{:from 0, :to 1}]]]
  (map play-query (repeat bulk) queries))

(deftest query-test
  (is
   (:result
    (tc/quick-check
     1000
     (prop/for-all [[bulk queries] bulk-and-queries-gen]
                   (let [itree (bulk->tree bulk)]
                     (= (map play-query (repeat bulk) queries)
                        (map (fn [{:keys [from to]}]
                               (let [it (Intervals/query itree from to)]
                                 (loop [r []]
                                   (if (.next it)
                                     (recur (conj r {:from (.from it) :to (.to it) :id (.id it) :data (.data it)}))
                                     r))))
                             queries))))))))
;
;(def operation-gen
;  (g/tuple (g/one-of [(g/return [play-type-in type-in])
;                      (g/return [play-delete-range delete-range])])
;           (g/tuple (g/large-integer* {:min 0 :max 10000000})
;                    (g/large-integer* {:min 0 :max 10000000}))))
;
;(deftest type-and-delete-test
;  (is (:result
;       (tc/quick-check
;        1000
;        (prop/for-all
;         [bulk intervals-bulk-gen
;          ops (g/vector operation-gen)]
;         (let [[tree model] (reduce (fn [[tree model] [[play real] args]]
;                                      [(apply real tree args)
;                                       (play model args)])
;                                    [(bulk->tree bulk) bulk]
;                                    ops)]
;           (= (set (drop-dead-markers (tree->intervals tree)))
;              (set (drop-dead-markers model)))))))))
;
;(deftest gc-test
;  (is
;   (:result
;    (tc/quick-check
;     100
;     (prop/for-all [[bulk i] (g/bind intervals-bulk-gen
;                                     (fn [b]
;                                       (g/tuple (g/return b) (g/choose 0 (count b))))
;                                     )]
;                   (let [tree (bulk->tree bulk)
;                         to-delete (i/int-set (range i))
;                         tree' (gc tree to-delete)]
;                     (= (tree->intervals tree')
;                        (remove (fn [m] (to-delete (-> m (.-attrs) (.-id)))) bulk))
;                     ))))))

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

  (defn sample [input]
    (reduce
      (fn [al {:keys [id from to data]}]
        (.add al
              (Intervals$Interval. id
                                   from
                                   to
                                   data))
        al)
     (java.util.ArrayList.)
     input))

  (sample (map (fn [i] {:from i :to (* 2 i) :data (str i "cm")}) (range 25)))



  (let [sample #_(sample (map (fn [i] {:from i :to (* 2 i) :data (str i "cm")}) (range 25)))
        (sample [
                  {:from 0, :to 0, :id 0, :data nil}
                 {:from 0, :to 0, :id 1, :data nil}
                 {:from 0, :to 0, :id 2, :data nil}
                 {:from 0, :to 0, :id 3, :data nil}
                 {:from 0, :to 0, :id 4, :data nil}
                 {:from 0, :to 0, :id 5, :data nil}
                 {:from 0, :to 1, :id 6 :data nil}
                 {:from 0, :to 1, :id 7, :data nil}])
        t (-> (Intervals. 4)
              (Intervals/insert sample)
              #_(Intervals/collapse 10 10)
              #_(Intervals/expand 1 1)
              )
        it (Intervals/query t 0 1)]
    (tp t)
    #_(loop [r []]
      (if (.next it)
        (recur (conj r {:from (.from it) :to (.to it) :id (.id it) :data (.data it)}))
        r))
    )

  )

