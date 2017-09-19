(ns andel.intervals-test
  (:require [clojure.test.check.generators :as g]
            [clojure.test.check.properties :as prop]
            [clojure.test.check :as tc]
            [andel.intervals :refer :all]
            [andel.tree :as tree]
            [clojure.test :refer :all]))


(defn compare-intervals [a b]
  (< (:from a) (:from b)))

(def sort-intervals (partial sort (comparator compare-intervals)))

(def interval-gen (g/fmap (fn [[a b g-l? g-r?]]
                            (map->Marker {:from (min a b)
                                          :to (max a b)
                                          :greedy-left? g-l?
                                          :greedy-right? g-r?
                                          :attrs nil}))
                          (g/tuple (g/large-integer* {:min 0 :max 10000})
                                   (g/large-integer* {:min 0 :max 10000})
                                   g/boolean
                                   g/boolean)))

(def intervals-bulk-gen (g/fmap (fn [v] (vec (sort-intervals v)))
                                (g/vector interval-gen 0 100)))

(deftest bulk-insertion
  (is (:result (tc/quick-check 100
                               (prop/for-all [bulk intervals-bulk-gen]
                                             (let [itree (add-markers (make-interval-tree) bulk)]
                                               (= (tree->intervals itree)
                                                  bulk)))))))

(deftest multiple-bulk-insertion
  (is (:result (tc/quick-check 30
                               (prop/for-all [bulk-bulk (g/vector intervals-bulk-gen)]
                                             (let [itree (reduce add-markers
                                                                 (make-interval-tree)
                                                                 bulk-bulk)]
                                               (= (set (mapcat vec bulk-bulk))
                                                  (set (tree->intervals itree)))))))))

;; [interval] -> [offset size] -> [interval]
(defn play-type-in [model [offset size]]
  (vec (->> model
            (map (fn [{:keys [from to greedy-left? greedy-right?] :as interval}]
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
                     interval))))))

(def bulk-offset-size-gen
  (g/bind intervals-bulk-gen
          (fn [bulk] (let [max-val (->> bulk
                                        (map :to)
                                        (apply max 0))]
                       (g/tuple (g/return bulk)
                                (g/vector (g/tuple (g/large-integer* {:min 0 :max max-val})
                                                   (g/large-integer* {:min 0 :max 10000}))))))))

(defn bulk->tree [bulk]
  (-> (make-interval-tree)
      (add-markers bulk)))


(deftest type-in-positive-test
  (is (:result (tc/quick-check 100
                               (prop/for-all [[bulk qs] bulk-offset-size-gen]
                                             (= (set (reduce play-type-in bulk qs))
                                                (set (tree->intervals
                                                       (reduce (fn [t [o s]] (type-in t o s)) (bulk->tree bulk) qs)))))))))

;; model -> [offset size] -> model
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
                                     (= (set (reduce play-delete-range bulk qs))
                                        (set (tree->intervals
                                               (reduce (fn [t [o s]] (delete-range t o s)) (bulk->tree bulk) qs))))))
       )))

(defn play-query [model {:keys [from to]}]
  (vec (filter (fn [m] (intersects? (.-from m) (.-to m) from to)) model)))

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

(deftest query-test
  (is (:result (tc/quick-check 100
                               (prop/for-all [[bulk queries] bulk-and-queries-gen]
                                             (= (map play-query (repeat bulk) queries)
                                                (map query-intervals (repeat (bulk->tree bulk)) queries)))))))

(def operation-gen
  (g/tuple (g/one-of [(g/return [play-type-in type-in])
                      (g/return [play-delete-range delete-range])])
           (g/tuple (g/large-integer* {:min 0 :max 10000000})
                    (g/large-integer* {:min 0 :max 10000000}))))

(deftest type-and-delete-test
  (is (:result
       (tc/quick-check 100
                               (prop/for-all
                                [bulk intervals-bulk-gen
                                 ops (g/vector operation-gen)]
                                (let [[tree model] (reduce (fn [[tree model] [[play real] args]]
                                                             [(apply real tree args)
                                                              (play model args)])
                                                           [(bulk->tree bulk) bulk]
                                                           ops)]
                                  (= (set (tree->intervals tree))
                                     (set model)))))
       )

      ))
