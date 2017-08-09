(ns andel.intervals-test
  (:require [clojure.test.check.generators :as g]
            [clojure.test.check.properties :as prop]
            [clojure.test.check :as tc]
            [andel.intervals :refer :all]
            [andel.tree :as tree]
            [clojure.test :refer :all]))


(defn compare-intervals [a b]
  (or (< (:from a) (:from b))
      (and (= (:from a) (:from b))
           (< (:to a) (:to b)))
      (and (= (:from a) (:from b))
           (= (:to a) (:to b))
           (and (:greedy-left? a)
                (not (:greedy-left? b))))
      (and (= (:from a) (:from b))
           (= (:to a) (:to b))
           (= (:greedy-left? a) (:greedy-left? b))
           (and (:greedy-right? a)
                (not (:greedy-right? b))))))

(def interval-comparator (comparator compare-intervals))

(def sort-intervals (partial sort interval-comparator))

(deftest simple-insert
  (let [itree (-> (make-interval-tree)
                  (add-intervals [{:from 5 :to 10 :greedy-left? false :greedy-right? false}]))]
    (is (= (tree->intervals itree) [{:from 5 :to 10 :greedy-left? false :greedy-right? false}]))))

(deftest multiple-insertitions
  (let [intervals [{:from 1 :to 10 :greedy-left? false :greedy-right? false}
                   {:from 9 :to 11 :greedy-left? false :greedy-right? false}
                   {:from 8 :to 12 :greedy-left? false :greedy-right? false}
                   {:from 3 :to 17 :greedy-left? false :greedy-right? false}
                   {:from 2 :to 18 :greedy-left? false :greedy-right? false}
                   {:from 7 :to 13 :greedy-left? false :greedy-right? false}
                   {:from 6 :to 14 :greedy-left? false :greedy-right? false}
                   {:from 5 :to 15 :greedy-left? false :greedy-right? false}
                   {:from 4 :to 16 :greedy-left? false :greedy-right? false}]
        itree (reduce add-intervals (make-interval-tree) (map vector intervals))]
    (is (= (tree->intervals itree) (sort-by :from intervals)))))

(deftest bulk-insertitions
  (let [intervals (sort-by :from  [{:from 0 :to 10 :greedy-left? false :greedy-right? false}
                                   {:from 0 :to 11 :greedy-left? false :greedy-right? false}
                                   {:from 0 :to 12 :greedy-left? false :greedy-right? false}
                                   {:from 0 :to 17 :greedy-left? false :greedy-right? false}
                                   {:from 2 :to 18 :greedy-left? false :greedy-right? false}
                                   {:from 7 :to 13 :greedy-left? false :greedy-right? false}
                                   {:from 6 :to 14 :greedy-left? false :greedy-right? false}
                                   {:from 5 :to 15 :greedy-left? false :greedy-right? false}
                                   {:from 4 :to 16 :greedy-left? false :greedy-right? false}])
        itree (add-intervals (make-interval-tree) intervals)]
    (is (= (tree->intervals itree) intervals))))

(def intervals-bulk-gen (g/fmap (fn [v] (vec (sort interval-comparator v)))
                                (g/vector
                                 (g/fmap (fn [[a b g-l? g-r?]]
                                           {:from (min a b)
                                            :to (max a b)
                                            :greedy-left? g-l?
                                            :greedy-right? g-r?})
                                         (g/tuple (g/large-integer* {:min 1 :max 10000})
                                                  (g/large-integer* {:min 1 :max 10000})
                                                  g/boolean
                                                  g/boolean))
                                 0 100)))

(deftest bulk-insertion
  (is (:result (tc/quick-check 1000
                               (prop/for-all [bulk intervals-bulk-gen]
                                             (let [itree (add-intervals (make-interval-tree) bulk)]
                                               (= (tree->intervals itree)
                                                  bulk)))))))

(deftest multiple-bulk-insertion
  (is (:result (tc/quick-check 100
                               (prop/for-all [bulk-bulk (g/vector intervals-bulk-gen)]
                                             (let [itree (reduce add-intervals
                                                                 (make-interval-tree)
                                                                 bulk-bulk)]
                                               (= (sort-intervals (mapcat vec bulk-bulk))
                                                  (sort-intervals (tree->intervals itree)))))))))

;; [{from to :g-l? :g-r?}] -> [offset size] -> [{:from :to :g-l? :g-r?}]
(defn play-type-in-once [model [offset size]]
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

;; [{:from :to :g-l? :g-r?] -> [[offset size]] -> [{:from :to :g-l? g-r?}]
(defn play-type-in-many [model [q & qs]]
  (if (nil? q)
    model
    (recur (play-type-in-once model q) qs)))

(def bulk-offset-size-gen
  (g/bind intervals-bulk-gen
          (fn [bulk] (let [max-val (->> bulk
                                        (map :to)
                                        (apply max 0))]
                       (g/tuple (g/return bulk)
                                (g/vector (g/tuple (g/large-integer* {:min 1 :max (inc max-val)})
                                                   (g/large-integer* {:min 1 :max 10000}))))))))

(deftest type-in-positive-test
  (is (:result (tc/quick-check 1000
                               (prop/for-all [[bulk qs] bulk-offset-size-gen]
                                             (= (sort-intervals (play-type-in-many bulk qs))
                                                (sort-intervals (tree->intervals
                                                                 (reduce type-in
                                                                         (-> (make-interval-tree)
                                                                             (add-intervals bulk))
                                                                         qs)))))))))

(defn play-query [model {:keys [from to]}]
  (vec (filter #(intersect % {:from from :to to}) model)))

(defn from-to-gen [max-val]
  (g/fmap (fn [[x y]] (if (< x y)
                        {:from x :to y}
                        {:from y :to x}))
          (g/tuple (g/large-integer* {:min 0 :max max-val})
                   (g/large-integer* {:min 0 :max max-val}))))

(def bulk-and-queries-gen
  (g/bind intervals-bulk-gen
          (fn [bulk] (g/tuple (g/return bulk)
                              (g/vector (from-to-gen (->> bulk
                                                       (map :to)
                                                       (apply max 0))))))))

(deftest query-test
  (is (:result (tc/quick-check 100
                               (prop/for-all [[bulk queries] bulk-and-queries-gen]
                                             (= (map play-query (repeat bulk) queries)
                                                (let [generated-tree (-> (make-interval-tree)
                                                                         (add-intervals bulk))]
                                                  (map query-intervals (repeat generated-tree) queries))))))))


(defn q&l-wrapper [itree from-to]
  (first (query-and-loc (zipper itree) from-to)))

(def simple-q&l-test
  (is (:result (tc/quick-check 100
                               (prop/for-all [[bulk queries] bulk-and-queries-gen]
                                             (= (map play-query (repeat bulk) queries)
                                                (let [generated-tree (-> (make-interval-tree)
                                                                         (add-intervals bulk))]
                                                  (map q&l-wrapper (repeat generated-tree) queries))))))))

(defn play-queries [model [q & qs] acc]
  (if (nil? q)
    acc
    (recur model
           qs
           (conj acc
                 (vec (filter #(intersect % q) model)))
           )))

(def bulk-and-increasing-queries-gen
  (g/bind intervals-bulk-gen
          (fn [bulk] (g/tuple (g/return bulk)
                              (let [max-val (->> bulk
                                             (map :to)
                                             (apply max 0))]
                                (g/fmap #(first (reduce (fn [[acc cur] [a b]]
                                                          [(conj acc {:from (min (inc cur) max-val)
                                                                      :to (min (+ a b (inc cur)) max-val)})
                                                           (+ cur a)])
                                                        [[] 0]
                                                        %))
                                        (g/vector (g/tuple g/nat g/nat))))))))

(def queries-test
  (is (:result (tc/quick-check 100
                               (prop/for-all [[bulk queries] bulk-and-increasing-queries-gen]
                                             (= (play-queries bulk queries [])
                                                (let [begin-loc (-> (make-interval-tree)
                                                              (add-intervals bulk)
                                                              (zipper))]
                                                  (first (reduce (fn [[acc loc] q]
                                                                   (let [[ans new-loc] (query-and-loc loc q)]
                                                                     [(conj acc ans) new-loc]))
                                                                 [[] begin-loc]
                                                                 queries)))))))))

(comment
  (run-tests)
  (clojure.test/test-vars [#'andel.intervals-test/generative]))
