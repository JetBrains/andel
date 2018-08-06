(ns andel.intervals-test
  (:require [clojure.test.check.generators :as g]
            [clojure.test.check.properties :as prop]
            [clojure.test.check :as tc]
            [andel.intervals :refer :all]
            [andel.tree :as tree]
            [clojure.test :refer :all]
            [clojure.data.int-map :as i]))


(defn compare-intervals [a b]
  (< (:from a) (:from b)))

(def sort-intervals (partial sort (comparator compare-intervals)))


(def intervals-bulk-gen
  (g/bind (g/large-integer* {:min 1 :max 1000})
          (fn [cnt]
            (g/let [a (g/vector (g/large-integer* {:min 0 :max 10000}) cnt)
                    b (g/vector (g/large-integer* {:min 0 :max 10000}) cnt)
                    g-l? (g/vector g/boolean cnt)
                    g-r? (g/vector g/boolean cnt)
                    ids (g/return (vec (range cnt)))]
              (sort-intervals (mapv (fn [a b g-l? g-r? id]
                                      (>Marker :from (min a b)
                                               :to (max a b)
                                               :greedy-left? g-l?
                                               :greedy-right? g-r?
                                               :attrs (>Attrs :id id)))
                                    a b g-l? g-r? ids))))))

(deftest bulk-insertion
  (is (:result (tc/quick-check 1000
                               (prop/for-all [bulk intervals-bulk-gen]
                                             (let [itree (add-markers empty-tree bulk)]
                                               (= (tree->intervals itree)
                                                  bulk)))))))

(deftest multiple-bulk-insertion
  (is (:result (tc/quick-check 100
                               (prop/for-all [bulk-bulk (g/vector intervals-bulk-gen)]
                                             (let [itree (reduce add-markers
                                                                 empty-tree
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
  (-> empty-tree
      (add-markers bulk)))


(deftest type-in-positive-test
  (is
   (:result
    (tc/quick-check
     1000
     (prop/for-all [[bulk qs] bulk-offset-size-gen]
                   (= (set (reduce play-type-in bulk qs))
                      (set (tree->intervals
                            (reduce (fn [t [o s]] (type-in t o s))
                                    (bulk->tree bulk) qs)))))))))

(defn drop-dead-markers [markers]
  (remove (fn [marker]
            (and (= (:from marker) (:to marker)) (not (:greedy-left? marker)) (not (:greedy-right? marker))))
          markers))

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
                                     (= (set (drop-dead-markers (reduce play-delete-range bulk qs)))
                                        (set (drop-dead-markers (tree->intervals
                                                                 (reduce (fn [t [o s]] (delete-range t o s)) (bulk->tree bulk) qs))))))))))

(defn play-query [model {:keys [from to]}]
  (vec (filter (fn [m] (intersects-inclusive? (.-from m) (.-to m) from to)) model)))

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
  (is
   (:result
    (tc/quick-check
     1000
     (prop/for-all [[bulk queries] bulk-and-queries-gen]
                   (let [itree (bulk->tree bulk)]
                     (= (map play-query (repeat bulk) queries)
                        (map (fn [{:keys [from to]}]
                               (query-intervals (zipper itree) from to))
                             queries))))))))

(def operation-gen
  (g/tuple (g/one-of [(g/return [play-type-in type-in])
                      (g/return [play-delete-range delete-range])])
           (g/tuple (g/large-integer* {:min 0 :max 10000000})
                    (g/large-integer* {:min 0 :max 10000000}))))

(deftest type-and-delete-test
  (is (:result
       (tc/quick-check
        1000
        (prop/for-all
         [bulk intervals-bulk-gen
          ops (g/vector operation-gen)]
         (let [[tree model] (reduce (fn [[tree model] [[play real] args]]
                                      [(apply real tree args)
                                       (play model args)])
                                    [(bulk->tree bulk) bulk]
                                    ops)]
           (= (set (drop-dead-markers (tree->intervals tree)))
              (set (drop-dead-markers model)))))))))

(deftest gc-test
  (is
   (:result
    (tc/quick-check
     100
     (prop/for-all [[bulk i] (g/bind intervals-bulk-gen
                                     (fn [b]
                                       (g/tuple (g/return b) (g/choose 0 (count b))))
                                     )]
                   (let [tree (bulk->tree bulk)
                         to-delete (i/int-set (range i))
                         tree' (gc tree to-delete)]
                     (= (tree->intervals tree')
                        (remove (fn [m] (to-delete (-> m (.-attrs) (.-id)))) bulk))
                     ))))))



