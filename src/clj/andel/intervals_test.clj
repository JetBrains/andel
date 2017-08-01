(ns andel.intervals-test
  (:require [clojure.test.check.generators :as g]
            [clojure.test.check.properties :as prop]
            [clojure.test.check :as tc]
            [andel.intervals :refer :all]
            [clojure.test :refer :all]))

(deftest simple-insert
  (let [itree (-> (make-interval-tree)
                  (add-intervals [{:from 5 :to 10}]))]
    (is (= (tree->intervals itree) [{:from 5 :to 10}]))))

(deftest multiple-insertitions
  (let [intervals [{:from 1 :to 10}
                   {:from 9 :to 11}
                   {:from 8 :to 12}
                   {:from 3 :to 17}
                   {:from 2 :to 18}                 
                   {:from 7 :to 13}
                   {:from 6 :to 14}
                   {:from 5 :to 15}
                   {:from 4 :to 16}]
        itree (reduce add-intervals (make-interval-tree) (map vector intervals))]
    (is (= (tree->intervals itree) (sort-by :from intervals)))))

(deftest bulk-insertitions
  (let [intervals (sort-by :from  [{:from 0 :to 10}
                                   {:from 0 :to 11}
                                   {:from 0 :to 12}
                                   {:from 0 :to 17}
                                   {:from 2 :to 18}                   
                                   {:from 7 :to 13}
                                   {:from 6 :to 14}
                                   {:from 5 :to 15}
                                   {:from 4 :to 16}])
        itree (add-intervals (make-interval-tree) intervals)]
    (is (= (tree->intervals itree) intervals))))

(deftest marker-query-test
  (let [itree (-> (make-interval-tree)
                  (add-intervals [{:from 8 :to 18}])
                  (add-intervals [{:from 21 :to 30}]))]
    (is (= [] (query-intervals itree 1 5)))
    (is (= [] (query-intervals itree 5 8)))
    (is (= [{:from 21 :to 30}] (query-intervals itree 19 22)))
    (is (= [{:from 8 :to 18} {:from 21 :to 30}] (query-intervals itree 10 25)))
    (is (= [{:from 8 :to 18} {:from 21 :to 30}] (query-intervals itree 2 35)))
    (is (= [] (query-intervals itree 35 50)))))


(def intervals-bulk-gen (g/fmap (fn [v] (vec (sort-by :from v)))
                                (g/vector
                                 (g/fmap (fn [[a b]] {:from (min a b) :to (max a b)})
                                         (g/tuple (g/large-integer* {:min 0 :max 10000})
                                                  (g/large-integer* {:min 0 :max 10000})))
                                 0 100)))


(deftest bulk-insertion
  (is (:result (tc/quick-check 1000
                               (prop/for-all [bulk intervals-bulk-gen]
                                             (let [itree (add-intervals (make-interval-tree) bulk)]
                                               (= (tree->intervals itree)
                                                  bulk)))))))

(mapcat vec (nth (g/sample (g/vector intervals-bulk-gen)) 5))

(deftest multiple-bulk-insertion
  (is (:result (tc/quick-check 100
                               (prop/for-all [bulk-bulk (g/vector intervals-bulk-gen)]
                                             (let [itree (reduce add-intervals
                                                                 (make-interval-tree)
                                                                 bulk-bulk)]
                                               (= (sort-by :from (mapcat vec bulk-bulk))
                                                  (tree->intervals itree))))))))

;; [{from to}] -> offset -> size -> [{from to}]
(defn play-type-in [model offset size]
  (->> model
       (map (fn [{:keys [from to] :as interval}]
              (cond
                (and  (<= from offset) (< offset to))
                {:from from :to (+ to size)}
                
                (< offset from)
                {:from (+ from size) :to (+ to size)}
                
                :else
                interval)))
       vec))

(def bulk-offset-size-gen
  (g/bind intervals-bulk-gen
          (fn [bulk] (let [max-val (->> bulk
                                        (map :to)
                                        (apply max 0))]
                       (g/tuple (g/return bulk)
                                (g/large-integer* {:min 0 :max max-val})
                                (g/large-integer* {:min 1 :max 10000}))))))

(deftest single-insertion
  (is (:result (tc/quick-check 1000
                               (prop/for-all [[bulk offset size] bulk-offset-size-gen]
                                             (let [real (-> (make-interval-tree)
                                                            (add-intervals bulk)
                                                            (type-in offset size)
                                                            (tree->intervals))
                                                   naive (play-type-in bulk offset size)]
                                               (= naive real)))))))

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
  (is (:result (tc/quick-check 1000
                               (prop/for-all [[bulk queries] bulk-and-queries-gen]
                                             (= (map play-query (repeat bulk) queries)
                                                (let [generated-tree (-> (make-interval-tree)
                                                                         (add-intervals bulk))]
                                                  (map query-intervals (repeat generated-tree) queries))))))))

(comment 
  (run-tests)

  [[[{:from 0, :to 1}]
    [{:from 0, :to 0}]]]

  (-> [{:from 0 :to 0} {:from 0 :to 1}]
      (play-query {:from 0 :to 1}))
  
  (-> (make-interval-tree)
      (add-intervals [{:from 0 :to 1}])
      (query-intervals 0 0))

  (intersect {:from 0 :to 1} {:from 0 :to 0})
  
  (-> (make-interval-tree)
      (query-intervals 2 2))

  (play-type-in [{:from 0 :to 1}] 0 1)
  
  (clojure.test/test-vars [#'andel.intervals-test/generative]))

