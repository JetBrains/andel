(ns andel.intervals-test
  (:require [andel.intervals :refer :all]
            [clojure.test :refer :all]))

(deftest create-and-destruct-tree
  (is (let [ranges [[5 10] [11 13] [17 20]]]
        (= ranges
           (-> ranges
               from-to->tree
               tree->from-to)))))

(deftest simple-insert
  (is (= (-> (make-empty-interval-tree)
             (add-interval {:from 5 :to 10})
             (tree->from-to))
         [[5 10] [plus-infinity plus-infinity]])))

(deftest multiple-insertitions
  (is (= (-> (make-empty-interval-tree)
             (add-interval {:from 5 :to 10})
             (add-interval {:from 17 :to 20})
             (add-interval {:from 11 :to 13})
             (tree->from-to))
         [[5 10] [11 13] [17 20] [plus-infinity plus-infinity]]))
  (is (= (-> (make-empty-interval-tree)
             (add-interval {:from 1 :to 10})
             (add-interval {:from 9 :to 11})
;             (add-interval {:from 8 :to 12})
;             (add-interval {:from 3 :to 17})
;             (add-interval {:from 2 :to 18})
;             (add-interval {:from 1 :to 19})
;             (add-interval {:from 7 :to 13})
;             (add-interval {:from 6 :to 14})
;             (add-interval {:from 5 :to 15})
;             (add-interval {:from 4 :to 16})
             (tree->from-to))
         [[0 10] [1 19] [2 18] [3 17] [4 16] [5 15] [6 14] [7 13] [8 12] [9 11] [plus-infinity plus-infinity]])))

(deftest insert-with-overlaping
  (is (= (with-out-str
           (-> (make-empty-interval-tree)
             (add-interval {:from 1 :to 10})
             (add-interval {:from 9 :to 11})
             (tree->from-to)))
         [[1 10] [9 11]])))


(deftest insert-at-same-pos
  (is (= (-> (make-empty-interval-tree)
             (add-interval {:from 10 :to 12})
             (add-interval {:from 10 :to 11})
             (tree->from-to))
      [[10 11] [10 12] [plus-infinity plus-infinity]]))
  (is (= (-> (make-empty-interval-tree)
             (add-interval {:from 10 :to 12})
             (add-interval {:from 10 :to 11})
             (add-interval {:from 10 :to 13})
             (add-interval {:from 10 :to 18})
             (add-interval {:from 10 :to 15})
             (add-interval {:from 10 :to 17})
             (tree->from-to))
         ;; reversed to addition order
         [[10 17] [10 15] [10 18] [10 13] [10 11] [10 12] [plus-infinity plus-infinity]])))

(deftest marker-query-test
  (let [itree (-> (make-empty-interval-tree)
                  (add-interval {:from 8 :to 18})
                  (add-interval {:from 21 :to 30}))]
    (is (= (query-markers itree 0 5) []))
    (is (= (query-markers itree 5 8) [{:from 8 :to 18}]))
    (is (= (query-markers itree 19 22) [{:from 21 :to 30}]))
    (is (= (query-markers itree 10 25) [{:from 8 :to 18} {:from 21 :to 30}]))
    (is (= (query-markers itree 2 35) [{:from 8 :to 18} {:from 21 :to 30}]))
    (is (= (query-markers itree 35 50) []))))

(deftest marker-query-test
  (let [itree (-> (make-empty-interval-tree)
;                  (add-interval {:from 0 :to 10})
                  (add-interval {:from 9 :to 11})
                  (add-interval {:from 8 :to 12})
                  (add-interval {:from 3 :to 17})
                  (add-interval {:from 2 :to 18})
                  (add-interval {:from 1 :to 19})
                  (add-interval {:from 7 :to 13})
                  (add-interval {:from 6 :to 14})
                  (add-interval {:from 5 :to 15})
                  (add-interval {:from 4 :to 16}))]
    (query-markers itree 0 3)))

(run-tests)


(comment

  (make-interval-tree)
  
  (let [tree (from-to->tree [[10 20] [50 100] [150 1000]])]
    (-> tree
        (scan-to-offset 25)))
  
  (-> (make-empty-interval-tree)
      (add-interval {:from 5 :to 10})
      (scan-to-offset 3)
      )

  )
