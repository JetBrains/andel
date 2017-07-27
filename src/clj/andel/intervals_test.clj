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

(deftest insert-right
  (is (= (-> (make-empty-interval-tree)
             (add-interval {:from 5 :to 10})
             (add-interval {:from 17 :to 20})
             (add-interval {:from 11 :to 13})
            (tree->from-to))
         [[5 10] [11 13] [17 20] [plus-infinity plus-infinity]])))

(run-tests)

