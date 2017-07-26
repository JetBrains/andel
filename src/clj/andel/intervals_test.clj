(ns andel.intervals-test
  (:require [andel.intervals :refer :all]
            [clojure.test :refer :all]))

(def interval {:from 2 :to 3})

(-> [[1 3] [20 24]]
    from-to->tree
    (get-insert-loc interval)
    (insert-in interval)
    tree->from-to)

(deftest create-and-destruct-tree
  (is (let [ranges [[1 2] [4 8] [16 32]]]
        (= ranges
           (-> ranges
               from-to->tree
               tree->from-to)))))

(run-all-tests)