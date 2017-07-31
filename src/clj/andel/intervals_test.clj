(ns andel.intervals-test
  (:require [andel.intervals :refer :all]
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
  (let [intervals (sort-by :from  [{:from 1 :to 10}
                                   {:from 9 :to 11}
                                   {:from 8 :to 12}
                                   {:from 3 :to 17}
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
    (is (= [{:from 8 :to 18}] (query-intervals itree 5 8)))
    (is (= [{:from 21 :to 30}] (query-intervals itree 19 22)))
    (is (= [{:from 8 :to 18} {:from 21 :to 30}] (query-intervals itree 10 25)))
    (is (= [{:from 8 :to 18} {:from 21 :to 30}] (query-intervals itree 2 35)))
    (is (= [] (query-intervals itree 35 50)))))

(comment 
  (run-tests)

  (clojure.test/test-vars [#'andel.intervals-test/simple-insert]))

