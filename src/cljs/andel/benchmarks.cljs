(ns andel.benchmarks
  (:require [andel.text :as text]
            [andel.intervals :as intervals]
            [andel.tree :as tree]))


;; benchmarks

(defn current-time! []
  (.now js/Date))

(defn text-tree-info [t]
  (loop [acc {:nodes 0 :leafs 0}
         loc (text/zipper t)]
    (if (tree/end? loc)
      (js/console.log (str "TEXT: " acc))
      (if (tree/node? (tree/node loc))
        (recur (update acc :nodes inc) (tree/next loc))
        (recur (update acc :leafs inc) (tree/next loc))))))

(defn intervals-tree-info [t]
  (loop [acc {:nodes 0 :leafs 0}
         loc (intervals/zipper t)]
    (if (tree/end? loc)
      (js/console.log (str "INTERVALS: " acc))
      (if (tree/node? (tree/node loc))
        (recur (update acc :nodes inc) (tree/next loc))
        (recur (update acc :leafs inc) (tree/next loc))))))

(defn bench [name f & {:keys [count] :or {count 10}}]
  (let [start-time (current-time!)]
    (js/console.log (str "START BENCH " name))
    (mapv (fn [f] (f)) (repeat count f))
    (let [end-time (current-time!)
          total-time (- end-time start-time)]
      (js/console.log (str "END BENCH: " name " "
                            {:count count
                             :total total-time
                             :average (/ total-time count)})))))

(defn bench-insert [markup]
  (bench "TREE INSERT"
         (fn []
           (-> (intervals/make-interval-tree)
               (intervals/add-markers markup)))
         :count 1))

(defn bench-insert-base [markup]
  (bench "BASE INSERT"
   (fn []
     (mapv (fn [m] (update m :from inc)) markup))
   :count 100))

(defn bench-query [markup]
  (let [itree (-> (intervals/make-interval-tree)
                  (intervals/add-markers markup))]
    (bench "TREE QUERY"
           (fn []
             (let [from (rand-int 160000)
                   to (+ from 3200)]
               (intervals/query-intervals itree from to)))
           :count 10000)))

(defn play-query [model {:keys [from to]}]
  (vec (filter (fn [m] (intervals/intersects? (.-from m) (.-to m) from to)) model)))

(defn bench-query-base [markup]
  (bench "QUERY BASE"
         (fn []
           (let [from (rand-int 160000)
                 to (+ from 3200)]
             (play-query markup {:from from :to to})))
         :count 1000))

(defn bench-type-in [markup]
  (let [itree (-> (intervals/make-interval-tree)
                  (intervals/add-markers markup))]
    (bench "TYPE-IN BENCH"
           (fn []
             (let [offset (rand-int 160000)
                   size 1]
               (intervals/type-in itree [offset size])))
           :count 1000)))

(defn bench-delete [markup]
  (let [itree (-> (intervals/make-interval-tree)
                  (intervals/add-markers markup))]
    (bench "DELETE BENCH"
           (fn []
             (let [offset (rand-int 160000)
                   size 1]
               (intervals/delete-range itree [offset size])))
           :count 1)))

(defn bench-editing [markup]
  (let [itree (-> (intervals/make-interval-tree)
                  (intervals/add-markers markup))]
    (bench "TREE EDITING"
           (fn []
             (let [cmd (rand-nth [:insert :delete])]
               (case cmd
                 :insert ))))))

#_(bind-function! "ctrl-b" (fn [s]
                           (let [markup (:raw-markers s)
                                 interval-tree (get-in s [:document :markup])
                                 text-tree (get-in s [:document :text])]
                             #_(text-tree-info text-tree)
                             (intervals-tree-info interval-tree)
                             #_(bench-insert markup)
                             #_(bench-insert-base markup)
                             #_(bench-query markup)
                             #_(bench-query-base markup)
                             #_(bench-type-in markup)
                             #_(bench-delete markup))
                           (js/alert "BENCH DONE")
                           s))
