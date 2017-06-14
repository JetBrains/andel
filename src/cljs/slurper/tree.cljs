(ns slurper.tree
  (:require [clojure.zip :as z])
  (:refer-clojure :exclude (replace remove next)))

(defrecord Node [metrics children])

(defn make-node [children {::keys [reducing-fn pack-children-fn metrics-fn]}]
  (Node. (if (string? children)
           (metrics-fn children) 
           (reduce (fn [acc x] (reducing-fn acc (metrics-fn x))) (reducing-fn) children))
         (pack-children-fn children)))
 

(defn ^boolean node? [x]
  (instance? Node x))

(defn zipper [tree {::keys [reducing-fn metrics-fn pack-children-fn]}]
  (let [config {::pack-children-fn pack-children-fn
                ::reducing-fn reducing-fn
                ::metrics-fn (fn [x]
                               (if (node? x)
                                 (:metrics x)
                                 (metrics-fn x)))}]
    (vary-meta (z/zipper node?
                         :children
                         (fn [node children] (make-node children config))
                         tree)               
               merge config)))
 
(def split-thresh 4)

(defn fast-split [i s]
  (cond
    (string? s)
    [(subs s 0 i)
     (subs s i)]
    (vector? s)
    [(subvec s 0 i)
     (subvec s i)]
    :else (split-at i s)))

(defn partition-binary [s thresh]
  (let [cs (count s)]
    (if (< cs thresh)
      [s]
      (let [[left right] (fast-split (quot cs 2) s)]
        (concat (partition-binary left thresh)
                (partition-binary right thresh))))))

(defn split-node [node config]
  (let [children (:children node)]
    (if (< (count children) split-thresh)
      [node]
      (map #(make-node % config)
           (partition-binary children split-thresh)))))

(defn split-needed? [children]
  (reduce (fn [_ c] (if (<= split-thresh (count (:children c)))
                     (reduced true)
                     false)) false children))

(defn split-children [children config]
  (if (split-needed? children)
    (loop [[node & rest-children] children
           result (transient [])]
      (if node
        (let [node-c (:children node)]
          (if (< (count node-c) split-thresh)
            (recur rest-children (conj! result node))
            (recur rest-children (reduce conj! result (map #(make-node % config)
                                                           (partition-binary node-c split-thresh))))))
        (persistent! result)))
    children))

(defn loc-acc [[_ {::keys [acc]} :as loc]]
  (or acc
      ((::reducing-fn (meta loc)))))

(defn merge-needed? [children merge-thresh]
  (reduce (fn [_ c] (if (< (count (:children c)) merge-thresh)
                     (reduced true)
                     false)) false children))

(defn merge-children [children config]
  (let [merge-thresh (quot split-thresh 2)]
    (if (merge-needed? children merge-thresh)
      (let [[result last] (reduce
                           (fn [[result left] right]
                             (let [left-children (:children left)
                                   right-children (:children right)
                                   left-c (count left-children)
                                   right-c (count right-children)]
                               (if (or (< left-c merge-thresh) (< right-c merge-thresh))
                                 (if (<= split-thresh (+ left-c right-c))
                                   (let [[children-left children-right]
                                         (partition-binary (concat left-children right-children) split-thresh)]
                                     [(conj! result (make-node children-left config))
                                      (make-node children-right config)])
                                   [result (make-node (concat left-children right-children) config)])
                                 [(conj! result left) right])))
                           [(transient []) (first children)]
                           (drop 1 children))]
        (persistent! (conj! result last)))
      children)))

(defn balance-children [children config]
  (if (node? (first children))
    (let [children' (split-children children config)]
      (if (< (reduce + (map (comp count :children) children')) split-thresh)
        (mapcat :children children')
        (merge-children children' config)))
    children))

(defn grow-tree [children config]
  (let [balanced-children (balance-children children config)]
    (if (< (count balanced-children) split-thresh)
      (make-node balanced-children config)
      (recur [(make-node balanced-children config)] config))))

(defn up [[node {:keys [changed?] :as path} :as loc]]
  (if changed?
    (let [config (meta loc)]
      (if-let [parent (z/up loc)]
        (z/replace parent (grow-tree (z/children parent) config))
        (with-meta [(grow-tree [node] config) nil] (meta loc))))
    (z/up loc)))

(defn right [[node {::keys [acc]} :as loc]]
  (when-let [r (z/right loc)]
    (let [{::keys [reducing-fn metrics-fn]} (meta loc)]
      (assoc-in r [1 ::acc] (if (some? acc)
                              (reducing-fn acc (metrics-fn node))
                              (metrics-fn node))))))

(defn down [[_ {::keys [acc]} :as loc]]
  (some-> (z/down loc)
          (assoc-in [1 ::acc] acc)))

(defn root
  "Modified version of clojure.zip/root to work with balancing version of up"
  [loc]
  (if (= :end (loc 1))
    (z/node loc)
    (let [p (up loc)]
      (if p
        (recur p)
        (z/node loc)))))

(defn next
  "Modified version of clojure.zip/next to work with balancing version of up"
  [loc]
  (if (= :end (loc 1))
    loc
    (or 
     (and (z/branch? loc) (down loc))
     (right loc)
     (loop [p loc]
       (if-let [u (up p)]
         (or (right u) (recur u))
         [(z/node p) :end])))))

(defn skip
  "Modified version of clojure.zip/next to work with balancing version of up"
  [loc]
  (if (= :end (loc 1))
    loc
    (or 
     (right loc)
     (loop [p loc]
       (if-let [u (up p)]
         (or (right u) (recur u))
         [(z/node p) :end])))))

(def insert-right z/insert-right)
(def remove z/remove)
(def children z/children)
(def branch? z/branch?)
(def node z/node)
(def end? z/end?)
(def edit z/edit)

(defn reset [loc]
  (zipper (root loc)
          (meta loc)))

(defn jump-down [[pnode _ :as loc] i]
  (let [[node {acc ::acc :as path}] (down loc)
        {::keys [reducing-fn metrics-fn]} (meta loc)
        [lefts rights] (fast-split i (:children pnode))
        acc (or acc (reducing-fn))]
    (with-meta [(nth (:children pnode) i)
                (assoc path
                       ::acc (if (string? lefts)
                               (reducing-fn acc (metrics-fn lefts))
                               (reduce reducing-fn acc lefts))
                       :l (vec (seq lefts))
                       :r (drop 1 rights))]
      (meta loc))))

(defn scan [[node {rights :r lefts :l acc ::acc :as path} :as loc] pred]
  (if (end? loc)
    loc
    (let [{::keys [reducing-fn metrics-fn]} (meta loc)
          next-loc (if (nil? lefts)
                     loc
                     (loop [l (transient lefts)
                            [n & r] (cons node rights)
                            acc (or acc (reducing-fn))]
                       (when (some? n)
                         (let [acc' (reducing-fn acc (metrics-fn n))]
                           (if (pred acc')
                             (with-meta [n (assoc path
                                                  :l (persistent! l)
                                                  :r r
                                                  ::acc acc)] (meta loc))
                             (recur (conj! l n)
                                    r
                                    acc'))))))]
      (if (some? next-loc)
        (if (z/branch? next-loc)
          (recur (down next-loc) pred)
          next-loc)
        (recur (skip loc) pred)))))

(defn till-offset [i]
  #(< i (nth % 0)))

(defn insert-left [loc x]
  (let [{::keys [reducing-fn metrics-fn]} (meta loc)]
    (-> loc
        (z/insert-left x)
        (update-in [1 ::acc] reducing-fn (metrics-fn x)))))


(defn metrics [x]
  (cond
    (string? x) (let [l (.-length x)]
                  [l (loop [i 0
                            c 0]
                       (if (identical? i l)
                         c
                         (if (identical? (.charAt x i) \newline)
                           (recur (inc i) (inc c))
                           (recur (inc i) c))))])
    (char? x) [1 (if (= x \newline) 1 0)]
    :else (:metrics x)))

(defn pack-children [[c :as new-children]]
   (if (char? c)
     (apply str new-children)
     (vec new-children)))

(defn r-f
  ([] (transient [0 0]))
  ([[x1 x2 :as acc] [y1 y2]]
   (-> acc
       (assoc! 0 (+ x1 y1))
       (assoc! 1 (+ x2 y2)))))


(def config1 {::reducing-fn r-f
              ::metrics-fn metrics
              ::pack-children-fn pack-children})

(def t1
  (make-node
   [(make-node "foo1" config1)
    (make-node "bar1" config1)
    (make-node "baz1" config1)] config1))

(defn insert-string [loc s]
  (reduce (fn [l c]
            (-> l
                (z/insert-right c)
                (right)))
          loc s))

(comment
  (def editor-impl (slurp (clojure.java.io/file "/Users/jetzajac/Projects/ultimate/community/platform/platform-impl/src/com/intellij/openapi/editor/impl/EditorImpl.java")))

  (do
    (time
     (partition-binary editor-impl split-thresh))
    0)

  (do
    (time (make-node editor-impl config1))
    0)

  (do
    (time
     (doall (pack-children editor-impl)))
    0
    )

  (def t2 (-> (zipper (make-node editor-impl config1)
                      config1)
              (assoc-in [1 :changed?] true)
              (root)))

  
  (time
   (-> (zipper t2 config1)
       (scan (till-offset 50000))
       (insert-string "huj")
       (root))
   0
   )
  

  *make-node*
  *make-node-args*
  

  (-> (zipper t1 config1)
      (down)
      (right))

  (-> (zipper t1 config1)
      (scan (till-offset 2))
      (insert-string "abcd")
      
      ;(insert-string "abcd")
      ;(scan (till-offset 14))
      #_(z/remove)
      #_(z/remove)
      #_(z/remove)
      (root))

  (-> (zipper (make-node "sdfkljnasklfjnasdkljfnasjkdfnjasnflnsadlkfjnsadjfnlsadkfnjasdnflksdnfjsnflkjsdnjfnsdakljfnsajkfnsjnfsnfjklsdnfkljsanfjsdnfknsadklfn" config1)
              config1)
      (assoc-in [1 :changed?] true)
      (scan (till-offset 0))
      (insert-string "hujsd lfjnasdjkfn alskfjsdnjksdnfjsfklsdnjklfnlsdfjknsadjfsljfn sdjfknasdfjdsnflsdanfjsdnfsdljfnsdajfnsadfjkdnsals kdjnfjklsadnfsajldfnldsnjlkfnslkdnjdlfnjkdnsflksd nksdnfljksdnfjksadnlkjdnfsdjknkads fnsalkans")
      (insert-string "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111")
      (insert-string "a")
      (remove)
      (root))
      

  **path

  
  (scan (till-offset 1))
  (up)
  (up)
  (scan (till-offset 3))
  (z/remove)
  (z/remove)
  (z/remove)
  (root)
  

  children'
  
  (-> (zipper t1 r-f)
      (scan (till-offset 4))
      (z/remove)
      (z/remove)
      (scan (till-offset 5))
      (z/remove)
      (scan (till-offset 6))
      (z/remove)
      
                                        ;(z/insert-right "\n\n\nnxyuu123")
                                        ;(z/insert-right "poipoipoipoi")

      #_(scan (till-offset 8))
      
      #_(z/insert-left "\nxyztasdsdfsdf")
      (root)
      ))
