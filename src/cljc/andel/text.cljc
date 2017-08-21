(ns andel.text
  (:require [andel.tree :as tree]
            [andel.fast-zip :as fz]))

#?(:clj
   (do
     (defn array [& args] (object-array args))
     (def some-array (array 1 2 3))
     (defn array? [x]
       (= (type some-array) (type x)))))

#?(:clj
   (defn metrics [x]
     (assert (string? x))
     (let [l (count x)]
       (array l
              (loop [i 0
                     c 0]
                (if (= i l)
                  c
                  (if (= (.charAt x i) \newline)
                    (recur (inc i) (inc c))
                    (recur (inc i) c)))))))
   :cljs
   (defn metrics [x]
     (assert (string? x))
     (let [l (.-length x)]
       (array l
              (loop [i 0
                     c 0]
                (if (identical? i l)
                  c
                  (if (identical? (.charAt x i) \newline)
                    (recur (inc i) (inc c))
                    (recur (inc i) c))))))))

(defn r-f
  ([] (array 0 0))
  ([[x1 x2 :as acc] [y1 y2]]
   (array (+ x1 y1)
          (+ x2 y2))))

(defn split-count [i j thresh]
  (let [x (- j i)]
    (if (< x thresh)
      [[i j]]
      (let [x-h (quot x 2)]
        (concat (split-count i (+ i x-h) thresh) (split-count (+ i x-h) j thresh))))))

(def string-thresh 64)
(def string-merge-thresh (quot string-thresh 2))

(defn split-string [x]
  (assert (<= string-thresh (count x)))
  (map (fn [[i j]] (subs x i j)) (split-count 0 (count x) string-thresh)))

(def tree-config {:reducing-fn r-f
                  :metrics-fn metrics
                  :leaf-overflown? (fn [x] (<= string-thresh (count x)))
                  :split-thresh 64
                  :split-leaf split-string
                  :leaf-underflown? (fn [s] (< (count s) string-merge-thresh))
                  :merge-leafs (fn [s1 s2] (str s1 s2))})

(defn mark-changed [loc]
  (update loc :path assoc :changed? true))

(defn make-text [s]
  (-> (tree/zipper (tree/make-node [(tree/make-leaf s tree-config)] tree-config) tree-config)
      (tree/down)
      (mark-changed)
      (tree/root)))

(defn zipper [tree]
  (tree/zipper tree tree-config))

(def root tree/root)

(defn metrics-offset [m]
  (some-> m (aget 0)))

(defn metrics-line [m]
  (some-> m (aget 1)))

(defn by-offset [i]
  (fn [acc m] (< i (metrics-offset (r-f acc m)))))

(defn by-line [i]
  (fn [acc m] (<= i (metrics-line (r-f acc m)))))


(defn offset [loc]
  (let [node (.-node loc)
        path (.-path loc)
        acc (some-> path .-acc)
        o-acc (.-o-acc path)]
    (if (tree/end? loc)
      (metrics-offset (.-metrics node))
      (or (metrics-offset o-acc) (metrics-offset acc) 0))))

(defn line [loc]
  (let [node (.-node loc)
        path (.-path loc)
        acc (some-> path .-acc)
        o-acc (.-o-acc path)]
    (if (tree/end? loc)
      (metrics-line (.-metrics node))
      (or (metrics-line o-acc) (metrics-line acc) 0))))

(defn count-of [s c from to]
  (loop [res 0
         from from]
    (let [i (clojure.string/index-of s c from)]
      (if (and (some? i) (< i to))
        (recur (inc res) (inc i))
        res))))

(defn nth-index [s c n]
  (if (identical? n 0)
    0
    (loop [from 0
           n n]
      (let [i (clojure.string/index-of s c from)]
        (if (identical? n 1)
          i
          (when (some? i)
            (recur (inc i) (dec n))))))))

(defn forget-acc [loc]
  (fz/update-path loc #(fz/assoc-o-acc % nil)))

(defn scan-to-offset [loc i]
  (let [loc' (tree/scan loc (by-offset i))]
    (if (tree/end? loc')
      loc'
      (let [loc' (forget-acc loc')
            o (offset loc')
            l (line loc')]
        (fz/update-path loc'
                        #(fz/assoc-o-acc % (array i (+ l (count-of (.-data (tree/node loc')) \newline 0 (- i o))))))))))

(defn retain [loc l]
  (scan-to-offset loc (+ (offset loc) l)))

(defn scan-to-line [loc i]
  (let [loc' (tree/scan loc (by-line i))]
    (if (tree/end? loc')
      loc'
      (let [loc' (forget-acc loc')
            o (offset loc')
            l (line loc')
            idx (nth-index (.-data (tree/node loc')) \newline (- i l))]
        (-> loc'
            (fz/update-path #(fz/assoc-o-acc % (array (+ o idx) i)))
            (cond-> (< 0 i) (retain 1)))))))

(defn line-length [loc]
  (let [next-loc (scan-to-line loc (inc (line loc)))
        len (- (offset next-loc)
               (offset loc))]
    (if (tree/end? next-loc)
      len
      (dec len))))

(defn lazy-text [loc l]
  (when (< 0 l)
    (if (tree/end? loc)
      (throw (ex-info "Length is out of bounds" {:l l}))
      (if (tree/branch? loc)
        (recur (tree/down loc) l)
        (let [i (offset loc)
              text (.-data (tree/node loc))
              base-offset (metrics-offset (tree/loc-acc loc))
              start (- i base-offset)
              end (min (count text) (+ start l))
              s (subs text start end)
              s-len (count s)]
          (if (< s-len l)
            (cons s (lazy-seq (lazy-text (tree/next (forget-acc loc)) (- l s-len))))
            (list s)))))))

(defn lines-count [t]
  (or (some-> t
              (.-metrics)
              (metrics-line)
              (inc))
      0))

(defn text-length [t]
  (or (some-> t
              (.-metrics)
              (metrics-offset))
      0))

(defn text [loc l]
  (loop [s ""
         lt (lazy-text loc l)]
    (if-let [f (first lt)]
      (recur (str s f) (rest lt))
      s)))

(defn insert [loc s]
  (if (tree/branch? loc)
    (recur (tree/down loc) s)
    (let [i (offset loc)
          chunk-offset (metrics-offset (tree/loc-acc loc))
          rel-offset (- i chunk-offset)]
      (-> loc
          (tree/edit (fn [node]
                       (let [data (.-data node)]
                         (tree/make-leaf (str (subs data 0 rel-offset) s (subs data rel-offset)) tree-config))))
          (retain (count s))))))

(defn delete [loc l]
  (if (tree/branch? loc)
    (recur (tree/down loc) l)
    (let [i (offset loc)
          chunk-offset (metrics-offset (tree/loc-acc loc))
          rel-offset (- i chunk-offset)
          chunk-l (count (.-data (tree/node loc)))
          end (min chunk-l (+ rel-offset l))
          next-loc  (if (and (= rel-offset 0) (= end chunk-l))
                      (tree/remove (forget-acc loc))
                      (-> loc
                          (tree/edit (fn [node]
                                       (let [s (.-data node)]
                                         (tree/make-leaf (str (subs s 0 rel-offset) (subs s end)) tree-config))))
                          (scan-to-offset i)))
          deleted-c (- end rel-offset)]
      (if (< deleted-c l)
        (recur next-loc (- l deleted-c))
        next-loc))))

(def reset tree/reset)

(defn debug-tree [t]
  (if (array? (.-children t))
    (assoc t :children (vec (map debug-tree (.-children t))))
    t))

(defn play [t operation]
  (root (reduce (fn [loc [code arg]]
                  (case code
                    :retain (retain loc arg)
                    :insert (insert loc arg)
                    :delete (delete loc (if (string? arg) (count arg) arg))))
                (zipper t)
                operation)))


(defn line-text [t i]
  (let [loc (scan-to-line (zipper t) i)]
    (text loc (line-length loc))))
