(ns andel.text
  (:require [andel.tree :as tree])
  (:require [andel.array-list :as al])
  #?(:clj (:import [andel.tree ZipperLocation Leaf]
                   [java.lang CharSequence])))

#?(:clj
   (do
     (defn array [& args] (long-array args))))

#?(:clj
   (defn metrics [^String x]
     (let [l (.length x)]
       (long-array [l
                    (loop [i 0
                           c 0]
                      (if (= i l)
                        c
                        (if (= (.charAt x i) \newline)
                          (recur (inc i) (inc c))
                          (recur (inc i) c))))])))
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
#?(:clj
   (defn r-f
     ([] (long-array [0 0]))
     ([^"[J" acc ^"[J" metrics]
      (doto (long-array 2)
        (aset 0 (+ (aget acc 0) (aget metrics 0)))
        (aset 1 (+ (aget acc 1) (aget metrics 1))))))
   :cljs
   (defn r-f
     ([] (array 0 0))
     ([[x1 x2 :as acc] [y1 y2]]
      (array (+ x1 y1)
             (+ x2 y2)))))

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

(def tree-config {:make-node (fn [children]
                               (tree/->Node (reduce (fn [acc x] (r-f acc (tree/metrics x)))
                                                    (r-f) children)
                                            children))
                  :reducing-fn r-f
                  :metrics-fn metrics
                  :leaf-overflown? (fn [x] (<= string-thresh (count x)))
                  :split-thresh 32
                  :split-leaf split-string
                  :leaf-underflown? (fn [s] (< (count s) string-merge-thresh))
                  :merge-leafs (fn [s1 s2] (str s1 s2))})

(defn make-text [s]
  (-> (tree/zipper (tree/make-node (al/into-array-list [(tree/make-leaf s tree-config)]) tree-config) tree-config)
      (tree/down)
      (tree/mark-changed)
      (tree/root)))

(defn zipper [tree]
  (tree/zipper tree tree-config))

(def root tree/root)

(defn metrics-offset [^"[J" m]
  (some-> m (aget 0)))

(defn node-offset
  "Returns offset of the current node ignoring overriding accumulator"
  [loc]
  (metrics-offset (tree/acc loc)))

(defn metrics-line [^"[J" m]
  (some-> m (aget 1)))

(defn by-offset [i]
  (fn [acc m] (<= i (metrics-offset (r-f acc m)))))

(defn by-offset-exclusive [i]
  (fn [acc m] (< i (metrics-offset (r-f acc m)))))

(defn by-line [i]
  (fn [acc m] (<= i (metrics-line (r-f acc m)))))

(defn offset [^ZipperLocation loc]
  (if (tree/end? loc)
    (metrics-offset (tree/metrics (tree/node loc)))
    (or (metrics-offset (.-o-acc loc))
        (metrics-offset (.-acc loc))
        0)))

(defn line [^ZipperLocation loc]
  (if (tree/end? loc)
    (metrics-line (tree/metrics (tree/node loc)))
    (or (metrics-line (.-o-acc loc)) (metrics-line (.-acc loc)) 0)))

(defn node-line
  "Returns offset of the current node ignoring overriding accumulator"
  [loc]
  (metrics-line (tree/acc loc)))

(defn count-of [s c from to]
  (loop [res 0
         from from]
    (let [i (clojure.string/index-of s c from)]
      (if (and (some? i) (< i to))
        (recur (inc res) (inc i))
        res))))

(defn nth-index [s c n]
  (if (= n 0)
    0
    (loop [from 0
           n n]
      (let [i (clojure.string/index-of s c from)]
        (if (= n 1)
          i
          (when (some? i)
            (recur (inc i) (dec n))))))))

(defn forget-acc [loc]
  (tree/assoc-o-acc loc nil))

(defn- at-the-right-border? [loc]
  (let [s (.-data ^Leaf (tree/node loc))
        o (offset loc)
        loc-offset (metrics-offset (tree/loc-acc loc))
        rel-offset (- o loc-offset)]
    (= rel-offset (count s))))

(defn scan-to-offset [loc i]
  (let [offset-loc (tree/scan loc (by-offset i))]
    (if (tree/end? offset-loc)
      offset-loc
      (let [o (node-offset offset-loc)
            s (subs (.-data ^Leaf (tree/node offset-loc)) 0 (- i o))
            a (.-acc ^ZipperLocation offset-loc)
            offset-loc (tree/assoc-o-acc offset-loc (r-f a (metrics s)))
            next-node (tree/next offset-loc)]
        (if (and (at-the-right-border? offset-loc)
                 (not (tree/end? next-node)))
          next-node
          offset-loc)))))

(defn retain [loc l]
  (scan-to-offset loc (+ (offset loc) l)))

(defn scan-to-line-start [loc n]
  (let [nth-eol-loc (tree/scan loc (by-line n))]
    (if (or (tree/end? nth-eol-loc) (<= n 0))
      nth-eol-loc
      (let [o (node-offset nth-eol-loc)
            l (node-line nth-eol-loc)
            eol-idx (nth-index (.-data ^Leaf (tree/node nth-eol-loc))
                               \newline
                               (- n l))
            s (subs (.-data ^Leaf (tree/node nth-eol-loc)) 0 eol-idx)
            a (.-acc ^ZipperLocation nth-eol-loc)
            prev-line-end (tree/assoc-o-acc nth-eol-loc (r-f a (metrics s)))]
        (-> prev-line-end
            (retain 1))))))

(defn distance-to-EOL [loc]
  (let [next-loc (scan-to-line-start loc (inc (line loc)))
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
              text (.-data ^Leaf (tree/node loc))
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
              (tree/metrics)
              (metrics-line)
              (inc))
      0))

(defn text-length [t]
  (or (some-> t
              (tree/metrics)
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
          (tree/edit (fn [^Leaf node]
                       (let [data (or (some-> node (.-data)) "")]
                         (tree/make-leaf (str (subs data 0 rel-offset) s (subs data rel-offset)) tree-config))))
          (retain (count s))))))

(defn delete [loc l]
  (if (tree/branch? loc)
    (recur (tree/down loc) l)
    (let [i (offset loc)
          chunk-offset (metrics-offset (tree/loc-acc loc))
          rel-offset (- i chunk-offset)
          chunk-l (count (.-data ^Leaf (tree/node loc)))
          end (min chunk-l (+ rel-offset l))
          next-loc  (if (and (= rel-offset 0) (= end chunk-l) (not (tree/last-root-child? loc)))
                      (tree/remove (forget-acc loc))
                      (-> loc
                          (tree/edit (fn [node]
                                       (let [s (.-data ^Leaf node)]
                                         (tree/make-leaf (str (subs s 0 rel-offset) (subs s end)) tree-config))))
                          (scan-to-offset i)))
          deleted-c (- end rel-offset)]
      (if (< deleted-c l)
        (recur next-loc (- l deleted-c))
        next-loc))))

(def reset tree/reset)

(defn play [t operation]
  (root (reduce (fn [loc [code arg]]
                  (case code
                    :retain (retain loc arg)
                    :insert (insert loc arg)
                    :delete (delete loc (if (string? arg) (count arg) arg))))
                (zipper t)
                operation)))


(defn line-text [t i]
  (let [loc (scan-to-line-start (zipper t) i)]
    (text loc (distance-to-EOL loc))))


(defn leaf->text [loc]
  {:base (metrics-offset (tree/loc-acc loc))
   :text (.-data ^Leaf (tree/node loc))})

(defn scan-by-offset-exclusive [loc i]
  (tree/scan loc (by-offset-exclusive i)))

#?(:clj
    (deftype TextSequence [t ^{:volatile-mutable true} loc from to]
      CharSequence
      (^int length [this]
            (- to from))
      (^char charAt [this ^int index]
             (assert (< index (- to from)) "Index out of range")
             (let [^int absolute-index (+ index from)
                   {:keys [^int base ^String text]} (leaf->text loc)]
               (cond
                 (< absolute-index base)
                 (let [new-loc (scan-by-offset-exclusive (zipper t) absolute-index)
                       {:keys [^int base ^String text]} (leaf->text new-loc)]
                   (set! loc new-loc)
                   (.charAt text (- absolute-index base)))

                 (< absolute-index (+ base (count text)))
                 (.charAt text (- absolute-index base))

                 (<= (+ base (count text)) absolute-index)
                 (let [new-loc (scan-by-offset-exclusive loc absolute-index)
                       {:keys [base ^String text]} (leaf->text new-loc)]
                   (set! loc new-loc)
                   (.charAt text (- absolute-index base)))

                 :else (assert "No way"))))
      (^CharSequence subSequence [this ^int from' ^int to']
                     (assert (< from' (- to from)) "From index out of range")
                     (assert (<= to' (- to from)) "To index out of range")
                     (TextSequence. t (scan-by-offset-exclusive (zipper t) 0) (+ from from') (+ from to')))
      (^String toString [this] (text (scan-to-offset (zipper t) from) (- to from)))))

#?(:clj
    (defn ^CharSequence text->char-seq [t]
      (TextSequence. t (scan-by-offset-exclusive (zipper t) 0) 0 (text-length t))))
