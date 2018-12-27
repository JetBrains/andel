(ns andel.text
  (:require [andel.tree :as tree])
  (:require [andel.array-list :as al])
  #?(:clj (:import [andel.tree ZipperLocation Leaf]
                   [andel Text]
                   [java.lang CharSequence])))

(defn count-codepoints ^long [^String s]
  (.codePointCount s 0 (.length s)))

(defn chars->codepoints [^String s chars-offset]
  (count-codepoints (subs s 0 chars-offset)))

(defn codepoints->chars [^String s codepoints-offset]
  (.offsetByCodePoints s 0 codepoints-offset))

(defrecord TextMetrics [^long length ;; in codepoints
                        ^double geometric-length
                        ^long lines-count
                        ^long chars-count
                        ^double newline-prefix-length
                        ^double max-line-length
                        ^double newline-suffix-length])

(defn geom-metrics-pred [^double to]
  (fn ^long [^long offset ^double geom-offset ^long char-offset]
    (if (<= to geom-offset) 1 0)))

(defn offset-metrics-pred [^long to]
  (fn ^long [^long offset ^double geom-offset ^long char-offset]
    (if (<= to offset) 1 0)))

(defn char-metrics-pred [^long to]
  (fn ^long [^long offset ^double geom-offset ^long char-offset]
    (if (<= to char-offset) 1 0)))

(defn geom-map ^ints [^String str]
  (let [l (.length str)
        m (int-array (inc l))]
    (aset m 0 0)
    (loop [i 0
           o 0]
      (if (< i l)
        (let [c (.charAt str i)
              o' (if (= c \tab) (+ o 4) (inc o))]
          (aset m (inc i) o')
          (recur (inc i) o'))
        m))))

(defn metrics-to [^String str ^clojure.lang.IFn$LDLL pred]
  (let [metrics (Text/metricsTo str pred)]
    (TextMetrics.
      (.-length metrics)
      (.-geometricLength metrics)
      (.-linesCount metrics)
      (.-charsCount metrics)
      (.-newlinePrefixGeomLength metrics)
      (.-maxLineLength metrics)
      (.-newlineSuffixGeomLength metrics))))

(defn metrics [^String str]
  (metrics-to str (offset-metrics-pred (.codePointCount str 0 (.length str)))))

(defn ^TextMetrics scan-r-f
  ([] (TextMetrics. 0 0 0 0 0 0 0))
  ([^TextMetrics acc ^TextMetrics metrics]
   (TextMetrics. (+ (.-length acc) (.-length metrics))
                 (+ (.-geometric-length acc) (.-geometric-length metrics))
                 (+ (.-lines-count acc) (.-lines-count metrics))
                 (+ (.-chars-count acc) (.-chars-count metrics))
                 0
                 0
                 0)))

(defn build-r-f
  ([] (TextMetrics. 0 0 0 0 0 0 0))
  ([^TextMetrics acc ^TextMetrics metrics]
   (let [l-length (.-length acc)
         l-lines-count (.-lines-count acc)
         l-prefix (.-newline-prefix-length acc)
         l-maxlen (.-max-line-length acc)
         l-suffix (.-newline-suffix-length acc)
         r-length (.-length acc)
         r-lines-count (.-lines-count metrics)
         r-prefix (.-newline-prefix-length metrics)
         r-maxlen (.-max-line-length metrics)
         r-suffix (.-newline-suffix-length metrics)]
     (TextMetrics. (+ (.-length acc) (.-length metrics))
                   (+ (.-geometric-length acc) (.-geometric-length metrics))
                   (+ (.-lines-count acc) (.-lines-count metrics))
                   (+ (.-chars-count acc) (.-chars-count metrics))
                   (if (= l-lines-count 0)
                     (+ l-prefix r-prefix)
                     l-prefix)
                   (max l-maxlen
                        r-maxlen
                        (+ l-suffix
                           r-prefix))
                   (if (= r-lines-count 0)
                     (+ l-suffix r-suffix)
                     r-suffix)))))

(defn split-count [^long i ^long j ^long thresh]
  (let [x (- j i)]
    (if (< x thresh)
      [[i j]]
      (let [x-h (quot x 2)]
        (concat (split-count i (+ i x-h) thresh) (split-count (+ i x-h) j thresh))))))

(def ^{:tag 'long} string-thresh 64)
(def ^{:tag 'long} string-merge-thresh (quot string-thresh 2))

(defn split-string [x]
  (assert (<= string-thresh (count-codepoints x)))
  (map (fn [[i j]] (subs x (codepoints->chars x i) (codepoints->chars x j)))
       (split-count 0 (count-codepoints x) string-thresh)))

(def tree-config {:make-node (fn [children]
                               (tree/->Node (reduce (fn [acc x] (build-r-f acc (tree/metrics x)))
                                                    (build-r-f) children)
                                            children))
                  :reducing-fn build-r-f
                  :metrics-fn metrics
                  :leaf-overflown? (fn [x] (<= string-thresh (count-codepoints x)))
                  :split-thresh 32
                  :split-leaf split-string
                  :leaf-underflown? (fn [s] (< (count-codepoints s) string-merge-thresh))
                  :merge-leafs (fn [s1 s2] (str s1 s2))})

(defn- reduce-string [^String s]
  (let [last-idx (count-codepoints s)
        leaf-vec (if (= last-idx 0)
                   [(tree/make-leaf "" tree-config)]
                   (let [leaf-vec (transient [])]
                     (loop [idx 0]
                       (if (= idx last-idx)
                         (persistent! leaf-vec)
                         (let [idx' (min (+ idx string-thresh) last-idx)]
                           (conj! leaf-vec (tree/make-leaf (subs s
                                                                 (codepoints->chars s idx)
                                                                 (codepoints->chars s idx')) tree-config))
                           (recur idx'))))))]
    leaf-vec))

(defn- reduce-nodes [nodes-vec]
  (let [p (:split-thresh tree-config)]
    (mapv (fn [nodes]
            (tree/make-node (al/into-array-list nodes) tree-config))
          (partition p p nil nodes-vec))))

(defn- reduce-tree [node-vec]
  (loop [node-vec' (reduce-nodes node-vec)]
    (if (= 1 (count node-vec'))
      (first node-vec')
      (recur (reduce-nodes node-vec')))))

(defn make-text [s]
  (reduce-tree (reduce-string s)))

(defn zipper [tree]
  (tree/zipper tree tree-config))

(defn root [^ZipperLocation loc]
  (let [root (tree/root loc)]
    (if-not (empty? (.-children root))
      root
      (make-text ""))))

(defn metrics-offset ^long [^TextMetrics m]
  (.-length m))

(defn metrics-geom-offset ^double [^TextMetrics m]
  (.-geometric-length m))

(defn metrics-char-offset ^long [^TextMetrics m]
  (.-chars-count m))

(defn metrics-line ^long [^TextMetrics m]
  (.-lines-count m))

(defn node-offset
  "Returns offset of the current node ignoring overriding accumulator"
  ^long [loc]
  (metrics-offset (tree/acc loc)))

(defn node-geom-offset
  ^double [loc]
  (metrics-geom-offset (tree/acc loc)))

(defn node-char-offset
  ^long [loc]
  (metrics-char-offset (tree/acc loc)))

(defn by-offset [^long i]
  (fn [acc m] (<= i (metrics-offset (scan-r-f acc m)))))

(defn by-geom-offset [^double i]
  (fn [acc m] (<= i (metrics-geom-offset (scan-r-f acc m)))))

(defn by-char-offset [^long i]
  (fn [acc m] (<= i (metrics-char-offset (scan-r-f acc m)))))

(defn by-char-offset-exclusive [^long i]
  (fn [acc m] (< i (metrics-char-offset (scan-r-f acc m)))))

(defn by-line [^long i]
  (fn [acc m] (<= i (metrics-line (scan-r-f acc m)))))

(defn offset ^long [^ZipperLocation loc]
  (cond
    (tree/end? loc) (metrics-offset (tree/metrics (tree/node loc)))
    (.-o-acc loc) (metrics-offset (.-o-acc loc))
    (.-acc loc) (metrics-offset (.-acc loc))
    :else 0))

(defn geom-offset ^double [^ZipperLocation loc]
  (cond
    (tree/end? loc) (metrics-geom-offset (tree/metrics (tree/node loc)))
    (.-o-acc loc) (metrics-geom-offset (.-o-acc loc))
    (.-acc loc) (metrics-geom-offset (.-acc loc))
    :else 0))

(defn line ^long [^ZipperLocation loc]
  (cond
    (tree/end? loc) (metrics-line (tree/metrics (tree/node loc)))
    (.-o-acc loc) (metrics-line (.-o-acc loc))
    (.-acc loc) (metrics-line (.-acc loc))
    :else 0))

(defn char-offset ^long [^ZipperLocation loc]
  (cond
    (tree/end? loc) (metrics-char-offset (tree/metrics (tree/node loc)))
    (.-o-acc loc) (metrics-char-offset (.-o-acc loc))
    (.-acc loc) (metrics-char-offset (.-acc loc))
    :else 0))

(defn node-line
  "Returns offset of the current node ignoring overriding accumulator"
  ^long [loc]
  (metrics-line (tree/acc loc)))

(defn nth-index [s c ^long n]
  (if (= n 0)
    0
    (chars->codepoints s
     (loop [from 0
           n n]
      (let [i (clojure.string/index-of s c from)]
        (if (= n 1)
          i
          (when (some? i)
            (recur (inc (long i)) (dec n)))))))))

(defn forget-acc [loc]
  (tree/assoc-o-acc loc nil))

(defn- at-the-right-codepoint-border? [loc]
  (let [s (.-data ^Leaf (tree/node loc))
        o (offset loc)
        loc-offset (metrics-offset (tree/loc-acc loc))
        rel-offset (- o loc-offset)]
    (= rel-offset (count-codepoints s))))

(defn scan-to-offset [loc ^long i]
  (let [offset-loc (tree/scan loc (by-offset i))]
    (if (tree/end? offset-loc)
      offset-loc
      (let [o (node-offset offset-loc)
            s (.-data ^Leaf (tree/node offset-loc))
            a (.-acc ^ZipperLocation offset-loc)
            offset-loc (tree/assoc-o-acc offset-loc (scan-r-f a (metrics-to s (offset-metrics-pred (- i o)))))
            next-node (tree/next-leaf offset-loc)]
        (if (and (at-the-right-codepoint-border? offset-loc)
                 (not (tree/end? next-node)))
          next-node
          offset-loc)))))

(defn scan-to-geom-offset [loc ^double i]
  (let [offset-loc (tree/scan loc (by-geom-offset i))]
    (if (tree/end? offset-loc)
      offset-loc
      (let [o (node-geom-offset offset-loc)
            s (.-data ^Leaf (tree/node offset-loc))
            a (.-acc ^ZipperLocation offset-loc)
            offset-loc (tree/assoc-o-acc offset-loc (scan-r-f a (metrics-to s (geom-metrics-pred (- i o)))))
            next-node (tree/next-leaf offset-loc)]
        (if (and (at-the-right-codepoint-border? offset-loc)
                 (not (tree/end? next-node)))
          next-node
          offset-loc)))))

(defn- at-the-right-char-border? [loc]
  (let [s (.-data ^Leaf (tree/node loc))
        o (char-offset loc)
        loc-offset (metrics-char-offset (tree/loc-acc loc))
        rel-offset (- o loc-offset)]
    (= rel-offset (count s))))

(defn scan-to-char-offset [loc ^long i]
  (let [offset-loc (tree/scan loc (by-char-offset i))]
    (if (tree/end? offset-loc)
      offset-loc
      (let [o (node-char-offset offset-loc)
            s (.-data ^Leaf (tree/node offset-loc))
            a (.-acc ^ZipperLocation offset-loc)
            offset-loc (tree/assoc-o-acc offset-loc (scan-r-f a (metrics-to s (char-metrics-pred (- i o)))))
            next-node (tree/next-leaf offset-loc)]
        (if (and (at-the-right-char-border? offset-loc)
                 (not (tree/end? next-node)))
          next-node
          offset-loc)))))

(defn retain [loc ^long l]
  (scan-to-offset loc (+ (offset loc) l)))

(defn scan-to-line-start [loc ^long n]
  (let [nth-eol-loc (tree/scan loc (by-line n))]
    (if (or (tree/end? nth-eol-loc) (<= n 0))
      nth-eol-loc
      (let [o (node-offset nth-eol-loc)
            l (node-line nth-eol-loc)
            data (.-data ^Leaf (tree/node nth-eol-loc))
            eol-idx (nth-index data
                               \newline
                               (- n l))
            s (.-data ^Leaf (tree/node nth-eol-loc))
            a (.-acc ^ZipperLocation nth-eol-loc)
            prev-line-end (tree/assoc-o-acc nth-eol-loc (scan-r-f a (metrics-to s (offset-metrics-pred eol-idx))))]
        (-> prev-line-end
            (retain 1))))))

(defn distance-to-EOL ^long [loc]
  (let [next-loc (scan-to-line-start loc (inc (line loc)))
        len (- (offset next-loc)
               (offset loc))]
    (if (tree/end? next-loc)
      len
      (dec len))))

(defn lines-count [t]
  (if t
    (inc (.-lines-count ^TextMetrics (tree/metrics t)))
    0))

(defn text-length ^long [t]
  (if t
    (metrics-offset (tree/metrics t))
    0))

(defn chars-count [t]
  (if t
    (metrics-char-offset (tree/metrics t))
    0))

(defn text [loc ^long l]
  (loop [loc loc
         l l
         sb (StringBuilder.)]
    (if (< 0 l)
      (if (tree/end? loc)
        (throw (ex-info "Length is out of bounds" {:l l}))
        (if (tree/branch? loc)
          (recur (tree/down-forward loc) l sb)
          (let [i (offset loc)
                text (.-data ^Leaf (tree/node loc))
                base-offset (metrics-offset (tree/loc-acc loc))
                start (- i base-offset)
                end (min (count-codepoints text) (+ start l))
                s-len (- end start)
                chars-start (codepoints->chars text start)
                chars-end (codepoints->chars text end)]
            (recur (tree/next (forget-acc loc))
              (- l s-len)
              (.append sb ^java.lang.CharSequence text chars-start chars-end)))))
      (.toString sb))))

(defn text-up-to-char [loc ^long l]
  (loop [loc loc
         l l
         sb (StringBuilder.)]
    (if (< 0 l)
      (if (tree/end? loc)
        (throw (ex-info "Length is out of bounds" {:l l}))
        (if (tree/branch? loc)
          (recur (tree/down-forward loc) l sb)
          (let [i (char-offset loc)
                text (.-data ^Leaf (tree/node loc))
                base-offset (metrics-char-offset (tree/loc-acc loc))
                start (- i base-offset)
                end (min (count text) (+ start l))
                s-len (- end start)]
            (recur (tree/next (forget-acc loc))
              (- l s-len)
              (.append sb ^java.lang.CharSequence text start end)))))
      (.toString sb))))

(defn as-string [text-tree]
  (text (zipper text-tree)
             (text-length text-tree)))

(defn insert [loc ^String s]
  (if (tree/branch? loc)
    (recur (tree/down-forward loc) s)
    (let [i (offset loc)
          chunk-offset (metrics-offset (tree/loc-acc loc))
          rel-offset (- i chunk-offset)]
      (-> loc
          (tree/edit (fn [^Leaf node]
                       (let [data (or (some-> node (.-data)) "")
                             rel-char-offset (codepoints->chars data rel-offset)]
                         (def data data)
                         (tree/make-leaf (str (subs data 0 rel-char-offset)
                                              s
                                              (subs data rel-char-offset)) tree-config))))
          (retain (count-codepoints s))))))

(defn delete [loc ^long l]
  (if (tree/branch? loc)
    (recur (tree/down-forward loc) l)
    (let [i (offset loc)
          chunk-offset (metrics-offset (tree/loc-acc loc))
          rel-offset (- i chunk-offset)
          chunk-l (count-codepoints (.-data ^Leaf (tree/node loc)))
          end (min chunk-l (+ rel-offset l))
          next-loc  (if (and (= rel-offset 0) (= end chunk-l))
                      (tree/remove (forget-acc loc))
                      (-> loc
                          (tree/edit (fn [node]
                                       (let [s (.-data ^Leaf node)]
                                         (tree/make-leaf
                                          (str (subs s 0 (codepoints->chars s rel-offset))
                                               (subs s (codepoints->chars s end)))
                                          tree-config))))
                          (scan-to-offset i)))
          deleted-c (- end rel-offset)]
      (if (< deleted-c l)
        (recur next-loc (- l deleted-c))
        next-loc))))

(defn delete-chars [loc ^long l]
  (if (tree/branch? loc)
    (recur (tree/down-forward loc) l)
    (let [i (char-offset loc)
          chunk-offset (metrics-char-offset (tree/loc-acc loc))
          rel-offset (- i chunk-offset)
          chunk-l (count (.-data ^Leaf (tree/node loc)))
          end (min chunk-l (+ rel-offset l))
          next-loc  (if (and (= rel-offset 0) (= end chunk-l))
                      (tree/remove (forget-acc loc))
                      (-> loc
                          (tree/edit (fn [node]
                                       (let [s (.-data ^Leaf node)]
                                         (tree/make-leaf (str (subs s 0 rel-offset) (subs s end)) tree-config))))
                          (scan-to-char-offset i)))
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
                    :delete (delete loc (if (string? arg) (count-codepoints arg) arg))))
                (zipper t)
                operation)))


(defn line-text [t i]
  (let [loc (scan-to-line-start (zipper t) i)]
    (text loc (distance-to-EOL loc))))

(defn text-range [tree ^long from ^long to]
  (assert (<= from to) {:from from :to to})
  (if (= from to)
    ""
    (-> (zipper tree)
        (scan-to-offset from)
        (text (- to from)))))

(defn max-line-length ^double [text]
  (.-max-line-length ^TextMetrics (tree/metrics text)))

(defn leaf->text [loc]
  {:base (metrics-char-offset (tree/loc-acc loc))
   :text (.-data ^Leaf (tree/node loc))})

(defn- scan-by-char-offset-exclusive [loc i]
  (tree/scan loc (by-char-offset-exclusive i)))

(defn skip-to-line-end [loc]
  (let [offset (offset loc)
        delta (distance-to-EOL loc)]
    (scan-to-offset loc (+ offset delta))))

(defn skip-columns [loc ^double x]
  (let [geom (geom-offset loc)
        cur-line (line loc)
        loc' (scan-to-geom-offset loc (+ geom x))]
    (if (= cur-line (line loc'))
      loc'
      (skip-to-line-end loc))))

(defn column ^double [^ZipperLocation loc] ;; todo this doesn't work anymore?
  (let [cur-line (line loc)
        start-loc (scan-to-line-start (zipper (root loc)) cur-line)]
    (- (geom-offset loc) (geom-offset start-loc))))

(deftype TextSequence [t ^{:volatile-mutable true} loc ^long from-char ^long to-char]
  CharSequence
  (^int length [this]
               (- to-char from-char))
  (^char charAt [this ^int char-index]
                (assert (< char-index (- to-char from-char)) "Index out of range")
                (let [absolute-index (+ char-index from-char)
                      {:keys [^long base ^String text]} (leaf->text loc)]
                  (cond
                    (< absolute-index base)
                    (let [new-loc (scan-by-char-offset-exclusive (zipper t) absolute-index)
                          {:keys [^int base ^String text]} (leaf->text new-loc)]
                      (set! loc new-loc)
                      (.charAt text (- absolute-index base)))

                    (< absolute-index (+ base (count text)))
                    (.charAt text (- absolute-index base))

                    (<= (+ base (count text)) absolute-index)
                    (let [new-loc (scan-by-char-offset-exclusive loc absolute-index)
                          {:keys [^long base ^String text]} (leaf->text new-loc)]
                      (set! loc new-loc)
                      (.charAt text (- absolute-index base)))

                    :else (throw (ex-info "No way" {:from from-char :to to-char :index char-index})))))
  (^CharSequence subSequence [this ^int from' ^int to']
                             (assert (<= from' (- to-char from-char)) "From index out of range")
                             (assert (<= to' (- to-char from-char)) "To index out of range")
                             (TextSequence. t (scan-by-char-offset-exclusive (zipper t) 0) (+ from-char from') (+ from-char to')))
  (^String toString [this] (text-up-to-char (scan-to-char-offset (zipper t) from-char) (- to-char from-char))))

(defn ^CharSequence text->char-seq [t]
  (TextSequence. t (scan-by-char-offset-exclusive (zipper t) 0) 0 (chars-count t)))

(defn offset->char-offset ^long [text offset]
  (-> (zipper text)
      (scan-to-offset offset)
      (char-offset)))

(defn char-offset->offset ^long [text char-offset]
  (-> (zipper text)
      (scan-to-char-offset char-offset)
      (offset)))
