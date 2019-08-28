(ns andel.text-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as g]
            [clojure.test.check.properties :as prop]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer :all]
            [clojure.test.check :as tc])
  (:import [andel
            Rope Rope$Zipper Rope$Node
            Text Text$TextMetrics Text$TextOps Text$Sequence Rope$ZipperOps]))

(defn codepoints-count [^String arg]
  (.codePointCount arg 0 (.length arg)))

(defn play [t operation]
  (Text/root
   (reduce (fn [loc [code arg]]
             (case code
               :retain (Text/retain loc ^long arg)
               :insert (Text/insert loc ^String arg)
               :delete (Text/delete loc ^long (if (string? arg) (codepoints-count arg) arg))))
           (Text/zipper t)
           operation)))

(defn play-transient [t operation]
  (Text/root
   (reduce (fn [loc [code arg]]
             (case code
               :retain (Text/retain loc ^long arg)
               :insert (Text/insert loc ^String arg)
               :delete (Text/delete loc ^long (if (string? arg) (codepoints-count arg) arg))))
           (Rope/toTransient (Text/zipper t))
           operation)))

(defn op-frames-gen [size]
  (g/not-empty
   (g/vector
    (g/one-of [(g/tuple
                (g/return :retain) g/s-pos-int)
               (g/tuple
                (g/return :insert) g/string-alphanumeric)
               (g/tuple
                (g/return :delete) g/s-pos-int)])
    1 size)))

(defn random-ops [text frames]
  (loop [text text
         result []
         [[code arg :as op] & rest-ops] frames]
    (if (nil? op)
      (conj result [:retain (count text)])
      (let [arg (if (#{:retain :delete} code) (min arg (count text)) arg)]
        (case code
          :retain (if (empty? text)
                    result
                    (recur (subs text arg)
                      (conj result [:retain arg])
                      rest-ops))
          :insert (recur text
                    (conj result [:insert arg])
                    rest-ops)
          :delete (if (empty? text)
                    result
                    (recur (subs text arg)
                      (conj result [:delete (subs text 0 arg)])
                      rest-ops)))))))

(defn play-naive [text operation]
  (let [[idx text]
        (reduce (fn [[idx text] [code arg]]
                  (case code
                    :retain [(+ idx arg) text]
                    :delete [idx (str (subs text 0 idx) (subs text (+ idx (count arg))))]
                    :insert [(+ idx (count arg)) (str (subs text 0 idx)
                                                      arg
                                                      (subs text idx))]))
                [0 text] operation)]
    (assert (= idx (count text)))
    text))

(def operations-gen
  (g/fmap
   (fn [[text frames]]
     [text (random-ops text frames)])
   (g/tuple g/string-alphanumeric (op-frames-gen 50))))

(defn operations-seq-gen [size]
  (g/fmap
   (fn [[starting-text operations]]
     [starting-text
      (second
       (reduce
        (fn [[text r] frames]
          (let [ops (random-ops text frames)]
            [(play-naive text ops)
             (conj r ops)]))
        [starting-text []]
        operations))])
   (g/tuple g/string-alphanumeric (g/vector (op-frames-gen size) 1 size))))

(defn make-text [text]
  (Text/makeText text (Text$TextOps. 4 5)))

(defn play-test [play-impl]
  (prop/for-all [[text operation] (g/no-shrink operations-gen)]
                (try
                  (let [t' (play-impl (make-text text) operation)]
                    (= (Text/text (Text/zipper t') (Text/length t'))
                       (play-naive text operation)))
                  (catch Throwable ex
                    (def my-ex ex)
                    false))))

(defn play-many-test [play-impl]
  (prop/for-all [[text operations] (g/no-shrink (operations-seq-gen 10))]
                (let [t' (reduce play-impl (make-text text) operations)]
                  (= (Text/text (Text/zipper t') (Text/length t'))
                     (reduce play-naive text operations)))))

(defn tree-is-persistent-prop [play-impl]
  (prop/for-all [[text operations] (g/no-shrink (operations-seq-gen 10))]
                (let [ts (reduce (fn [[tree :as trees] operation]
                                   (cons (play tree operation) trees))
                                 (list (make-text text))
                                 operations)
                      naive-texts (reduce (fn [[text :as texts] operation]
                                            (cons (play-naive text operation) texts))
                                          (list text)
                                          operations)]
                  (= (map (fn [tree] (Text/text (Text/zipper tree) (Text/length tree))) ts)
                     naive-texts))))

(deftest generative
  (is (:result (tc/quick-check 3000 (play-test play))))

  (is (:result (tc/quick-check 3000 (play-test play-transient))))

  (is (:result (tc/quick-check 1000 (play-many-test play))))
  (is (:result (tc/quick-check 1000 (play-many-test play-transient))))

  (is (:result (tc/quick-check 10000 (tree-is-persistent-prop play))))
  (is (:result (tc/quick-check 10000 (tree-is-persistent-prop play-transient)))))

(comment

  (defn mp [^andel.Text$TextMetrics metrics]
    {:length           (.-length metrics)
     :geometric-length (.-geometricLength metrics)
     :lines-count      (.-linesCount metrics)
     :chars-count      (.-charsCount metrics)})

  (defn np [node]
    (if (instance? andel.Rope$Node node)
      (let [^andel.Rope$Node node node]
        {:metrics  (map mp (.-metrics node))
         :children (mapv np (.-children node))})
      (str node)))

  (defn zp [^Rope$Zipper zipper]
    {:siblings (mapv np (.-siblings zipper))
     :idx      (.-idx zipper)
     :parent   (when-let [p (.-parent zipper)]
                 (zp p))
     :acc      (some-> (.-acc zipper) (mp))
     :oacc     (some-> (.-oacc zipper) (mp))})

  (def foo
    [["diO874d2ojcHX733ue6MzQRaTWXvU88MPs"
      [[[:delete "diO874d2ojcHX73"] [:insert ""] [:retain 19]]]]])


  (let [[[text operations]] foo]
    (loop [text text
           [op & rest :as operations] operations]
      (when (seq rest)
        (let [text' (play-naive text op)
              naive-final (reduce play-naive text' rest)
              t (reduce play (make-text text') rest)
              final (Text/text (Text/zipper t) (Text/length t))]
          (if (= naive-final final)
            [[text operations]]
            (recur text' rest))))))

  (let [[[text operations]] foo
        t                  (make-text text)
        t'                 (reduce play t operations)
        should-be (reduce play-naive text operations)
        text' (Text/text (Text/zipper t') (Text/length t'))]
    {:success? (= should-be text')
     :before text
     :should-be-vs-after [(count should-be) (count text')]
     :operations operations
     :after text'
     :should-be should-be})

  (zp
   (-> (make-text "0000000000")
       (Text/zipper)
       (Text/retain 10)))
  )

(comment

  (do
    (require '[criterium.core :as crit])
    (require '[clj-async-profiler.core :as prof])
    (require '[clojure.test.check.random :as random])
    (require '[clojure.test.check.rose-tree :as rose])
    (require '[clojure.test.check.random :as random])

    (let [base "/Users/shparun/sources/ultimate/out/classes/production"
          modules (.list (clojure.java.io/file base))
          cl (.getClassLoader (.getClass (fn [])))]
      (doseq [m modules]
        (.addURL cl (.toURL (.getCanonicalFile (clojure.java.io/file base m)))))
      (with-bindings {Compiler/LOADER cl}
        (eval
        '(do
          (defmacro benchmark [& expressions]
            `(crit/with-progress-reporting
              (crit/quick-bench
               (do
                 ~@expressions
                 nil))))

          (defn generate [generator size seed]
            (let [rng (random/make-random seed)]
              (rose/root (g/call-gen generator rng size))))

          (def seed 1558543749024)

          (defn make-operation [text]
            (generate
             (g/fmap
              (fn [frames] (random-ops text frames))
              (op-frames-gen 10000))
             100
             seed))

          (def editor-impl
            (slurp (clojure.java.io/file "/Users/shparun/sources/ultimate/community/platform/platform-impl/src/com/intellij/openapi/editor/impl/EditorImpl.java")))

          (def operation (make-operation editor-impl))

          (def java-tree (Text/makeText editor-impl))

          (defn make-random-accesses [text c]
            (let [length (.length ^String text)
                  g (g/vector
                     (g/fmap (fn [[a b]] [(min length a) (min (- length a) b)])
                             (g/tuple (g/resize length g/pos-int) (g/resize 1024 g/pos-int))))]
              (generate g c seed)))

          (def random-accesses
            (make-random-accesses editor-impl 10000))

          (def immaculate-text (com.intellij.util.text.CharArrayUtil/createImmutableCharSequence editor-impl))

          (def ensure-chunked
            (let [^java.lang.reflect.Method m (doto (.getDeclaredMethod (type immaculate-text) "ensureChunked" (into-array java.lang.Class []))
                                                (.setAccessible true))]
              (fn [t] (.invoke m t (object-array [])))))

          (defn make-immutable-text [text]
            (ensure-chunked (com.intellij.util.text.CharArrayUtil/createImmutableCharSequence text)))


          (defn play-intellij [text operation]
            (loop [^com.intellij.util.text.ImmutableText it text
                   [[code arg :as op] & rest] operation
                   offset 0]
              (if (nil? op)
                it
                (case code
                  :retain (recur it rest (+ offset (long arg)))
                  :insert (recur (.insert it offset ^CharSequence arg) rest (+ offset (.length ^String arg)))
                  :delete (recur (.delete it offset (+ offset (.length ^String arg))) rest offset)))))



          (def not-so-immaculate-text (ensure-chunked immaculate-text))

          (def my-node
            (let [^java.lang.reflect.Field f (doto (.getDeclaredField (type immaculate-text) "myNode")
                                                   (.setAccessible true))]
              (fn [t] (.get f t))))

          (def head
            (let [^java.lang.reflect.Field f (doto (.getDeclaredField com.intellij.util.text.ImmutableText$CompositeNode "head")
                                                   (.setAccessible true))]
              (fn [n] (.get f n))))

          (def tail
            (let [^java.lang.reflect.Field f (doto (.getDeclaredField com.intellij.util.text.ImmutableText$CompositeNode "tail")
                                                   (.setAccessible true))]
              (fn [n] (.get f n))))

          (defn text-java [t [from len]]
            (-> (Text/zipper t)
                (Text/scanToOffset from)
                (Text/text len)))

          (defn text-intellij [t [from len]]
            (.subSequence ^com.intellij.util.text.ImmutableText t from (+ from len)))

          (defn random-access [t impl a]
            (into [] (map (fn [a] (str (impl t a)))) a))

          (defn iterate-char-seq [^CharSequence char-seq]
            (let [length (.length char-seq)]
              (loop [i 0]
                (when (< i length)
                  (.charAt char-seq i)
                  (recur (inc i))))))
          ;; eval here
          )))))

  (= (play-naive editor-impl operation)
     (let [t (play (Text/makeText editor-impl) operation)]
       (Text/text (Text/zipper t) (text-length t)))
     (let [t (play-transient (Text/makeText editor-impl) operation)]
       (Text/text (Text/zipper t) (text-length t))))

  (let [as (make-random-accesses editor-impl 10)]
    (= (random-access java-tree text-java as)
       (random-access immaculate-text text-intellij as)))

  (let [ts (Text$Sequence. java-tree)
        len (.length ts)]
    (and (= len (.length editor-impl))
         (loop [i 0]
           (if (< i len)
             (do
               (assert (= (.charAt ^String editor-impl i) (.charAt ts i)))
               (recur (inc i)))
             true))))

  (do
    (do
      (prn "MAKE TREE")
      (prn "ANDEL")
      (benchmark
       (Text/makeText editor-impl))
      (prn "IMMUTABLE TEXT")
      (benchmark
        (make-immutable-text editor-impl)))

    (do
      (prn "PLAY OPERATION")
      (prn "ANDEL")
      (benchmark
       (play java-tree operation))

      (prn "ANDEL TRANSIENT")
      (benchmark
       (play-transient java-tree operation))

      (prn "IMMUTABLE TEXT")
      (benchmark
       (play-intellij not-so-immaculate-text operation)))

    (do
      (prn "RANDOM ACCESS")

      (prn "ANDEL")
      (benchmark
       (random-access java-tree text-java random-accesses))

      (prn "IMMUTABLE TEXT")
      (benchmark
        (random-access not-so-immaculate-text text-intellij random-accesses)))
    (do
      (prn "CHAR SEQUENCE")
      (prn "ANDEL")
      (benchmark
        (iterate-char-seq (Text$Sequence. java-tree)))

      (prn "IMMUTABLE TEXT")
      (benchmark
       (iterate-char-seq not-so-immaculate-text))))


  )
