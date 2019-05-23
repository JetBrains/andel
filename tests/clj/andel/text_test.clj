(ns andel.text-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as g]
            [clojure.test.check.properties :as prop]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer :all]
            [clojure.test.check :as tc])
  (:import [andel Text Rope Rope$Zipper Rope$Node Text$TextMetrics Text$TextOps Rope$ZipperOps]))

(defn codepoints-count [^String arg]
  (.codePointCount arg 0 (.length arg)))

(defn play [t operation]
  (Text/root (reduce (fn [^Rope$Zipper loc [code arg]]
                  (case code
                    :retain (Text/retain loc ^long arg)
                    :insert (Text/insert loc ^String arg)
                    :delete (Text/delete loc ^long (if (string? arg) (codepoints-count arg) arg))
                    ))
                (Text/zipper t)
                operation)))

(defn op-frames-gen [size]
  (g/not-empty
   (g/vector
    (g/one-of [(g/tuple
                (g/return :retain) g/pos-int)
               (g/tuple
                (g/return :insert) g/string-alphanumeric)
               (g/tuple
                (g/return :delete) g/pos-int)])
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

(g/sample operations-gen)

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

(defn text-length [node]
  (.-length ^Text$TextMetrics (Rope/metrics node)))

(defn make-text [text]
  (let [ops (Text$TextOps. 4 5)]
    (Rope/growTree (Rope/wrapNode (Rope/makeLeaf text ops) ops) ops)))

(def play-test
  (prop/for-all [[text operation] operations-gen]
                (try
                  (let [t' (play (make-text text) operation)]
                    (= (Text/text (Text/zipper t') (text-length t'))
                       (play-naive text operation)))
                  (catch Throwable ex
                    (def my-ex ex)
                    false))))

(def play-many-test
  (prop/for-all [[text operations] (operations-seq-gen 10)]
                (try
                  (let [t' (reduce play (make-text text) operations)]
                    (= (Text/text (Text/zipper t') (text-length t'))
                       (reduce play-naive text operations)))
                  (catch Throwable ex
                    (def my-ex ex)
                    false))))

(deftest generative
  (is (:result (tc/quick-check 5000 play-test)))
  (is (:result (tc/quick-check 1000 play-many-test))))

(comment

  (defn mp [^andel.Text$TextMetrics metrics]
    {:length           (.-length metrics)
     :geometric-length (.-geometricLength metrics)
     :lines-count      (.-linesCount metrics)
     :chars-count      (.-charsCount metrics)})

  (defn np [node]
    (if (instance? andel.Rope$Node node)
      (let [^andel.Rope$Node node node]
        {:metrics  (mp (.-metrics node))
         :children (mapv np (.-children node))})
      (let [^andel.Rope$Leaf leaf node]
        {:metrics (mp (.-metrics leaf))
         :data    (.-data leaf)})))

  (defn zp [^Rope$Zipper zipper]
    {:siblings (mapv np (.-siblings zipper))
     :idx      (.-idx zipper)
     :parent   (when-let [p (.-parent zipper)]
                 (zp p))
     :root?    (.-isRoot zipper)
     :acc      (some-> (.-acc zipper) (mp))
     :oacc     (some-> (.-oacc zipper) (mp))
     :end?     (.-isEnd zipper)})

  (def foo
    [["0"
    [[[:delete "0"]
      [:retain 0]]]]])

  (let [[[text operations]] foo
        t                  (make-text text)
        t'                 (reduce play t operations)]
    {:before text
     :operations operations
     :after (Text/text (Text/zipper t') (text-length t'))
     :should-be (reduce play-naive text operations)})

  (zp
   (-> (make-text "00000000000000000")
       (Text/zipper)
       (Text/insert "0")
       (Text/delete (count "00000000000000000"))
       (Text/insert "1")
       (Text/root)))

  (require '[andel.text :as text])
  (require '[andel.tree :as tree])

  (into {}
         (-> (text/make-text "a")
             (text/zipper)
             (text/delete 1)
             (text/insert "b")))

  )

(comment

  (require '[clojure.test.check.rose-tree :as rose])
  (require '[clojure.test.check.random :as random])

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
    (slurp (clojure.java.io/file "../../community//platform/platform-impl/src/com/intellij/openapi/editor/impl/EditorImpl.java")))

  (def operation (make-operation editor-impl))

  (require '[onair.dev])
  (require '[andel.text])

  (def clj-tree (andel.text/make-text editor-impl))
  (def java-tree (Text/makeText editor-impl))

  (= (play-naive editor-impl operation)
     (andel.text/as-string (andel.text/play (andel.text/make-text editor-impl) operation))
     (let [t (play (Text/makeText editor-impl) operation)]
       (Text/text (Text/zipper t) (text-length t))))

  (def operation2 (make-operation (play-naive editor-impl operation)))

  (defn make-random-accesses [text c]
    (let [length (.length ^String text)
          g (g/vector
             (g/fmap (fn [[a b]] [(min length a) (min (- length a) b)])
                     (g/tuple (g/resize length g/pos-int) (g/resize 1024 g/pos-int))))]
      (generate g c seed)))

  (def random-accesses
    (make-random-accesses editor-impl 10000))

  (def immaculate-text (com.intellij.util.text.CharArrayUtil/createImmutableCharSequence editor-impl))

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

  (def ensure-chunked
    (let [^java.lang.reflect.Method m (doto (.getDeclaredMethod (type immaculate-text) "ensureChunked" (into-array java.lang.Class []))
                                    (.setAccessible true))]
      (fn [t] (.invoke m t (object-array [])))))

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

  (->> (iterate head (my-node not-so-immaculate-text))
       (take-while (fn [n] (= com.intellij.util.text.ImmutableText$CompositeNode (type n))))
       (count))

  (defn text-clj [t [from len]]
    (-> (andel.text/zipper t)
        (andel.text/scan-to-offset from)
        (andel.text/text len)))

  (defn text-java [t [from len]]
    (-> (Text/zipper t)
        (Text/scanToOffset from)
        (Text/text len)))

  (defn text-intellij [t [from len]]
    (.subSequence ^com.intellij.util.text.ImmutableText t from (+ from len)))

  (defn random-access [t impl a]
    (into [] (map (fn [a] (str (impl t a)))) a))

  (let [as (make-random-accesses editor-impl 10)]
    (= (random-access clj-tree text-clj as)
       (random-access java-tree text-java as)
       (random-access immaculate-text text-intellij as)))

  (do
    (do
      (prn "MAKE TREE")
      (prn "CLJ VERSION")
      (onair.dev/benchmark
       (andel.text/make-text editor-impl))

      (prn "JAVA VERSION")
      (onair.dev/benchmark
       (Text/makeText editor-impl))
      (prn "IMMUTABLE TEXT")
      (onair.dev/benchmark
        (ensure-chunked (com.intellij.util.text.CharArrayUtil/createImmutableCharSequence editor-impl))))

    (do
      (prn "PLAY OPERATION")
      (prn "CLJ VERSION")
      (onair.dev/benchmark
       (andel.text/play clj-tree operation))

      (prn "JAVA VERSION")
      (onair.dev/benchmark
       (play java-tree operation))

      (prn "IMMUTABLE TEXT")
      (onair.dev/benchmark
       (play-intellij not-so-immaculate-text operation)))

    (do
      (prn "RANDOM ACCESS")
      (prn "CLJ VERSION")
      (onair.dev/benchmark
        (random-access clj-tree text-clj random-accesses))

      (prn "JAVA VERSION")
      (onair.dev/benchmark
        (random-access java-tree text-java random-accesses))

      (prn "IMMUTABLE TEXT")
      (onair.dev/benchmark
        (random-access not-so-immaculate-text text-intellij random-accesses))))

  (defn calculate-codepoints [^String s]
     (.codePointCount s 0 (.length s)))

  (onair.dev/benchmark
   (calculate-codepoints editor-impl))


  (onair.dev/profile (Text/makeText editor-impl))

  (onair.dev/profile-with-interval 10000
   (random-access java-tree text-java random-accesses))


  )