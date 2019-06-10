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

(comment

  (tc/quick-check 10000 (tree-is-persistent-prop play-transient))


  (g/sample (operations-seq-gen 10))

  )

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

  ;; seed 1560198725280
  (def foo
    [["bOK0IDX7G0U4P42ycwhxXCM7Hk9nYntacgw63IOo5hqmXGUeC4fb132okmzdBvvs1QI5p414hx3BX5273321Clhk2Lakm2b5HLyrIXN5AR72liAm3Afz0Q5o9S86E2tkE5n8Vh8aasfL8KZ94g0TmgCsZxLCHp5XF3j36Ncn1dAA"
    [[[:insert
       "2fuFKYd7rR3jQ50tEJPWkvnbf40Gdx7ufDTN6c306s8oaG94GriHi7yzCGt83AskoT9B5xLTBUc63fu753105ZjYaWZGkr9696nu43io8E2k26ZV30a40g7dCX8F4Es568C09dmY"]
      [:insert
       "5IIxF82hPByqPrOA94f5hWwvCiAgeKXuiqMop06kK85YeTmmj3lMTABYUH306TLzvoEg"]
      [:insert
       "YIDV2GqzZDy9et57P53OjF0Lit2PDDNd5l260ZRWOi5VCLiX4Rj3w58quuXVQs2uOB6fkOyAx45XYpEKPbVi76hQ7xEV98kk98Q96183B40M3bzLHEj9w7zZE8cj3Rf1rJynt0xTyv0y"]
      [:retain 36]
      [:delete
       "3IOo5hqmXGUeC4fb132okmzdBvvs1QI5p414hx3BX5273321Clhk2Lakm2b5HLyrIXN"]
      [:retain 28]
      [:insert
       "xI4PD455QJ9nl691Iu4q13oLDjF3n65Os83woQyvYPzV622z46A4adQk0XLzFb71ZscS3G1ctrRi94xsJ9xx"]
      [:insert "1ray10W1FFZ4lD17V05QvC9xL8Iey97E93nlESmvbz6Cv"]
      [:insert "5gO6OaV39nWlb4hBt"]
      [:retain 41]]
     [[:retain 108] [:delete "26ZV30a40"] [:retain 478]]
     [[:insert
       "23i2cbi95x384N8L1d6nd6AYelU1mnicy4OYGkyWuQR9E2P5Dpho9b4s782gOwRw4bn5YH8g3D31hqRkc6Sy7YF6eyH2aAsnReIT4uEmK0KQNeG5RW17fasvO8ror3D05GLJRJgJm5Q2hY9eVtD2R42y2O13NK4U3xM2q18B5jNUd0"]
      [:delete "2fuFKYd7rR"]
      [:delete
       "3jQ50tEJPWkvnbf40Gdx7ufDTN6c306s8oaG94GriHi7yzCGt83AskoT9B5"]
      [:insert
       "m1rL4rmTXIt98A20GE7D6o5Nb93768177ex5U56BXUfHaE4uH6V5dXA6R8u4R0s11J9l1CZS5j1w"]
      [:retain 517]]
     [[:retain 91]
      [:insert
       "Mc7088EfV8asjuvQ9OXwdS0B94G58bDt8gwihiRsRI9cjOO9ru5yw54344Y7y2gIc42Unp9DEwg3KIbc25xuw4nzJy44Vj60HbUkr9IE2Vf08yK5UG09IzXR44pHtNsU3LGq0wdL3jneE4m4pV4uQu"]
      [:delete
       "2aAsnReIT4uEmK0KQNeG5RW17fasvO8ror3D05GLJRJgJm5Q2hY9eVtD2R42y2O13NK4U3xM2q18B5jNUd0m1rL4rmTXIt98A20GE7D6o5Nb93768177ex5U56BXUfHaE4uH6V5dXA6R8u4R0s11J9l1CZS5j1"]
      [:retain 76]
      [:insert
       "QE83D4PIHup8Eu1zIwp883PEU788wlVX3i4ri3S4Av29w5STY0Wo58q00i9H0H0tXx1x5FzF360CufOXn6lS60i4cyrbkPeOgjJwRDz8Dc"]
      [:retain 54]
      [:insert
       "WziqIirQj557JoqNH7rcd99N2UnVur8638Sp91T3IMl08HfKORFO0E0oF85pE3WCZ0q9IT047Ygl9Ld1FFbtJl5gsPBHPne93599vEhs4Mn33QD3qAm7LPLX01KUw7BG96ytEoaH7guO5M34HrbQ5WQMpM0q8c1v94seAfVmE483buk9c"]
      [:insert
       "6sPys8i0K8cxGn2nkXldolDsC1oWcHuid25938Y8d0lRKF2Ja6dTR5YlAHQYRlsPF1x0E83ogM4t0e1mb"]
      [:retain 388]]
     [[:insert
       "ggc8Jx8D86x7hf4os91T74Fk6UA3Cd1ZDENuZ6W61mz06Feuj4487r0ka4vz8iUoIzPP339Wbm2i2LIXL40"]
      [:insert
       "gHn717VkOW7EJqVUQZpBskid32C8kJdWG5h2Y76pR964Ymqc47tJ58xxRsTg5S1Fmicmwws7FEZVCViIXxllZK7X3664OL4MqtsqSRCr474"]
      [:insert
       "UsHgCXelOYS5AUeE4fK0lBo4Hhip8xqI3KQvw7FG63kGW06Ub5sm2nq166Cb2Q3waxIjgh1Ql6Nh7bzF44mNahd495C1AIBTnF9tBzE4n7PmV34AuJX5ZW"]
      [:delete "23i2cbi95x384N8L1d"]
      [:retain 1105]]
     [[:retain 141]
      [:delete
       "Tg5S1Fmicmwws7FEZVCViIXxllZK7X3664OL4MqtsqSRCr474UsHgCXelOYS5AUeE4fK0lBo4H"]
      [:delete "hip8xqI3KQvw7FG63kGW06Ub5sm2nq166Cb"]
      [:retain 1163]]
     [[:insert "uwh06J045FdD2XnDy5x"] [:retain 1304]]
     [[:delete "uwh06J045FdD2XnDy5xggc8Jx8D86x7hf4os91T"]
      [:insert
       "5td21ESTeNNcEKQ7g9GKD7R4L4e4NS8T6jle3NliP9xFnm73v7HS4EWDk8RdD3si2w6fUmaaiMW9cZv97vtM9O3Xfo8TQNt72cEAVmCt6801R82C058wZ6R0iRu3M8nE5avfWt26s"]
      [:insert
       "T5002l6xcMb7p7HebqX852R4o5l375r2i9hWEAU4i5o68pUUt26iqCx7yWiLD5EXC39v77Ycwwg696T6bIz71mn1H6BL5D4ZQUO85KXhTYr2NkmKUAI7kYET6sr28DAvwp8gzhcNX9E9R84u72NoG6Kv91A4hGVS07Nb24kp5Ff1u"]
      [:insert
       "djNE18Z5vRCd8du3Yf582Ic7Kv22c3PGV8sTbLlJwYAaRn2476Z9tshpVrDZNWa93U3JsqW4f0Ety6O2q8ohYLcDy25PDY6Y2Hkzd4rtn124Xl9P9Jjn0DBwRk7sJ0vJQ"]
      [:retain 8]
      [:insert
       "GpP9Pudh3o1esZq140J0Jtb038gcBxl905vVPl1Ll1FDcC8q441pv5u6777fCt125EVk813P6b3h2k0u6Zwwazcc1831i4EV7OnF05Zxb5r3mRoN"]
      [:insert "ysczIb0f6Xn0jh"]
      [:retain 1276]]
     [[:insert "RfPejp1p9VV24d5065TjMNLTJLD6gskQTa5"]
      [:retain 151]
      [:delete
       "HebqX852R4o5l375r2i9hWEAU4i5o68pUUt26iqCx7yWiLD5EXC39v77Ycwwg696T6bIz71mn1H6BL5D4ZQUO85KXhTYr2NkmKUAI7kYET6sr28DAvwp8gzhcNX9E9R84u72NoG6Kv91A4hGVS07Nb24kp5Ff1udjNE18Z5vRCd8"]
      [:delete
       "du3Yf582Ic7Kv22c3PGV8sTbLlJwYAaRn2476Z9tshpVrDZNWa93U3JsqW4f"]
      [:insert
       "WR425pm0OoyGBwIKi8NFhCG8u9J92bp1IC5VziQzfl1J0Dx9upmuW0cVBACt3MSW0QwuIJDr0gEG9n0O5x0y4N1I2BXJQgi1p0UVhJ8yp0F53A443W5HPBgQRq5QBjzcT3jPkrFYfDxt2n8Br"]
      [:retain 11]
      [:delete
       "YLcDy25PDY6Y2Hkzd4rtn124Xl9P9Jjn0DBwRk7sJ0vJQ74Fk6UA3GpP9Pudh3o1esZq140J0Jtb038gcBxl905vVPl1Ll1FDcC8q441pv5u6777fCt125EVk813P6b3h2k0u6Zwwazcc1831i4EV7OnF05Zxb5r3mRoNysczIb"]
      [:delete "0f6Xn0jhCd1ZDENuZ6W61mz06Feuj4487r0ka4vz8i"]
      [:retain 1242]]
     [[:insert
       "554olXv87VgYH6AC2m6xlF2gi8d2H0U60zSb5fUEh5v88flWyeX3sMy91W0k5TfASrAOp3A5Etb127ZPp5Yzj6q0QgN113IV6pT5E9369jb3DM0c72rJPmF3Q03yO0O3kZFkDn9FZHnK86Lzv68Q3y24p9V2D9PkfSHk03t"]
      [:delete
       "RfPejp1p9VV24d5065TjMNLTJLD6gskQTa55td21ESTeNNcEKQ7g9GKD7R4L4e4NS8T6jle3NliP9xFnm73v7HS4EWDk8RdD3si2w6fUmaaiMW9cZv97vtM9O3Xfo8TQN"]
      [:retain 96]
      [:insert
       "wq3b6600JE5iCO0ugp1b08Hx83ZzY3B3BF1d7P9cu73Qsauj2y6fc0L8HYV3LC7U770U8z4SCeP2e39B5sVxrT1Q38TRI2m43mDLwt6Tn2c"]
      [:delete
       "zfl1J0Dx9upmuW0cVBACt3MSW0QwuIJDr0gEG9n0O5x0y4N1I2BXJQgi1p0UVhJ8yp0F53A443W5HPBgQRq"]
      [:delete "5QB"]
      [:retain 1273]]]]])

  (let [[[text operations]] foo
        t                  (make-text text)
        t'                 (reduce play t operations)
        text' (Text/text (Text/zipper t') (Text/length t'))]
    {:success? (= text text')
     :before text
     :operations operations
     :after text'
     :should-be (reduce play-naive text operations)})

  (zp
   (-> (make-text "0000000000")
       (Text/zipper)
       (Text/retain 10)))

  )

(comment
  (do

  (require '[onair.dev])
  (require '[andel.text])
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

  (def clj-tree (andel.text/make-text editor-impl))
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

    (defn iterate-char-seq [^CharSequence char-seq]
      (let [length (.length char-seq)]
        (loop [i 0]
          (when (< i length)
            (.charAt char-seq i)
            (recur (inc i))))))
    ;; eval here
    )

  (= (play-naive editor-impl operation)
     (andel.text/as-string (andel.text/play (andel.text/make-text editor-impl) operation))
     (let [t (play (Text/makeText editor-impl) operation)]
       (Text/text (Text/zipper t) (text-length t)))
     (let [t (play-transient (Text/makeText editor-impl) operation)]
       (Text/text (Text/zipper t) (text-length t))))

  (->> (iterate head (my-node not-so-immaculate-text))
       (take-while (fn [n] (= com.intellij.util.text.ImmutableText$CompositeNode (type n))))
       (count))

  (let [as (make-random-accesses editor-impl 10)]
    (= (random-access clj-tree text-clj as)
       (random-access java-tree text-java as)
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

      (prn "JAVA VERSION TRANSIENT")
      (onair.dev/benchmark
       (play-transient java-tree operation))

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
        (random-access not-so-immaculate-text text-intellij random-accesses)))
    (do
      (prn "CHAR SEQUENCE")
      (prn "CLJ VERSION")
      (onair.dev/benchmark
        (iterate-char-seq (andel.text/text->char-seq clj-tree)))

      (prn "JAVA VERSION")
      (onair.dev/benchmark
        (iterate-char-seq (Text$Sequence. java-tree)))

      (prn "IMMUTABLE TEXT")
      (onair.dev/benchmark
       (iterate-char-seq not-so-immaculate-text))))

  )


(comment


  (def editor-impl
    (slurp (clojure.java.io/file "../../community//platform/platform-impl/src/com/intellij/openapi/editor/impl/EditorImpl.java")))

  (def t
    (Text/makeText editor-impl))

  (Text/length t)

  (-> (Text/zipper t)
      (Text/scanToOffset 6084)
      (Text/delete 80)
      (Text/root)
      (Text/length))


  )