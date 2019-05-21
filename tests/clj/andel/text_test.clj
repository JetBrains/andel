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
  (.-length ^Text$TextMetrics (Rope/getMetrics node)))

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