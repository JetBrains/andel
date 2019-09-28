(ns ^:lean-ns andel.text
  (:refer-clojure :exclude [transient persistent!])
  (:import [andel.text Text TextZipper]
           [java.lang CharSequence]))

(defn codepoints-count ^long [^String s]
  (.codePointCount s 0 (.length s)))

(defn transient ^TextZipper [^TextZipper zipper]
  (.asTransient zipper))

(defn persistent! ^TextZipper [^TextZipper zipper]
  (.asPersistent zipper))

(defn make-text ^Text [^String s]
  (Text/makeText s))

(defn zipper ^TextZipper [^Text tree]
  (.zipper tree))

(defn root ^Text [^TextZipper zipper]
  (.makeText zipper))

(defn offset ^long [^TextZipper zipper]
  (.codePointsOffset zipper))

(defn geom-offset ^long [^TextZipper zipper]
  (.geomOffset zipper))

(defn line ^long [^TextZipper zipper]
  (.lineNumber zipper))

(defn char-offset ^long [^TextZipper zipper]
  (.charOffset zipper))

(defn scan-to-offset [^TextZipper zipper ^long i]
  (.scanToCodepoint zipper i))

(defn scan-to-geom-offset [^TextZipper zipper ^long i]
  (.scanToGeomOffset zipper i))

(defn scan-to-char-offset [^TextZipper zipper ^long i]
  (.scanToCharOffset zipper i))

(defn retain [^TextZipper zipper ^long l]
  (.retain zipper l))

(defn scan-to-line-start [^TextZipper zipper ^long n]
  (.scanToLineStart zipper n))

(defn text-length ^long [^Text t]
  (.codePointsCount t))

(defn lines-count ^long [^Text t]
  (.linesCount t))

(defn distance-to-EOL ^long [loc]
  (let [t (root loc)]
    (if (< (line loc) (dec (lines-count t)))
      (let [next-loc (scan-to-line-start loc (inc (line loc)))]
        (dec (- (offset next-loc)
                (offset loc))))
      (- (text-length t) (offset loc)))))

(defn chars-count ^long [^Text t]
  (.charsCount t))

(defn text ^String [^TextZipper zipper ^long l]
  (.text zipper l))

(defn consume-text [^TextZipper zipper ^long l consumer]
  (if (instance? StringBuilder consumer)
    (.consume zipper l (reify andel.text.TextConsumer
                              (consume [_ str from to]
                                       (.append ^StringBuilder consumer str from to))))
    (.consume zipper l consumer)))

(defn as-string [text-tree]
  (text (zipper text-tree) (text-length text-tree)))

(defn insert [^TextZipper zipper ^String s]
  (.insert zipper s))

(defn delete [^TextZipper zipper ^long l]
  (.delete zipper l))

(defn text-range [^Text tree ^long from ^long to]
  (assert (<= from to) {:from from :to to})
  (if (= from to)
    ""
    (-> (zipper tree)
        (scan-to-offset from)
        (text (- to from)))))

(defn max-line-length ^long [^Text text]
  (.maxLineLength text))

(defn scan-to-line-end [loc]
  (let [offset (offset loc)
        delta (distance-to-EOL loc)]
    (scan-to-offset loc (+ offset delta))))

(defn skip-columns [loc ^long cols]
  (let [geom (geom-offset loc)
        cur-line (line loc)
        text-geom-len (.-geometricLength ^andel.impl.text.TextMetrics (.-metrics (.-rope (root loc))))
        loc' (scan-to-geom-offset loc (min (+ geom cols) text-geom-len))]
    (if (= cur-line (line loc'))
      loc'
      (scan-to-line-end loc))))

(defn column ^long [loc]
  (let [cur-line (line loc)
        start-loc (scan-to-line-start (zipper (root loc)) cur-line)]
    (- (geom-offset loc) (geom-offset start-loc))))

(defn ^CharSequence text->char-seq
  ([^Text t from to]
   (.charSequence t from to))
  ([^Text t]
   (.charSequence t)))

(defn offset->char-offset ^long [text ^long offset]
  (-> (zipper text)
      (scan-to-offset offset)
      (char-offset)))

(defn char-offset->offset ^long [text ^long char-offset]
  (-> (zipper text)
      (scan-to-char-offset char-offset)
      (offset)))

(defn line->offset ^long [line text]
  (offset (scan-to-line-start (zipper text) line)))

(defn offset->line ^long [offset text]
  (-> (zipper text)
      (scan-to-offset offset)
      (line)))

(defn play-operation [text-tree operation]
  (root
   (reduce (fn [z [op arg]]
             (case op
               :insert (insert z arg)
               :delete (delete z (count arg))
               :retain (let [co (char-offset z)]
                         (if (= (+ co arg) (text-length text-tree))
                           z
                           (scan-to-char-offset z (+ co arg))))))
           (zipper text-tree) operation)))
