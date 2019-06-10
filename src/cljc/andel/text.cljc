(ns andel.text
  (:refer-clojure :exclude [transient persistent!])
  (:import [andel Text Text$Sequence]
           [andel Rope Rope$Zipper]
           [java.lang CharSequence]))

(defn codepoints-count ^long [^String s]
  (.codePointCount s 0 (.length s)))

(defn transient [zipper]
  (Rope/toTransient zipper))

(defn persistent! [zipper]
  (Rope/toPersistent zipper))

(defn make-text [s]
  (Text/makeText s))

(defn zipper [tree]
  (Text/zipper tree))

(defn root [zipper]
  (Text/root zipper))

(defn offset ^long [zipper]
  (Text/offset zipper))

(defn geom-offset ^long [zipper]
  (Text/geomOffset zipper))

(defn line ^long [zipper]
  (Text/line zipper))

(defn char-offset ^long [zipper]
  (Text/charOffset zipper))

(defn scan-to-offset [zipper ^long i]
  (Text/scanToOffset zipper i))

(defn scan-to-geom-offset [zipper ^long i]
  (Text/scanToGeomOffset zipper i))

(defn scan-to-char-offset [zipper ^long i]
  (Text/scanToCharOffset zipper i))

(defn retain [zipper ^long l]
  (Text/retain zipper l))

(defn scan-to-line-start [zipper ^long n]
  (Text/scanToLineStart zipper n))

(defn distance-to-EOL ^long [loc]
  (let [next-loc (scan-to-line-start loc (inc (line loc)))
        len (- (offset next-loc)
               (offset loc))]
    (if (not (Rope/hasNext next-loc))
      len
      (dec len))))

(defn lines-count [t]
  (Text/linesCount t))

(defn text-length ^long [t]
  (Text/length t))

(defn chars-count [t]
  (Text/charsCount t))

(defn text ^String [zipper ^long l]
  (Text/text zipper l))

(defn as-string [text-tree]
  (Text/text (Text/zipper text-tree) (Text/length text-tree)))

(defn insert [zipper ^String s]
  (Text/insert zipper s))

(defn delete [zipper ^long l]
  (Text/delete zipper l))

(defn text-range [tree ^long from ^long to]
  (assert (<= from to) {:from from :to to})
  (if (= from to)
    ""
    (-> (Text/zipper tree)
        (Text/scanToOffset from)
        (Text/text (- to from)))))

(defn max-line-length ^long [text]
  (Text/maxLineLength text))

(defn skip-to-line-end [loc]
  (def loc loc)
  (let [offset (offset loc)
        delta (distance-to-EOL loc)]
    (scan-to-offset loc (+ offset delta))))

(defn skip-columns [loc ^long cols]
  (let [geom (geom-offset loc)
        cur-line (line loc)
        loc' (scan-to-geom-offset loc (+ geom cols))]
    (if (= cur-line (line loc'))
      loc'
      (skip-to-line-end loc))))

(defn column ^long [loc]
  (let [cur-line (line loc)
        start-loc (scan-to-line-start (zipper (root loc)) cur-line)]
    (- (geom-offset loc) (geom-offset start-loc))))

(defn ^CharSequence text->char-seq [t]
  (Text$Sequence. t))

(defn offset->char-offset ^long [text ^long offset]
  (-> (zipper text)
      (scan-to-offset offset)
      (char-offset)))

(defn char-offset->offset ^long [text ^long char-offset]
  (-> (zipper text)
      (scan-to-char-offset char-offset)
      (offset)))
