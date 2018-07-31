(ns andel.utils
  (:require [andel.text :as text]
            [andel.tree :as tree]))

(defn line-height ^double [{:keys [^double height ^double spacing] :as metrics}]
  (+ height spacing))

(defn pixels->grid-position
  "transforms absolute position in pixels into absolute [line col] value
   CAUTION! col might be bigger, than length of line."
  [[^long x ^long y] metrics]
  (let [line-height (line-height metrics)
        line (int (Math/floor (/ (double y) line-height)))
        col (int (Math/round (/ (double (max 0 (- x ^double (:gutter-width metrics))))
                                ^double (:width metrics))))]
    {:line line :col col}))

(defn offset->geom-offset ^long [zipper ^long offset]
  (-> zipper
      (text/scan-to-offset offset)
      (text/geom-offset)))

(defn selection-to-geom [line-zipper [from to :as selection]]
  (let [line-start (text/offset line-zipper)
        line-start-geom (text/geom-offset line-zipper)
        from-abs (+ ^long from line-start)
        zipper-from (text/scan-to-offset line-zipper from-abs)
        from-geom (- (text/geom-offset zipper-from) line-start-geom)
        to-geom (if (= to :infinity)
                  :infitiy
                  (- (text/geom-offset (text/scan-to-offset zipper-from (+ ^long to line-start)))
                     line-start-geom))]
    [from-geom to-geom]))

(defn grid-pos->offset
  "transforms [line col] value into absolute offset value"
  ^long [{:keys [^long line ^long col]} text]
  (let [line-loc (text/scan-to-line-start (text/zipper text) line)
        line-len (text/distance-to-EOL line-loc)
        line-offset (text/offset line-loc)
        line-geom-offset (text/geom-offset line-loc)
        col-loc (text/scan-to-geom-offset line-loc (+ col line-geom-offset))
        col-offset (text/offset col-loc)
        text-length (text/text-length text)
        offset (min text-length (max 0 (min col-offset (+ line-offset line-len))))]
    offset))

(defn line->offset
  "transforms line value into absolute offset value"
  ^long [line text]
  (grid-pos->offset {:line line :col 0} text))

(defn line->from-to-offsets [^long line text]
  (let [from (line->offset line text)
        to (dec (line->offset (inc line) text))]
    [from to]))

(defn offset->line
  "transforms absolute offset into absolute line value ignoring col"
  ^long [offset text]
  (-> text
      (text/zipper)
      (text/scan-to-offset offset)
      (text/line)))

(defn line-length ^long [line text]
  (text/distance-to-EOL (text/scan-to-line-start (text/zipper text) line)))

(defn offset->line-col
  "transforms absolute offset into absolute [line col] value"
  [^long offset text]
  (let [line (offset->line offset text)
        line-offset (grid-pos->offset {:line line :col 0} text)
        col (- offset line-offset)]
    {:line line
     :col col}))

(defn line->loc
  "transforms absolute line into zipper pointer"
  [line text]
   (-> text
       (text/zipper)
       (text/scan-to-line-start line)))

(defn line-col->loc
  "transforms absolute [line col] into zipper pointer"
  [{:keys [^long line ^long col]} text]
  (let [line-loc (line->loc line text)
        line-offset (text/offset line-loc)]
    (text/scan-to-offset line-loc (+ line-offset col))))

(defn line-number
  "transforms zipper pointer into line"
  ^long [loc]
  (text/line loc))

(defn scan-to-next-line [loc]
  (text/scan-to-line-start loc (inc (line-number loc))))

(defn sets-intersect? [s1 s2]
  (if (<= (count s1) (count s2))
    (reduce (fn [r x] (if (contains? s2 x) (reduced true) r)) false s1)
    (sets-intersect? s2 s1)))
