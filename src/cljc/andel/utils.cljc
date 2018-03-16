(ns andel.utils
  (:require [andel.text :as text]
            [andel.tree :as tree]))

(defn line-height [{:keys [height spacing] :as metrics}]
  (+ height spacing))

(defn pixels->grid-position
  "transforms absolute position in pixels into absolute [line col] value
   CAUTION! col might be bigger, than length of line."
  [[x y] metrics]
  (let [line-height (line-height metrics)
        font-width (:width metrics)
        line (int (Math/floor (/ (double y) line-height)))
        col (int (Math/floor (/ (double (- x (:gutter-width metrics))) font-width)))]
    {:line line :col col}))

(defn grid-pos->offset
  "transforms [line col] value into absolute offset value"
  [{:keys [line col]} text]
  (let [line-loc (text/scan-to-line-start (text/zipper text) line)
        line-len (text/distance-to-EOL line-loc)
        line-offset (text/offset line-loc)
        text-length (text/text-length text)
        offset (min text-length (max 0 (+ line-offset (min line-len col))))]
    offset))

(defn line->offset
  "transforms line value into absolute offset value"
  [line text]
  (grid-pos->offset {:line line :col 0} text))

(defn offset->line
  "transforms absolute offset into absolute line value ignoring col"
  [offset text]
  (-> text
      (text/zipper)
      (text/scan-to-offset offset)
      (text/line)))

(defn line-length [line text]
  (text/distance-to-EOL (text/scan-to-line-start (text/zipper text) line)))

(defn offset->line-col
  "transforms absolute offset into absolute [line col] value"
  [offset text]
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
  [{:keys [line col]} text]
  (let [line-loc (line->loc line text)
        line-offset (text/offset line-loc)]
    (text/scan-to-offset line-loc (+ line-offset col))))

(defn line-number
  "transforms zipper pointer into line"
  [loc]
  (text/line loc))

(defn scan-to-next-line [loc]
  (text/scan-to-line-start loc (inc (line-number loc))))
