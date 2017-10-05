(ns andel.utils
  (:require [andel.text :as text]
            [andel.tree :as tree]))

(defn line-height [{:keys [height spacing] :as metrics}]
  (+ height spacing))

(defn pixels->grid-position
  "transforms relative position in pixels into absolute [line col] value
   CAUTION! col might be bigger, than length of line."
  [[pix-x pix-y] {:keys [metrics pos] :as viewport}]
  (let [[_ top-px] pos
        line-height (line-height metrics)
        font-width (:width metrics)
        top-line (int (/ top-px line-height))
        y-shift (- (* line-height (- (/ top-px line-height) top-line)))
        x pix-x
        y (- (- pix-y y-shift) (/ line-height 2))
        rel-line (Math/round (/ y line-height))
        abs-line (+ top-line rel-line)
        abs-col (Math/round (/ x font-width))]
    {:line abs-line
     :col abs-col}))

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

(defn line-col->pixels
  "transforms absolute [line col] value into relative poisition in pixels"
  [{:keys [line col]} start-line shift {:keys [height width] :as metrics}]
  (let [rel-line (- line start-line)
        pix-x (* col width)
        pix-y (+ shift (/ height 2) (* line height))]
    [pix-x pix-y]))

(defn offset->pixels
  "transforms absolute offset value into relative poisition in pixels"
  [offset start-line shift metrics text]
   (-> offset
       (offset->line-col text)
       (line-col->pixels start-line shift metrics)))

(defn line-number
  "transforms zipper pointer into line"
  [loc]
  (text/line loc))

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

(defn scan-to-next-line [loc]
  (text/scan-to-line-start loc (inc (line-number loc))))
