(ns andel.utils
  (:require [andel.text :as text]
            [andel.tree :as tree]))

(defn line-height [{:keys [height spacing] :as metrics}]
  (+ height spacing))

(defn pixels->grid-position
  "transforms relative position in pixels into absolute [line col] value
   CAUTION! col might be bigger, than length of line."
  [[pix-x pix-y] start-line shift {:keys [width] :as metrics}]
  (let [line-height (line-height metrics)
        x pix-x
        y (- (- pix-y shift) (/ line-height 2))
        rel-line (Math/round (/ y line-height))
        abs-line (+ start-line rel-line)
        abs-col (Math/round (/ x width))]
    {:line abs-line
     :col abs-col}))

(defn line-col->offset
  "transforms [line col] value into absolute offset value"
  [{:keys [line col]} text]
  (let [line-loc (text/scan-to-line (text/zipper text) line)
        line-len (text/line-length line-loc)
        line-offset (text/offset line-loc)
        text-length (text/text-length text)
        offset (min (dec text-length) (max 0 (+ line-offset (min line-len col))))]
    offset))

(defn line->offset
  "transforms line value into absolute offset value"
  [line text]
  (line-col->offset {:line line :col 0} text))

(defn pixels->offset
  "transforms relative position in pixels into absolute offset value"
  [[pix-x pix-y] start-line shift metrics text]
  (-> [pix-x pix-y]
      (pixels->grid-position start-line shift metrics)
      (line-col->offset text)))

(defn offset->line
  "transforms absolute offset into absolute line value ignoring col"
  [offset text]
  (-> text
      (text/zipper)
      (text/scan-to-offset offset)
      (text/line)))

(defn line->length [line text]
  (text/line-length (text/scan-to-line (text/zipper text) line)))

(defn offset->line-col
  "transforms absolute offset into absolute [line col] value"
  [offset text]
  (let [line (offset->line offset text)
        line-offset (line-col->offset {:line line :col 0} text)
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

(defn offset->loc
  "transforms absolute offset into zipper pointer"
  [offset text]
  (-> text
      (text/zipper)
      (text/scan-to-offset offset)))

(defn loc->offset
  "transforms zipper pointer into offset"
  [loc]
  (text/offset loc))

(defn loc->line
  "transforms zipper pointer into line"
  [loc]
  (text/line loc))

(defn line->loc
  "transforms absolute line into zipper pointer"
  [line text]
   (-> text
       (text/zipper)
       (text/scan-to-line line)))

(defn line-col->loc
  "transforms absolute [line col] into zipper pointer"
  [{:keys [line col]} text]
  (let [line-loc (line->loc line text)
        line-offset (loc->offset line-loc)]
    (text/scan-to-offset line-loc (+ line-offset col))))

(defn last-line?
  [line text]
  (-> (line->loc line text)
      (tree/end?)))

(defn next-line-loc
  [line text]
  (if (last-line? line text)
    (line->loc line text)
    (let [line-loc (line->loc line text)
          offset (loc->offset line-loc)
          line-length (text/line-length line-loc)
          next-line-loc (text/scan-to-offset line-loc (+ offset line-length 1))]
      next-line-loc)))

(defn prev-line-loc
  [line text]
  (if (= line 0)
    0
    (let [prev-line-end (- (line->offset line text) 1)
          prev-line-loc (line->loc (offset->line prev-line-end text) text)]
      prev-line-loc)))
