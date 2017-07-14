(ns andel.utils
  (:require [andel.text :as text]
            [andel.tree :as tree]))

(defn pixels->line-col
  "transforms relative position in pixels into absolute [line col] value"
  [[pix-x pix-y] start-line shift {:keys [height width] :as metrics}]
  (let [x pix-x
        y (- (- pix-y shift) (/ height 2))
        rel-line (Math/round (/ y height))
        abs-line (+ start-line rel-line)
        abs-col (Math/round (/ x width))]
  [abs-line abs-col]))

(defn line-col->offset
  "transforms absolute [line col] value into absolute offset value"
  [[line col] {:keys [text] :as state}]
  (-> text
      (text/zipper)
      (text/scan-to-line line)
      (text/offset)
      (+ col)))

(defn line->offset
  "transforms line value into absolute offset value"
  [line {:keys [text] :as state}]
  (line-col->offset [line 0] state))

(defn pixels->offset
  "transforms relative position in pixels into absolute offset value"
  [[pix-x pix-y] start-line shift metrics state]
  (-> [pix-x pix-y]
      (pixels->line-col start-line shift metrics)
      (line-col->offset state)))

(defn offset->line
  "transforms absolute offset into absolute line value ignoring col"
  [offset {:keys [text] :as state}]
  (-> text
      (text/zipper)
      (text/scan-to-offset offset)
      (text/line)))

(defn offset->line-col
  "transforms absolute offset into absolute [line col] value"
  [offset {:keys [text] :as state}]
  (let [line (offset->line offset state)
        line-offset (line-col->offset [line 0] state)
        col (- offset line-offset)]
    [line col]))

(defn line-col->pixels
  "transforms absolute [line col] value into relative poisition in pixels"
  [[line col] start-line shift {:keys [height width] :as metrics}]
  (let [rel-line (- line start-line)
        pix-x (* col width)
        pix-y (+ shift (/ height 2) (* line height))]
    [pix-x pix-y]))

(defn offset->pixels
  "transforms absolute offset value into relative poisition in pixels"
  [offset start-line shift metrics state]
   (-> offset
       (offset->line-col state)
       (line-col->pixels start-line shift metrics)))

(defn offset->loc
  "transforms absolute offset into zipper pointer"
  [offset {:keys [text] :as state}]
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
  [line {:keys [text] :as state}]
   (-> text
       (text/zipper)
       (text/scan-to-line line)))

(defn line-col->loc
  "transforms absolute [line col] into zipper pointer"
  [[line col] {:keys [text] :as state}]
  (let [line-loc (line->loc line state)
        line-offset (loc->offset line-loc)]
    (text/scan-to-offset line-loc (+ line-offset col))))

(defn last-line? [line state]
  (-> (line->loc line state)
      (tree/end?)))

(defn next-line-loc
  [line state]
  (if (last-line? line state)
    line
    (let [line-loc (line->loc line state)
          offset (loc->offset line-loc)
          line-length (text/line-length line-loc)
          next-line-loc (text/scan-to-offset (+ offset line-length 1) line-loc)]
      next-line-loc)))

(defn prev-line-loc
  [line state]
  (if (= line 0)
    0
    (let [prev-line-end (- (line->offset line state) 1)
          prev-line-loc (line->loc (offset->line prev-line-end state) state)]
      prev-line-loc)))
