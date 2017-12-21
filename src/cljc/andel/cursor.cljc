(ns andel.cursor
  (:require [andel.tree :as tree]
            [andel.text :as text]
            [andel.array-list :as al]))

(defn- get-leaf-length [zipper]
  (assert (tree/leaf? zipper)
          "calling leaf-length on non-leaf")
  (-> zipper
      tree/node
      .-data
      count))

;; warning: breaks zipper accumulator
(defn- left [loc]
  (when (< 0 (.-idx loc))
    (tree/z-merge loc {:idx (dec (.-idx loc))
                       :acc nil
                       :o-acc nil})))

;; warning: breaks zipper accumulator
(defn- prev-loc [loc]
  (if (tree/end? loc)
    loc
    (or
     (and (tree/branch? loc) (tree/down loc))
     (left loc)
     (loop [^ZipperLocation p loc]
       (if-let [u (tree/up p)]
         (or (left u) (recur u))
         (tree/->zipper {:ops (.-ops loc)
                         :transient? (.-transient? loc)
                         :siblings (al/into-array-list [(tree/node p)])
                         :idx 0
                         :end? true}))))))

;; warning: breaks zipper accumulator
(defn- prev-leaf [loc]
  (let [loc (prev-loc loc)]
    (if (or (tree/leaf? (tree/node loc))
            (tree/end? loc))
      loc
      (recur loc))))

(defn make-cursor
  ([text offset]
   (-> text
       text/zipper
       (text/scan-to-offset offset)
       make-cursor))
  ([zipper]
   (let [node-offset (text/node-offset zipper)
         offset      (text/offset zipper)]
     {:zipper zipper
      :node-offset  node-offset
      :text-length  (text/text-length (tree/root zipper)) ;!!!!
      :inner-offset (- offset node-offset)
      :leaf-length  (get-leaf-length zipper)})))

(defn get-char [cursor]
  (when-let [{:keys [zipper inner-offset]} cursor]
    (-> zipper
        tree/node
        .-data
        (.charAt inner-offset))))

(defn next [{:keys [zipper node-offset text-length
                    inner-offset leaf-length] :as cursor}]
  (cond
    (< (inc inner-offset) leaf-length)
    (update cursor :inner-offset inc)

    (< (inc (+ node-offset inner-offset)) text-length)
    (let [next-leaf (tree/next-leaf zipper)]
      {:zipper       next-leaf
       :node-offset  (+ node-offset leaf-length)
       :text-length  text-length
       :inner-offset 0
       :leaf-length  (get-leaf-length next-leaf)})

    :else nil))

(defn prev [{:keys [zipper node-offset text-length
                    inner-offset leaf-length] :as cursor}]
  (cond
    (< 0 inner-offset)
    (update cursor :inner-offset dec)

    (< 0 (+ node-offset inner-offset))
    (let [prev-leaf   (prev-leaf zipper)
          leaf-length (get-leaf-length prev-leaf)]
      {:zipper       prev-leaf
       :node-offset  (- node-offset leaf-length)
       :text-length  text-length
       :inner-offset (dec leaf-length)
       :leaf-length  leaf-length})

    :else nil))

(defn offset [{:keys [node-offset inner-offset] :as cursor}]
  (+ node-offset inner-offset))
