(ns andel.cursor-test
  (:require [clojure.test.check.generators :as g]
            [clojure.test.check.properties :as prop]
            [clojure.test.check :as tc]
            [clojure.test :refer :all]
            [andel.text :as text]
            [andel.cursor :as cursor]))

(def single-operation-gen
  (g/one-of [(g/return :next)
             (g/return :prev)]))

(def multiple-operations-gen
  (g/not-empty (g/vector single-operation-gen)))

(def text-gen
  (g/fmap text/make-text
          (g/not-empty g/string-ascii)))

(def text-offset-pair-gen
  (g/bind text-gen
          (fn [t]
            (let [t-l (text/text-length t)]
              (g/fmap (fn [o] [t o])
                      (g/large-integer* {:min 0
                                         :max (max 0 (dec t-l))}))))))

(defn play-naive [[t offset] op]
  (let [text-length (text/text-length t)]
    (case op
      :next (if (< (inc offset) text-length)
              (let [c (nth (text/text->char-seq t) (inc offset))]
                [c [t (inc offset)]])
              [nil [t offset]])
      :prev (if (<= 0 (dec offset))
              (let [c (nth (text/text->char-seq t) (dec offset))]
                [c [t (dec offset)]])
              [nil [t offset]]))))

(defn play-cursor [cursor op]
  (case op
    :next (if-let [next-cursor (cursor/next cursor)]
            [(cursor/get-char next-cursor) next-cursor]
            [nil cursor])
    :prev (if-let [prev-cursor (cursor/prev cursor)]
            [(cursor/get-char prev-cursor) prev-cursor]
            [nil cursor])))

(defn play-transient-cursor [t-cursor op]
  (case op
    :next (do (cursor/next! t-cursor)
              [(.getChar t-cursor) t-cursor])
    :prev (do (cursor/prev! t-cursor)
              [(.getChar t-cursor) t-cursor])))

(def cursor-test
  (prop/for-all
   [[t o] text-offset-pair-gen
    ops multiple-operations-gen]
   (let [cursor (cursor/make-cursor t o)
         t-cursor (cursor/transient cursor)
         result (get
                 (reduce
                  (fn [[naive cursor acc] op]
                    (let [[cn naive'] (play-naive naive op)
                          [cc cursor'] (play-cursor cursor op)
                          [ct t-cursor'] (play-transient-cursor
                                          t-cursor op)]
                      [naive'
                       cursor'
                       (and acc (= cc cn ct))]))
                  [[t o]
                   (cursor/make-cursor t o)
                   true]
                  ops)
                 2)]
     result)))

(tc/quick-check 10000 cursor-test)
