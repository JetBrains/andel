(ns andel.cursor-test
  (:require [clojure.test.check.generators :as g]
            [clojure.test.check.properties :as prop]
            [clojure.test.check :as tc]
            [clojure.test :refer :all]
            [andel.text :as text]
            [andel.cursor :as cursor])
  (:import [andel Cursor$ImmutableCursor Cursor$TransientCursor Cursor Text]))

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
              (let [c (-> (text/zipper t)
                          (text/scan-to-offset (inc offset))
                          (text/text 1)
                          (.codePointAt 0))]
                [c [t (inc offset)]])
              [nil [t offset]])
      :prev (if (<= 0 (dec offset))
              (let [c (-> (text/zipper t)
                          (text/scan-to-offset (dec offset))
                          (text/text 1)
                          (.codePointAt 0))]
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
    :next (if-let [next-cursor (cursor/next! t-cursor)]
            [(cursor/get-char next-cursor) next-cursor]
            [nil t-cursor])
    :prev (if-let [prev-cursor (cursor/prev! t-cursor)]
            [(cursor/get-char prev-cursor) prev-cursor]
            [nil t-cursor])))

(defn play-java-cursor [jcursor op]
  (case op
    :next (if-let [next-cursor (Cursor/next jcursor)]
            [(.getChar next-cursor) next-cursor]
            [nil jcursor])
    :prev (if-let [prev-cursor (Cursor/prev jcursor)]
            [(.getChar prev-cursor) prev-cursor]
            [nil jcursor])))

(defn play-java-tcursor [^Cursor$TransientCursor jtcursor op]
  (case op
    :next (if-let [next-cursor (.next jtcursor)]
            [(.getChar next-cursor) next-cursor]
            [nil jtcursor])
    :prev (if-let [prev-cursor (.prev jtcursor)]
            [(.getChar prev-cursor) prev-cursor]
            [nil jtcursor])))

(def cursor-test
  (prop/for-all
   [[t o] text-offset-pair-gen
    ops multiple-operations-gen]
   (let [cursor (cursor/make-cursor t o)
         t-cursor (cursor/transient cursor)
         j-cursor (Cursor$ImmutableCursor.
                    (Text/makeText (text/as-string t))
                    o)
         jt-cursor (Cursor$TransientCursor.
                    (Text/makeText (text/as-string t))
                    o)
         result (get
                 (reduce
                  (fn [[naive cursor t-cursor j-cursor jt-cursor acc] op]
                    (let [[cn naive'] (play-naive naive op)
                          [cc cursor'] (play-cursor cursor op)
                          [ct t-cursor'] (play-transient-cursor
                                          t-cursor op)
                          [cj j-cursor'] (play-java-cursor
                                          j-cursor op)
                          [cjt jt-cursor'] (play-java-tcursor
                                             jt-cursor op)]
                      [naive'
                       cursor'
                       t-cursor'
                       j-cursor'
                       jt-cursor'
                       (and acc (= cc cn ct cj cjt))]))
                  [[t o]
                   cursor
                   t-cursor
                   j-cursor
                   jt-cursor
                   true]
                  ops)
                 2)]
     result)))

(tc/quick-check 10000 cursor-test)