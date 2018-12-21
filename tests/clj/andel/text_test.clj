(ns andel.text-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as g]
            [clojure.test.check.properties :as prop]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [andel.text :as text]
            [andel.tree :as tree]))

(def op-frames-gen (g/vector
                    (g/one-of [(g/tuple
                                (g/return :retain) g/pos-int)
                               (g/tuple
                                (g/return :insert) g/string-alphanumeric)
                               (g/tuple
                                (g/return :delete) g/pos-int)])))

(defn random-ops [text frames]
  (loop [text text
         result []
         [[code arg :as op] & rest-ops] frames]
    (cond
      (empty? text) result
      (nil? op) (conj result [:retain (count text)])
      :else (let [arg (if (#{:retain :delete} code) (min arg (count text)) arg)]
              (case code
                :retain (recur (subs text arg)
                               (conj result [:retain arg])
                               rest-ops)
                :insert (recur text
                               (conj result [:insert arg])
                               rest-ops)
                :delete (recur (subs text arg)
                               (conj result [:delete (subs text 0 arg)])
                               rest-ops))))))

(def operation-gen
  (g/fmap (fn [[text frames]] [text (random-ops text frames)])
          (g/tuple g/string-alphanumeric op-frames-gen)))

(defn play-naive [text operation]
  (let [[idx text]
        (reduce (fn [[idx text] [code arg]]
                  (case code
                    :retain [(+ idx arg) text]
                    :delete [idx (str (subs text 0 idx) (subs text (+ idx (count arg))))]
                    :insert [(+ idx (count arg)) (str (subs text 0 idx)
                                                      arg
                                                      (subs text idx))]))
                [0 text] operation)]
    (assert (= idx (count text)))
    text))

(def play-test
  (prop/for-all [[text operation] operation-gen]
                (def last-op [text operation])
                (let [t (text/play (text/make-text text) operation)]
                  (= (text/text (text/zipper t) (text/text-length t))
                     (play-naive text operation)))))

(deftest generative
  (is (:result (tc/quick-check 3000 play-test))))

