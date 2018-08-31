(ns andel.andel-test
  (:require [clojure.test :refer :all]
            [andel.core :as core]
            [andel.text :as text]
            [andel.controller :refer :all]))

(defn preprocess-test-string [s]
  (let [caret-offset (.indexOf s "|")]
    {:caret-offset caret-offset
     :test-string  (.replace s "|" "")}))

(defn offset-and-string->test-string [{:keys [caret-offset test-string]}]
  (str (subs test-string 0 caret-offset) \| (subs test-string caret-offset)))

(defn andel-state->offset-and-string [s]
  (let [text (get-in s [:document :text])]
    {:caret-offset (core/caret-offset s)
     :test-string  (str
                    (core/text-at-offset
                     text 0 (text/text-length text)))}))

(defn mock-andel-state [{:keys [caret-offset test-string]}]
  (-> (core/make-editor-state nil)
      (assoc-in [:document :text] (text/make-text test-string))
      (assoc-in [:editor :caret :offset] caret-offset)))

(defmacro andel-test [op str1 str2 & args]
  `(let [res-str1#  (-> ~str1
                        preprocess-test-string
                        mock-andel-state
                        (#(apply ~op % [~@args]))
                        andel-state->offset-and-string
                        offset-and-string->test-string)]
     (is (= res-str1# ~str2))))

(deftest backspace-test
  (andel-test backspace "|" "|")
  (andel-test backspace "aaa|" "aa|")
  (andel-test backspace "|aaa" "|aaa"))

(deftest delete-test
  (andel-test delete "|" "|")
  (andel-test delete "aa|a" "aa|")
  (andel-test delete "aaa|" "aaa|"))

(deftest type-in-test
  (andel-test type-in "|" "|" "")
  (andel-test type-in "|" "42|" "42"))

(deftest end-test
  (andel-test end "|" "|" false)
  (andel-test end "|aaa" "aaa|" false)
  (andel-test end "|aaa\nbbb" "aaa|\nbbb" false))

(deftest set-caret-at-offset-test
  (andel-test set-caret-at-offset "|" "|" 0 false)
  (andel-test set-caret-at-offset "|0123456789" "01234|56789" 5 false))

(deftest home-test
  ;; TODO: fix this case
  #_(andel-test home "|" "|" false)
  (andel-test home "aaa|" "|aaa" false)
  (andel-test home "   aaa|" "   |aaa" false)
  (andel-test home "   |aaa" "|   aaa" false))

(deftest move-caret-test
  (andel-test move-caret "|" "|" :right false)
  (andel-test move-caret "|" "|" :left false)
  (andel-test move-caret "|" "|" :up false)
  (andel-test move-caret "|" "|" :down false)
  (andel-test move-caret "a|a" "aa|" :right false)
  (andel-test move-caret "a|a" "|aa" :left false)
  (andel-test move-caret "aa\nb|b" "a|a\nbb" :up false)
  (andel-test move-caret "a|a\nbb" "aa\nb|b" :down false)
  (andel-test move-caret "|" "|" :word-forward false)
  (andel-test move-caret "|aaa" "aaa|" :word-forward false)
  (andel-test move-caret "|  aaa" "  |aaa" :word-forward false)
  (andel-test move-caret "|  ;aaa" "  |;aaa" :word-forward false)
  (andel-test move-caret "|;aaa" ";|aaa" :word-forward false)
  (andel-test move-caret "|" "|" :word-backward false)
  (andel-test move-caret "aaa|" "|aaa" :word-backward false)
  (andel-test move-caret "aaa  |" "aaa|  " :word-backward false)
  (andel-test move-caret "aaa;  |" "aaa;|  " :word-backward false)
  (andel-test move-caret "aaa;|" "aaa|;" :word-backward false))

(comment (run-tests))
