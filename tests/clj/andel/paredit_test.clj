(ns andel.paredit-test
  (:require [clojure.test :refer :all]
            [andel.core :as core]
            [andel.text :as text]
            [andel.paredit :refer :all]))

(defn preprocess-test-string [s]
  (let [caret-offset (.indexOf s "|")]
    {:caret-offset caret-offset
     :test-string  (.replace s "|" "")}))

(defn andel-state->offset-and-string [s]
  {:caret-offset (core/caret-offset s)
   :test-string  (str
                  (core/text-at-offset
                   s 0 (text/text-length
                        (get-in s [:document :text]))))})

(defn mock-andel-state [{:keys [caret-offset test-string]}]
  (-> (core/make-editor-state nil)
      (assoc-in [:document :text] (text/make-text test-string))
      (assoc-in [:editor :caret :offset] caret-offset)))

(defn paredit-test [op str1 str2]
  (-> str1
      preprocess-test-string
      mock-andel-state
      op
      andel-state->offset-and-string
      (= (preprocess-test-string str2))))

(deftest slurp-forward-1
  (is (paredit-test slurp-forward "(|) a " "(| a) ")))

(deftest slurp-forward-2
  (is (paredit-test slurp-forward "(|) ()" "(| ())")))

(deftest slurp-backward-1
  (is (paredit-test slurp-backward " a (|)" " (a |)")))

(deftest slurp-backward-2
  (is (paredit-test slurp-backward "() (|)" "(() |)")))

(run-all-tests)
