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
  (is (paredit-test slurp-forward "(|) a" "(| a)")))

(deftest slurp-forward-2
  (is (paredit-test slurp-forward "(|) aaa" "(| aaa)")))

(deftest slurp-forward-3
  (is (paredit-test slurp-forward "(|) ()" "(| ())")))

(deftest slurp-forward-4
  (is (paredit-test slurp-forward "(|) (   )" "(| (   ))")))

(deftest slurp-forward-5
  (is (paredit-test slurp-forward "(|) \"aa bb\"" "(| \"aa bb\")")))

(deftest slurp-backward-1
  (is (paredit-test slurp-backward "a (|)" "(a |)")))

(deftest slurp-backward-2
  (is (paredit-test slurp-backward "aaa (|)" "(aaa |)")))

(deftest slurp-backward-3
  (is (paredit-test slurp-backward "() (|)" "(() |)")))

(deftest slurp-backward-4
  (is (paredit-test slurp-backward "(   ) (|)" "((   ) |)")))

(deftest slurp-backward-5
  (is (paredit-test slurp-backward "\"aa bb\" (|)" "(\"aa bb\" |)")))

(deftest barf-forward-1
  (is (paredit-test barf-forward "(|a)" "()|a")))

(deftest barf-forward-2
  (is (paredit-test barf-forward "(|aaa)" "()|aaa")))

(deftest barf-forward-3
  (is (paredit-test barf-forward "(|())" "()|()")))

(deftest barf-forward-4
  (is (paredit-test barf-forward "(|(   ))" "()|(   )")))

(deftest barf-forward-5
  (is (paredit-test barf-forward "(|\"aa  aa\")" "()|\"aa  aa\"")))

(deftest barf-backward-1
  (is (paredit-test barf-backward "(a|)" "a(|)")))

(deftest barf-backward-2
  (is (paredit-test barf-backward "(aaa|)" "aaa(|)")))

(deftest barf-backward-3
  (is (paredit-test barf-backward "(()|)" "()(|)")))

(deftest barf-backward-4
  (is (paredit-test barf-backward "((   )|)" "(   )(|)")))

(deftest barf-backward-5
  (is (paredit-test barf-backward "(\"aa bb\"|)" "\"aa bb\"(|)")))

(deftest navigate-line-start-1
  (is (paredit-test navigate-line-start "aaa|" "|aaa")))

(deftest navigate-line-start-2
  (is (paredit-test navigate-line-start "  aaa|" "  |aaa")))

(deftest navigate-line-start-3
  (is (paredit-test (comp navigate-line-start navigate-line-start) "  aaa|" "|  aaa")))
