(ns andel.paredit-test
  (:require [clojure.test :refer :all]
            [andel.core :as core]
            [andel.text :as text]
            [andel.paredit :refer :all]))

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
  (-> (core/make-editor-state nil nil)
      (assoc-in [:document :text] (text/make-text test-string))
      (assoc-in [:editor :caret :offset] caret-offset)))

(defmacro paredit-test [op str1 str2]
  `(let [res-str1#  (-> ~str1
                        preprocess-test-string
                        mock-andel-state
                        ~op
                        andel-state->offset-and-string
                        offset-and-string->test-string)]
     (is (= res-str1# ~str2))))

(deftest slurp-forward-test
  (paredit-test slurp-forward "(|) a" "(| a)")
  (paredit-test slurp-forward "(|) aaa" "(| aaa)")
  (paredit-test slurp-forward "(|) ()" "(| ())")
  (paredit-test slurp-forward "(|) (   )" "(| (   ))")
  (paredit-test slurp-forward "(|) \"aa bb\"" "(| \"aa bb\")"))

(deftest slurp-backward-test
  (paredit-test slurp-backward "a (|)" "(a |)")
  (paredit-test slurp-backward "aaa (|)" "(aaa |)")
  (paredit-test slurp-backward "() (|)" "(() |)")
  (paredit-test slurp-backward "(   ) (|)" "((   ) |)")
  (paredit-test slurp-backward "\"aa bb\" (|)" "(\"aa bb\" |)"))

(deftest barf-forward-test
  (paredit-test barf-forward "(|a)" "()|a")
  (paredit-test barf-forward "(|aaa)" "()|aaa")
  (paredit-test barf-forward "(|())" "()|()")
  (paredit-test barf-forward "(|(   ))" "()|(   )")
  (paredit-test barf-forward "(|\"aa  aa\")" "()|\"aa  aa\""))

(deftest barf-backward-test
  (paredit-test barf-backward "(a|)" "a(|)")
  (paredit-test barf-backward "(aaa|)" "aaa(|)")
  (paredit-test barf-backward "(()|)" "()(|)")
  (paredit-test barf-backward "((   )|)" "(   )(|)")
  (paredit-test barf-backward "(\"aa bb\"|)" "\"aa bb\"(|)"))

(deftest splice-test
  (paredit-test splice "(|)" "|")
  (paredit-test splice "[(|)]" "[|]")
  (paredit-test splice "(aaa | bbb)" "aaa | bbb")
  (paredit-test splice "(|[] () () {})" "|[] () () {}"))

(deftest splice-kill-left-test
  (paredit-test splice-kill-left "(|)" "|")
  (paredit-test splice-kill-left "(()|[])" "|[]")
  (paredit-test splice-kill-left "(aaa | bbb)" "| bbb"))

(deftest splice-kill-right-test
  (paredit-test splice-kill-right "(|)" "|")
  (paredit-test splice-kill-right "([]|{})" "[]|")
  (paredit-test splice-kill-right "(aaa | bbb)" "aaa |"))

(deftest navigate-next-form-test
  (paredit-test navigate-next-form "|" "|")
  (paredit-test navigate-next-form "|aaa" "aaa|")
  (paredit-test navigate-next-form "a|aa" "aaa|")
  (paredit-test navigate-next-form "(|)" "()|")
  (paredit-test navigate-next-form "()| ()" "() ()|")
  (paredit-test navigate-next-form "(()| ())" "(() ()|)"))

(deftest navigate-next-form-down-test
  (paredit-test navigate-next-form-down "|" "|")
  (paredit-test navigate-next-form-down "(|)" "(|)")
  (paredit-test navigate-next-form-down "|()" "(|)")
  (paredit-test navigate-next-form-down "| aaa ()" " aaa (|)")
  (paredit-test navigate-next-form-down "(| ())" "( (|))")
  (paredit-test navigate-next-form-down "(| aaa ())" "( aaa (|))")
  (paredit-test navigate-next-form-down "( aaa (|))" "( aaa (|))"))

(deftest navigate-next-form-up-test
  (paredit-test navigate-next-form-up "|" "|")
  (paredit-test navigate-next-form-up "(|)" "()|")
  (paredit-test navigate-next-form-up "((|aaa))" "((aaa)|)"))

(deftest navigate-prev-form-down-test
  (paredit-test navigate-prev-form-down "|" "|")
  (paredit-test navigate-prev-form-down "(|)" "(|)")
  (paredit-test navigate-prev-form-down "()|" "(|)")
  (paredit-test navigate-prev-form-down "() aaa|" "(|) aaa")
  (paredit-test navigate-prev-form-down "(()|)" "((|))")
  (paredit-test navigate-prev-form-down "(() aaa|)" "((|) aaa)")
  (paredit-test navigate-prev-form-down "((|) aaa)" "((|) aaa)"))

(deftest navigate-prev-form-up-test
  (paredit-test navigate-prev-form-up "|" "|")
  (paredit-test navigate-prev-form-up "(|)" "|()")
  (paredit-test navigate-prev-form-up "((aaa|))" "(|(aaa))"))

(deftest navigate-line-start-test
  (paredit-test navigate-line-start "aaa|" "|aaa")
  (paredit-test navigate-line-start "  aaa|" "  |aaa")
  (paredit-test navigate-line-start "  |aaa" "|  aaa")
  (paredit-test navigate-line-start "|  aaa" "  |aaa"))

(deftest backspace-test
  (paredit-test backspace "|" "|")
  (paredit-test backspace "a|" "|")
  (paredit-test backspace "()|" "|")
  (paredit-test backspace "(|)" "|")
  (paredit-test backspace "(aaa)|" "(aaa|)")
  (paredit-test backspace "(aaa|)" "(aa|)"))

(deftest delete-test
  (paredit-test delete "|" "|")
  (paredit-test delete "|a" "|")
  (paredit-test delete "|()" "|")
  (paredit-test delete "|(aaa)" "|(aaa)")
  (paredit-test delete "(|aaa)" "(|aa)"))

(comment (run-tests))
