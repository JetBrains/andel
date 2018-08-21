(ns andel.paredit-test
  (:require [clojure.test :refer :all]
            [andel.andel-test :refer [andel-test]]
            [andel.paredit :refer :all]))

(deftest slurp-forward-test
  (andel-test slurp-forward "(|) a" "(| a)")
  (andel-test slurp-forward "(|) aaa" "(| aaa)")
  (andel-test slurp-forward "(|) ()" "(| ())")
  (andel-test slurp-forward "(|) (   )" "(| (   ))")
  (andel-test slurp-forward "(|) \"aa bb\"" "(| \"aa bb\")")
  (andel-test slurp-forward "((|)) ()" "((|) ())")
  (andel-test slurp-forward "((|)) aaa" "((|) aaa)"))

(deftest slurp-backward-test
  (andel-test slurp-backward "a (|)" "(a |)")
  (andel-test slurp-backward "aaa (|)" "(aaa |)")
  (andel-test slurp-backward "() (|)" "(() |)")
  (andel-test slurp-backward "(   ) (|)" "((   ) |)")
  (andel-test slurp-backward "\"aa bb\" (|)" "(\"aa bb\" |)")
  (andel-test slurp-backward "() ((|))" "(() (|))")
  (andel-test slurp-backward "aaa ((|))" "(aaa (|))"))

(deftest barf-forward-test
  (andel-test barf-forward "(|a)" "()|a")
  (andel-test barf-forward "(|aaa)" "()|aaa")
  (andel-test barf-forward "(|())" "()|()")
  (andel-test barf-forward "(|(   ))" "()|(   )")
  (andel-test barf-forward "(|\"aa  aa\")" "()|\"aa  aa\""))

(deftest barf-backward-test
  (andel-test barf-backward "(a|)" "a(|)")
  (andel-test barf-backward "(aaa|)" "aaa(|)")
  (andel-test barf-backward "(()|)" "()(|)")
  (andel-test barf-backward "((   )|)" "(   )(|)")
  (andel-test barf-backward "(\"aa bb\"|)" "\"aa bb\"(|)"))

(deftest splice-test
  (andel-test splice "(|)" "|")
  (andel-test splice "[(|)]" "[|]")
  (andel-test splice "(aaa | bbb)" "aaa | bbb")
  (andel-test splice "(|[] () () {})" "|[] () () {}"))

(deftest splice-kill-left-test
  (andel-test splice-kill-left "(|)" "|")
  (andel-test splice-kill-left "(()|[])" "|[]")
  (andel-test splice-kill-left "(aaa | bbb)" "| bbb"))

(deftest splice-kill-right-test
  (andel-test splice-kill-right "(|)" "|")
  (andel-test splice-kill-right "([]|{})" "[]|")
  (andel-test splice-kill-right "(aaa | bbb)" "aaa |"))

(deftest navigate-next-form-test
  (andel-test navigate-next-form "|" "|" false)
  (andel-test navigate-next-form "|aaa" "aaa|" false)
  (andel-test navigate-next-form "a|aa" "aaa|" false)
  (andel-test navigate-next-form "(|)" "()|" false)
  (andel-test navigate-next-form "()| ()" "() ()|" false)
  (andel-test navigate-next-form "(()| ())" "(() ()|)" false))

(deftest navigate-next-form-down-test
  (andel-test navigate-next-form-down "|" "|" false)
  (andel-test navigate-next-form-down "(|)" "(|)" false)
  (andel-test navigate-next-form-down "|()" "(|)" false)
  (andel-test navigate-next-form-down "| aaa ()" " aaa (|)" false)
  (andel-test navigate-next-form-down "(| ())" "( (|))" false)
  (andel-test navigate-next-form-down "(| aaa ())" "( aaa (|))" false)
  (andel-test navigate-next-form-down "( aaa (|))" "( aaa (|))" false))

(deftest navigate-next-form-up-test
  (andel-test navigate-next-form-up "|" "|" false)
  (andel-test navigate-next-form-up "(|)" "()|" false)
  (andel-test navigate-next-form-up "((|aaa))" "((aaa)|)" false))

(deftest navigate-prev-form-down-test
  (andel-test navigate-prev-form-down "|" "|" false)
  (andel-test navigate-prev-form-down "(|)" "(|)" false)
  (andel-test navigate-prev-form-down "()|" "(|)" false)
  (andel-test navigate-prev-form-down "() aaa|" "(|) aaa" false)
  (andel-test navigate-prev-form-down "(()|)" "((|))" false)
  (andel-test navigate-prev-form-down "(() aaa|)" "((|) aaa)" false)
  (andel-test navigate-prev-form-down "((|) aaa)" "((|) aaa)" false))

(deftest navigate-prev-form-up-test
  (andel-test navigate-prev-form-up "|" "|" false)
  (andel-test navigate-prev-form-up "(|)" "|()" false)
  (andel-test navigate-prev-form-up "((aaa|))" "(|(aaa))" false))

(deftest backspace-test
  (andel-test backspace "|" "|")
  (andel-test backspace "a|" "|")
  (andel-test backspace "()|" "|")
  (andel-test backspace "(|)" "|")
  (andel-test backspace "(aaa)|" "(aaa|)")
  (andel-test backspace "(aaa|)" "(aa|)")
  (andel-test backspace "|\"\"" "|\"\"")
  (andel-test backspace "\"\"|" "|")
  (andel-test backspace "\"|\"" "|")
  (andel-test backspace "\" \"|" "\" |")
  (andel-test backspace "\"| \"" "| \"")
  (andel-test backspace "\\\"\"|" "\\\"|"))

(deftest delete-test
  (andel-test delete "|" "|")
  (andel-test delete "|a" "|")
  (andel-test delete "|()" "|")
  (andel-test delete "|(aaa)" "(|aaa)")
  (andel-test delete "(|aaa)" "(|aa)")
  ;; delete and backspace have common aux function
  ;; so no need for many string literal tests here
  (andel-test delete "\"\"|" "\"\"|"))

(comment (run-tests))
