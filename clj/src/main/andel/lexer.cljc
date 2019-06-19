(ns andel.lexer)

(defprotocol Lexer
  (lexemes [this from to constructor])
  (lexemes-hash [this from to])
  (update-text [this text-tree offset])
  (token-type [this offset])
  (token-type-at-char [this char-offset]))

(defmulti create-lexer (constantly :idea))
(defmethod create-lexer :default [& args] nil)