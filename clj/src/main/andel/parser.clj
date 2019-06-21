(ns andel.parser
  (:require [andel.text :as text])
  (:import [jsitter.api Zipper SyntaxHighlighter ApiKt Parser NodeType Terminal Language Text]))



(defn s-expr [^Zipper z]
  (let [node-type (.getNodeType z)]
    (if (instance? Terminal node-type)
      (.getName node-type)
      (lazy-seq (let [child (.down z)]
                  (apply list (symbol (.getName node-type))
                         (map s-expr (take-while some? (iterate #(.right ^Zipper %) child)))))))))



(comment

  (def cobra-go-str (slurp "/Users/jetzajac/cobra.go"))
  (def cobra-go-str (slurp "/Users/jetzajac/Projects/jsitter/testData/router_go"))

  (def text (andel.Text/makeText cobra-go-str))

  (def jsitter-text (andel.JSitterText. text))

  (def source_file (NodeType. "source_file"))

  (def golang (doto
               (jsitter.impl.ImplKt/loadTSLanguage "go")
               (.register source_file)))

  (def parser (.parser golang source_file))

  (time
   (def parse-res (.parse parser jsitter-text nil)))

  (def tree (.getTree parse-res))
  (def root (.zipper tree))
  (require 'clojure.pprint)
  (clojure.pprint/pprint (s-expr root))

  (.getByteSize tree)

  )