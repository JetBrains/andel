(ns andel.parser
  (:require [andel.text :as text])
  (:import [jsitter.api Zipper SyntaxHighlighter ApiKt Parser NodeType Terminal Language Text]))



(defn s-expr [^Zipper z]
  (let [node-type (.getNodeType z)]
    (if (instance? Terminal node-type)
      (symbol (.getName node-type))
      (lazy-seq (let [child (.down z)]
                  (apply list (symbol (.getName node-type))
                         (map s-expr (take-while some? (iterate #(.right ^Zipper %) child)))))))))



(comment

  (def cobra-go-str (slurp "/Users/jetzajac/cobra.go"))
  (def cobra-go-str (slurp "/Users/jetzajac/Projects/jsitter/testData/router_go"))

  (def t (andel.Text/makeText cobra-go-str (andel.Text$TextOps. 32 256)))


  (def chunk-bytes-size (* 1024 1024))
  (def utf16-le kotlin.text.Charsets/UTF_16LE)

  (def jsitter-text
    (reify jsitter.api.Text
         (read [_ start-byte buffer]
               (let [start-char (quot start-byte 2)
                     loc (text/scan-to-char-offset (text/zipper t) start-char)
                     text-chars-count (text/chars-count t)]
                 (andel.Text/reduceText loc (min (quot chunk-bytes-size 2) (- text-chars-count start-char))
                                        nil
                                        (reify andel.Text$TextReducer
                                               (rf [_ _ str start end]
                                                   (let [bytes (.getBytes ^String str utf16-le)]
                                                     (.put buffer bytes (* 2 start) (* 2 (- end start)))))))))
         (getEncoding [_] jsitter.api.Encoding/UTF16)))

  (def source_file (NodeType. "source_file"))

  (def golang (doto
               (jsitter.impl.ImplKt/loadTSLanguage "go")
               (.register source_file)))

  (def parser (.parser golang source_file))

  (require )

  (time
   (def parse-res (.parse parser jsitter-text [])))

  (def tree (.getTree parse-res))
  (def root (.zipper tree))
  (require 'clojure.pprint)
  (clojure.pprint/pprint (s-expr root))

  (.getByteSize tree)



  )