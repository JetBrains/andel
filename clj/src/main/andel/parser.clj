(ns andel.parser
  (:require [andel.text :as text])
  (:import [jsitter.api Zipper SyntaxHighlighter ApiKt Parser NodeType Terminal Language Text]
           [andel Intervals Intervals$Zipper]))



(defn s-expr [^Zipper z]
  (let [node-type ^NodeType (.getNodeType z)]
    (if (instance? Terminal node-type)
      (.getName node-type)
      (lazy-seq (let [child (.down z)]
                  (apply list (symbol (.getName node-type))
                         (map s-expr (take-while some? (iterate #(.right ^Zipper %) child)))))))))



(comment

  (def cobra-go-str (slurp "/Users/jetzajac/cobra.go"))
  (def cobra-go-str (slurp "/Users/jetzajac/Projects/jsitter/testData/router_go"))

  (def text (andel.Text/makeText cobra-go-str (andel.Text$TextOps. 32, 64)))

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

  (defn next-to-highlight [^jsitter.api.Zipper syntax-z ^long range-start-byte ^long range-end-byte]
    (loop [^jsitter.api.Zipper syntax-z (jsitter.api.ApiKt/next syntax-z)]
      (when syntax-z
        (let [from-byte (.getByteOffset syntax-z)
              to-byte (+ (.getByteOffset syntax-z) (.getByteSize syntax-z))
              terminal? (instance? Terminal (.getNodeType syntax-z))]
          (cond
            (<= to-byte range-start-byte)
            (recur (jsitter.api.ApiKt/skip syntax-z))

            (<= range-start-byte from-byte to-byte range-end-byte)
            syntax-z

            (<= range-end-byte from-byte)
            nil

            terminal? syntax-z
            :else (recur (jsitter.api.ApiKt/next syntax-z)))))))



  (defonce next-id (atom 0))

  (defn highlight-syntax [^andel.Intervals intervals-tree ^jsitter.api.Tree syntax-tree text range-start-byte range-end-byte highlighter]
    (let [ctx (andel.Intervals$EditingContext. (.-nextInnerId intervals-tree)
                                               (.-maxChildren intervals-tree)
                                               (.linear (.-parentsMap intervals-tree)))
          ^jsitter.api.Zipper syntax-z (next-to-highlight (.zipper syntax-tree) range-start-byte range-end-byte)
          text-z (andel.text/scan-to-char-offset (andel.text/transient (andel.text/zipper text)) (quot (.getByteOffset syntax-z) 2))
          from-offset (andel.text/offset text-z)
          to-offset (andel.text/offset (andel.text/scan-to-char-offset (andel.text/zipper text) (quot range-end-byte 2)))
          intervals-z (andel.Intervals/skipToOffset (andel.Intervals$Zipper/create (.openRoot intervals-tree) ctx true) (* 2 from-offset))
          has-next? (andel.Intervals$Zipper/hasNext intervals-z)]
      (loop [text-z text-z
             ^jsitter.api.Zipper syntax-z syntax-z
             ^andel.Intervals$Zipper intervals-z (if has-next? (andel.Intervals$Zipper/next intervals-z) intervals-z)
             intervals-done? (not has-next?)]
        (let [action (if (nil? syntax-z)
                       (if intervals-done? :return :remove)
                       (cond
                         intervals-done? :insert
                         (and (= (andel.text/offset text-z) (Intervals$Zipper/from intervals-z))
                              (= (highlighter syntax-z) (Intervals$Zipper/data intervals-z))
                              (= (-> (andel.text/persistent! text-z)
                                     (andel.text/scan-to-char-offset (quot (+ (.getByteOffset syntax-z) (.getByteSize syntax-z)) 2))
                                     (andel.text/offset))
                                 (Intervals$Zipper/to intervals-z))) :skip
                         (< (text/offset text-z) (Intervals$Zipper/from intervals-z)) :insert
                         :else :remove))]
          (case action
            :return (andel.Intervals.
                      (.maxChildren ctx)
                      (andel.Intervals$Zipper/root intervals-z)
                      (.closedRoot intervals-tree)
                      (.forked (.parentsMap ctx))
                      (.nextId ctx))
            :insert (let [^jsitter.api.Zipper syntax-z' (next-to-highlight syntax-z range-start-byte range-end-byte)
                          intervals-z' (if-let [attrs (highlighter syntax-z)]
                                         (let [syntax-to (-> (andel.text/persistent! text-z)
                                                             (andel.text/scan-to-char-offset (quot (+ (.getByteOffset syntax-z) (.getByteSize syntax-z)) 2))
                                                             (andel.text/offset))]
                                           (Intervals$Zipper/insert intervals-z (swap! next-id inc) (text/offset text-z) syntax-to false false attrs))
                                         intervals-z)]
                      (recur
                        (when syntax-z' (andel.text/scan-to-char-offset text-z (quot (.getByteOffset syntax-z') 2)))
                        syntax-z'
                        intervals-z'
                        intervals-done?))
            :skip (let [^jsitter.api.Zipper syntax-z' (next-to-highlight syntax-z range-start-byte range-end-byte)
                        text-z' (when syntax-z' (andel.text/scan-to-char-offset text-z (quot (.getByteOffset syntax-z') 2)))]
                    (if (Intervals$Zipper/hasNext intervals-z)
                      (let [^andel.Intervals$Zipper intervals-z' (Intervals$Zipper/next intervals-z)]
                        (recur
                          text-z'
                          syntax-z'
                          intervals-z'
                          (<= to-offset (Intervals$Zipper/from intervals-z'))))
                      (recur
                        text-z'
                        syntax-z'
                        intervals-z
                        true)))
            :remove (let [intervals-z' (Intervals$Zipper/remove intervals-z)]
                      (if (Intervals$Zipper/hasNext intervals-z')
                        (let [intervals-z'' (Intervals$Zipper/next intervals-z')]
                          (recur text-z syntax-z intervals-z'' (<= to-offset (Intervals$Zipper/from intervals-z''))))
                        (recur text-z syntax-z intervals-z' true))))))))


  (time
   (def intervals-tree (highlight-syntax (andel.Intervals/empty 32) tree text 0 (.getByteSize tree) (fn [^jsitter.api.Zipper z] (.getNodeType z)))))

  (time
   (def intervals-tree (andel.SyntaxHighlighting/highlightSyntax (andel.Intervals/empty 32)
                                                                 tree
                                                                 text
                                                                 0 (.getByteSize tree)
                                                                 (reify andel.SyntaxHighlighting$SyntaxHighlighter
                                                                        (highlight [_ zip] (.getNodeType zip))))))

  (defn np [node]
    (if (instance? andel.Intervals$Node node)
      (let [^andel.Intervals$Node node node]
        {:starts (.-starts node)
         :ends (.-ends node)
         ;:ids (.-ids node)
         :children (mapv np (.-children node))})
      (str node)))

  (defn tp [^Intervals tree]
    {:open-root (np (.-openRoot tree))
     :closed-root (np (.-closedRoot tree))
     :mapping (.-parentsMap tree)})


  (clojure.pprint/pprint (tp intervals-tree))


  (def text (andel.Text/makeText "func hello() { sayHello() }"))
  (def parser (.parser golang source_file))

  (def jsitter-text (andel.JSitterText. text))
  (time
   (def parse-res (.parse parser jsitter-text nil)))

  (def tree (.getTree parse-res))

  (def intervals-tree (highlight-syntax (andel.Intervals/empty 32) tree text 0 (.getByteSize tree) (fn [^jsitter.api.Zipper z] (.getNodeType z))))

  (clojure.pprint/pprint (tp intervals-tree))

  (def text' (-> (andel.Text/zipper text)
                 (andel.Text/scanToCharOffset 23)
                 (andel.Text/insert "Bye")
                 (andel.Text/delete 2)
                 (andel.Text/root)))

  [[:retain 23] [:insert "Bye"] [:delete 2] [:retain (- (count "func hello() { sayHello() }") 2 23)]]

  (def edits [(jsitter.api.Edit. (* 2 23) (* 2 23) (* 2 (+ 23 3)))
              (jsitter.api.Edit. (* 2 (+ 23 3)) (* 2 (+ 5 23)) (* 2 (+ 23 3)))])


  (def parse-res' (.parse parser
                          (andel.JSitterText. text')
                          (jsitter.api.Increment. tree edits)))

  (def changed-ranges (.getChangedRanges parse-res'))

  (def tree' (.getTree parse-res'))



  (def intervals-tree' (andel.SyntaxHighlighting/highlightSyntax (-> intervals-tree
                                             (Intervals/expand 23 3)
                                             (Intervals/collapse 26 2))
                                            tree'
                                            text'
                                           (.getFirst (first changed-ranges)) (.getSecond (first changed-ranges))
                                           (reify andel.SyntaxHighlighting$SyntaxHighlighter
                                                  (highlight [_ zip] (.getNodeType zip)))))

  (time
  (def fresh-intervals-tree' (highlight-syntax (andel.Intervals/empty 32) tree' text' 0 (.getByteSize tree') (fn [^jsitter.api.Zipper z] (.getNodeType z)))))

  (= (tp intervals-tree')
     (tp fresh-intervals-tree'))

  (clojure.pprint/pprint (s-expr (.zipper tree)))
  (clojure.pprint/pprint (s-expr (.zipper tree')))
  (require 'clojure.pprint)
  (require 'clojure.data)
  *e
  (clojure.pprint/pprint (tp intervals-tree'))
  (clojure.pprint/pprint (tp fresh-intervals-tree'))
  (clojure.pprint/pprint (tp intervals-tree))



  (do(prn "===========================")
  (clojure.pprint/pprint
   (clojure.data/diff (tp intervals-tree')
                     (tp fresh-intervals-tree'))))


  (Intervals$Zipper/from
    (Intervals$Zipper/next
     (Intervals/skipToOffset (Intervals$Zipper/create (.-openRoot intervals-tree) nil true)
                                 30)))

  *e
  )