(ns slurper.lexer)

(defn- modespec->mode [modespec]
  (.getMode js/CodeMirror (clj->js {:indentUnit 2}) modespec))

(defn- copy-state [mode state]
  (letfn
    [(copy-value [v]
                 (if (array? v)
                   (.concat v (clj->js []))
                   v))
     (default-copy-state [state]
                         (let [js-obj (clj->js {})]
                           (doseq [k (js-keys state)]
                             (aset js-obj k (copy-value (aget state k))))
                           js-obj))]
    (if (or (true? state) (nil? state))
      state
      ((or (.-copyState mode)
            default-copy-state)
        state))))

;; [String LexerState] -> [[Token] LexerState]
(defn lex [modespec text state]
  (let [mode (modespec->mode modespec)
         state (copy-state mode state)
         *result (atom
                  {:tokens []
                   :state  state})]
    (.runMode js/CodeMirror text modespec
              (fn [text style line-number offset state]
                (swap! *result
                       (fn [result]
                         (-> result
                             (update :tokens conj [(count text) style])
                             (assoc :state state)))))
              (clj->js {:state state}))
    @*result
    #_(update @*result :state (partial copy-state mode))))



(comment

  (let [{:keys [state tokens]} (lex "text/x-java" "class Foo { static void foo() { return \"hell\\" nil)]
    (prn "FIRST" tokens)
    ;    (prn "STATE " (.-lastType state))
    (js/console.log "BEFORE " (.stringify js/JSON state))
    (let [{:keys [tokens]} (lex "text/x-java" "o world\";} }" state)]
      (prn "SECOND " tokens))
    (js/console.log "AFTER  " (.stringify js/JSON state))
    (let [{:keys [tokens]} (lex "text/x-java" "o world\";} }" state)]
      (prn "SECOND11 " tokens))
    )

  (let [{:keys [state tokens]} (lex "javascript" "function foo() { return \"hell\\" nil)]
    (prn "FIRST" tokens)
    ;    (prn "STATE " (.-lastType state))
    (js/console.log "BEFORE " (.stringify js/JSON state))
    (let [{:keys [tokens]} (lex "javascript" "o world\";}" state)]
      (prn "SECOND " tokens))
    (js/console.log "AFTER  " (.stringify js/JSON state))
    (let [{:keys [tokens]} (lex "javascript" "o world\";}" state)]
      (prn "SECOND11 " tokens))
    )

  )

(comment
  (use 'figwheel-sidecar.repl-api)

  (prn (:tokens (lex "javascript" "function foo() { return 42; }" nil)))
  ((.-startState (modespec->mode "javascript")))
  (modespec->mode "javascript"))