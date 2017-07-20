(ns andel.lexer
  (:require [cljs.core.async :as core.async])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]
                   [reagent.interop :refer [$]]))

(defn- modespec->mode [modespec]
  ($  js/CodeMirror getMode #js{:indentUnit 2} modespec))

(defn- copy-state [mode state]
  (letfn
    [(copy-value [v]
                 (if (array? v)
                   (.concat v #js[])
                   v))
     (default-copy-state [state]
                         (let [js-obj #js{}]
                           (doseq [k (js-keys state)]
                             (aset js-obj k (copy-value (aget state k))))
                           js-obj))]
    (if (or (true? state) (nil? state))
      state
      ((or (.-copyState mode)
            default-copy-state)
        state))))

(defn style->keyword [style]
  (some-> style keyword))

;; [String LexerState] -> [[Token] LexerState]
(defn lex [modespec text initial-state]
  (let [mode (modespec->mode modespec)
        tokens #js []
        *state (atom initial-state)]
    ($ js/CodeMirror runMode text modespec
       (fn [text style line-number offset state]
         (.push tokens [(.-length text) (style->keyword style)])
         (reset! *state state))
       #js {:state (copy-state mode initial-state)})

    {:tokens (vec tokens)
     :state @*state}))

(defn submit-request! [{:keys [input] :as worker} {:keys [index text] :as req}]
  (core.async/put! input req))

(defn new-lexer-worker [modespec]
  (let [input (core.async/chan)
        output (core.async/chan)
        states #js [nil]]
    (go-loop []
      (when-let [{:keys [index text] :as req} (core.async/<! input)]
        (let [base-state (aget states index)
              {:keys [tokens state]} (lex modespec text base-state)]
          (.splice states (inc index) (- (.-length states) index))
          (.push states state)
          (core.async/>! output (assoc req :tokens tokens)))
        (recur)))
    {:input input
     :output output
     :modespec modespec}))

(comment

  (def worker (new-lexer-worker "text/x-java"))
  (go
   (submit-request! worker :index 0
                           :text "class Foo { static void foo() { return \"hell\\"))

  (go
   (submit-request! worker :index 1
                    :text "o world\";} }"))

  (go
   (prn (core.async/<! (:output worker))))

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
  (nth [1] 0)
  (conj (take 2 [1 2 3]) 2)
  (prn (:tokens (lex "javascript" "function foo() { return 42; }" nil)))
  ((.-startState (modespec->mode "javascript")))
  (modespec->mode "javascript"))
