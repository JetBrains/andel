(ns andel.editor
  (:require [clojure.core.async :as a]
            [clojure.set]

            [cljsjs.create-react-class]
            [cljsjs.react.dom]
            [garden.core :as g]
            [garden.stylesheet :refer [at-keyframes]]

            [andel.keybind :as keybind]
            [andel.styles :as styles]
            [andel.theme :as theme]
            [andel.lexer :as lexer]
            [andel.text :as text]
            [andel.intervals :as intervals]
            [andel.utils :as utils]
            [andel.tree :as tree]
            [andel.render :as render]
            [cljsjs.element-resize-detector])
  (:require-macros [reagent.interop :refer [$ $!]]
                   [cljs.core.async.macros :refer [go]]))


(defn font->str [font-name height]
  (str height "px " font-name))


(defn measure [font-name height spacing]
  (let [canvas (js/document.createElement "canvas")
        ctx    (.getContext canvas "2d")]
    (set! (.-font ctx) (font->str font-name height))
    (let [width (.-width (.measureText ctx "X"))]
      {:width     width
       :height    height
       :spacing   spacing
       :font-name font-name})))


(defn measure-async [font-name size spacing]
  (go
   (measure font-name size spacing)
   (loop []
     (if (.. js/document
             -fonts
             (check (font->str font-name size)))
       (measure font-name size spacing)
       (do
         (a/<! (a/timeout 100))
         (recur))))))


(defn load-editors-common! []
  (let [loaded (a/promise-chan)]
    (go
     (dotimes [pr (map styles/include-script
                       ["codemirror/addon/runmode/runmode-standalone.js"
                        "codemirror/addon/runmode/runmode-standalone.js"
                        "codemirror/mode/javascript/javascript.js"
                        "codemirror/mode/clike/clike.js"
                        "codemirror/mode/clojure/clojure.js"
                        "bloomfilter.js/bloomfilter.js"])]
       (a/<! pr))
     (let [{:keys [height font-name] :as font-metrics} (a/<! (measure-async "Fira Code" 16 3))]
       (styles/defstyle
         (garden.stylesheet/at-keyframes "blinker"
                                         ["50%" {:opacity "0"}]))
       (styles/defstyle :line-text
                        [:.line-text
                         {:position      :absolute
                          :padding       "0px"
                          :margin        "0px"
                          :background    "inherit"
                          :border-radius "0px"
                          :border-width  "0px"
                          :font-family   font-name
                          :font-size     (styles/px height)
                          :color         theme/foreground
                          :left          0
                          :line-height   "normal"
                          :user-select   :none
                          :top           0}])
       (a/>! loaded {:font-metrics font-metrics})))
    loaded))


(defonce *editors-common (load-editors-common!))

(def token-class
  (let [tokens-cache #js {}]
    (fn [tt]
      (when tt
        (if-let [c (aget tokens-cache (name tt))]
          c
          (let [class (name tt)]
            (styles/defstyle tt [(str "." class) (theme/token-styles tt)])
            (aset tokens-cache (name tt) class)
            class))))))

(defn tokens->markers [tokens]
  (let [tokens-count (count tokens)]
    (loop [i      0
           offset 0
           result (make-array tokens-count)]
      (if (< i tokens-count)
        (let [[len tt] (aget tokens i)
              m        (intervals/->Marker offset (+ offset len) false false (intervals/->Attrs nil nil (token-class tt) 0))]
          (aset result i m)
          (recur (inc i)
            (+ offset len)
            result))
        result))))

(defn merge-tokens [lexer-markers]
  (fn [rf]
    (let [lexer-markers-count (count lexer-markers)
          *i                  (atom 0)]
      (fn
        ([] (rf))
        ([acc m]
         (loop [i   @*i
                acc acc]
           (if (and (< i lexer-markers-count) (< (.-from (aget lexer-markers i)) (.-from m)))
             (recur (inc i) (rf acc (aget lexer-markers i)))
             (do
               (reset! *i i)
               (rf acc m)))))
        ([acc]
         (loop [i   @*i
                acc acc]
           (if (< i lexer-markers-count)
             (recur (inc i)
                    (rf acc (aget lexer-markers i)))
             (rf acc))))))))

(defn deliver-lexems! [{:keys [req-ts tokens index text]} state-ref]
  (let [res (swap! state-ref
                   (fn [state]
                     (let [timestamp (-> state :document :timestamp)]
                       (if (= timestamp req-ts)
                         (-> state
                             (assoc-in [:document :lines index :tokens] tokens)
                             ;; (assoc-in [:document :hashes (hash text)] markers)
                             (assoc-in [:document :first-invalid] (inc index)))
                         state))))]
    (= (get-in res [:document :timestamp]) req-ts)))


(defn run-lexer-loop! [state-ref]
  (let [{:keys [document] :as state}    @state-ref
        {:keys [modespec lexer-broker]} document
        {:keys [input output]}          (lexer/new-lexer-worker modespec)]
    (go
     (loop [state      nil
            line       0
            start-time 0]
       (let [elapsed    (- (.getTime (js/Date.)) start-time)
             next-text  (when (< line (text/lines-count (get-in state [:document :text])))
                          (some-> state :document :text (text/line-text line)))
             [val port] (a/alts!
                          (cond-> [lexer-broker output]
                            (some? next-text) (conj
                                                [input
                                                 {:index  line
                                                  :text   next-text
                                                  :req-ts (get-in state [:document :timestamp])}]))
                          :priority true)]
         (let [start-time' (if (< 3 elapsed)
                             (do (a/<! (a/timeout 1))
                               (.getTime (js/Date.)))
                             start-time)]
           (cond
             (= port lexer-broker) (recur val (get-in val [:document :first-invalid]) start-time')
             (= port output)       (let [delivered? (deliver-lexems! val state-ref)]
                                     (recur state (if delivered? (inc line) line) start-time'))
             (= port input)        (recur state line start-time'))))))))


(defn attach-lexer! [state-ref]
  (run-lexer-loop! state-ref)
  (add-watch state-ref :lexer
             (fn [_ _ old-s new-s]
               (let [old-ts (get-in old-s [:document :timestamp])
                     new-ts (get-in new-s [:document :timestamp])
                     broker (get-in new-s [:document :lexer-broker])]
                 (when (not= old-ts new-ts)
                   (a/put! broker new-s))))))

(defn ready-to-view? [editor-state]
  (some? (get-in editor-state [:viewport :metrics])))

;; renderring

(defn el
  ([tag props]
   (.createElement js/React tag props))
  ([tag props children]
   (.createElement js/React tag props children)))


(defn div [class style]
  (doto (js/document.createElement "div")
        (.setAttribute "class" class)
        (.setAttribute "style" style)))

(defn span [class text]
  (doto (js/document.createElement "span")
        (.setAttribute "class" class)
        (.appendChild (js/document.createTextNode text))))



(defn make-node [tag]
  (js/document.createElement (name tag)))

(defn make-text-node [s]
  (js/document.createTextNode s))

(defn assoc-attr! [e a v]
  (.setAttribute e (name a) (if (keyword? v) (name v) v))
  e)

(defn conj-child! [e c]
  (.appendChild e c)
  e)

(defn render-selection [[from to] metrics]
  (div nil
       (render/style
        (render/selection-style from to metrics))))

(defn render-caret [col {:keys [width] :as metrics}]
  (doto (div nil nil)
        (.appendChild
          (div nil (render/style (render/active-line-style metrics))))
        (.appendChild
          (div nil (render/style (render/caret-style col metrics))))))

(defn push! [a x]
  (.push a x)
  a)

(defn append-child! [e c]
  (.appendChild e c)
  e)

(defn measure-time [name f]
  (fn [& args]
    (let [start (str name "-start")
          end   (str name "-end")]
      (js/performance.mark start)
      (let [result (apply f args)]
        (js/performance.mark end)
        (js/performance.measure (str name) start end)
        result))))

(def real-dom
  (js/createReactClass
   #js {:componentWillUpdate
        (fn [next-props next-state]
          (this-as this
            (let [elt   ($ next-props :dom)
                  node  (.findDOMNode js/ReactDOM this)
                  child (.-firstChild node)]
              (when (some? child)
                (.removeChild node child))
              (.appendChild node elt))))
        :componentDidMount
        (fn []
          (this-as this
            (let [elt  ($ ($ this :props) :dom)
                  node (.findDOMNode js/ReactDOM this)]
              (.appendChild node elt))))

        :render
        (fn [_] (el "div" #js {:key "realDOM"}))}))




(defn line-info-eq? [x y]
  (let [array-eq? (fn [a b]
                    (and (= (count a) (count b))
                         (loop [i 0]
                           (if (< i (count a))
                             (if (= (aget a i) (aget b i))
                               (recur (inc i))
                               false)
                             true))))]
    (and (= (.-caret x) (.-caret y))
         (= (.-selection x) (.-selection y))
         (= (.-text x) (.-text y))
         (array-eq? (.-foreground x) (.-foreground y))
         (array-eq? (.-background x) (.-background y)))))


(def render-raw-line
  (js/createReactClass
    #js {:shouldComponentUpdate (fn [new-props new-state]
                                  (this-as this
                                    (let [old-props (aget this "props")
                                          old-line-info (aget old-props "lineInfo")
                                          new-line-info (aget new-props "lineInfo")]
                                      (or (not (line-info-eq? old-line-info new-line-info))
                                          (not (= (aget old-props "metrics") (aget new-props "metrics")))))))
          :render (fn [_]
                    (this-as this
                            (let [props (aget this "props")
                                  metrics (aget props "metrics")
                                  line-info (aget props "lineInfo")
                                  text      (.-text line-info)
                                  selection (.-selection line-info)
                                  bg-markup (.-background line-info)
                                  fg-markup (.-foreground line-info)
                                  caret (.-caret line-info)
                                  {:keys [width height]} metrics
                                  bg-dom (loop [i 0
                                                col 0
                                                dom (js/document.createElement "div")]
                                           (if (< i (count bg-markup))
                                             (let [len (aget bg-markup i)
                                                   class (aget bg-markup (inc i))]
                                               (if (some? class)
                                                 (recur (+ i 2)
                                                        (+ col len)
                                                        (append-child! dom (div class
                                                                                (str "left: " (* col width) "px;"
                                                                                     "width:" (* width len) "px;"
                                                                                     "height:100%; position : absolute;"))))
                                                 (recur (+ i 2)
                                                        (+ col len)
                                                        dom)))
                                             dom))
                                  fg-dom (loop [i 0
                                                col 0
                                                dom (doto (js/document.createElement "pre")
                                                      (.setAttribute "class" "line-text"))]
                                           (if (< i (count fg-markup))
                                             (let [len (aget fg-markup i)
                                                   class (aget fg-markup (inc i))]
                                               (recur (+ i 2)
                                                      (+ col len)
                                                      (append-child! dom (span class (subs text col (+ col len))))))
                                             (if (< col (count text))
                                               (append-child! dom (span nil (subs text col (count text))))
                                               dom)))]
                              (el real-dom #js {:dom (-> (div "render-line" nil)
                                                         (cond-> (some? selection)
                                                                 (append-child! (render-selection selection metrics)))
                                                         (append-child! bg-dom)
                                                         (append-child! fg-dom)
                                                         (cond-> (some? caret)
                                                           (append-child! (render-caret caret metrics))))}))))}))



(def render-line
  (js/createReactClass
   #js {:shouldComponentUpdate
        (fn [new-props new-state]
          (this-as this
            (let [old-props (aget ($ this :props) "props")
                  new-props (aget new-props "props")]
              (not (render/line-info-equiv? old-props new-props)))))
        :render (fn [_]
                  (this-as this
                    (let [props (aget (aget this "props") "props")
                          metrics (aget (aget this "props") "metrics")
                          line-info (build-line-info props)]

                      (el render-raw-line #js{:lineInfo line-info
                                              :metrics metrics}))))}))

(defonce erd (js/elementResizeDetectorMaker #js{:strategy "scroll"}))

(defn setup-erd [dom-node listener]
  (.listenTo erd dom-node listener))

(defn remove-erd [dom-node listener]
  (.removeListener erd dom-node listener))

(def scroll
  (js/createReactClass
   #js {:shouldComponentUpdate
        (fn [new-props new-state]
          (this-as this
            (let [old-props ($ this :props)]
              (not= (aget old-props "props") (aget new-props "props")))))
        :componentDidMount
        (fn []
          (this-as cmp
            (let [props     (aget (aget cmp "props") "props")
                  node      (.findDOMNode js/ReactDOM cmp)
                  on-resize (fn [node]
                              (let [height ($ node :offsetHeight)
                                    width  ($ node :offsetWidth)]
                                ;; paint flashing on linux when viewport bigger than screen size
                                ((props :onResize) (- width 0) (- height 3))))]
              (aset cmp "onResize" on-resize)
              (on-resize node)
              (setup-erd node on-resize))))
        :componentWillUnmount
        (fn []
          (this-as cmp
            (let [node (.findDOMNode js/ReactDOM cmp)]
              (remove-erd node (aget cmp "onResize")))))
        :render (fn [_]
                  (this-as this
                    (let [props          (aget (aget this "props") "props")
                          child          (props :child)
                          on-resize      (props :onResize)
                          on-mouse-wheel (props :onMouseWheel)]
                      (el "div"
                          #js {:key     "scroll"
                               :style   #js {:display  "flex"
                                             :flex     "1"
                                             :overflow :hidden}
                               :onWheel (fn [evt]
                                          (on-mouse-wheel (.-deltaX evt) (.-deltaY evt))
                                          (.preventDefault evt))}
                          [child]))))}))

(defn editor-viewport [props]
  (let [state ($ props :editorState)
        on-mouse-down (aget props "onMouseDown")
        on-drag-selection (aget props "onDragSelection")
        viewport (:viewport state)
        _ (styles/defstyle :render-line
            [:.render-line
             {:height   (str (utils/line-height (:metrics viewport)) "px")
              :position :relative
              :overflow :hidden}])
        {:keys [y-shift] :as viewport-info} (render/viewport-info viewport)
        children (transduce
                  (map (fn  [props]
                         (el "div"
                             #js {:key (:line-number props)
                                  :style #js {:transform (str "translate3d(0px, " y-shift "px, 0px)")}}
                             #js [(el render-line
                                      #js{:props props
                                          :metrics metrics})])))
                  (completing push!)
                  (array)
                  (render/viewport-lines state viewport-info))]
    (el "div"
        #js {:style       #js {:background theme/background
                               :width      "100%"
                               :overflow   "hidden"}
             :key         "viewport"
             :onMouseDown (fn [event]
                            (when on-mouse-down

                              (let [offsetHost (or (.-offsetParent (.-currentTarget event))
                                                   (.-currentTarget event))
                                    x          (- ($ event :clientX) (.-offsetLeft offsetHost))
                                    y          (- ($ event :clientY) (.-offsetTop offsetHost))]
                                (on-mouse-down x y))))
             :onMouseMove (fn [event]
                            (when on-drag-selection
                              (when (= ($ event :buttons) 1)
                                (let [offsetHost (or (.-offsetParent (.-currentTarget event))
                                                     (.-currentTarget event))
                                      x          (- ($ event :clientX) (.-offsetLeft offsetHost))
                                      y          (- ($ event :clientY) (.-offsetTop offsetHost))]
                                  (on-drag-selection x y)))))}
        children)))

(def next-tick
  (let [w js/window]
    (or ($ w :requestAnimationFrame)
        ($ w :webkitRequestAnimationFrame)
        ($ w :mozRequestAnimationFrame)
        ($ w :msRequestAnimationFrame))))

(def hidden-text-area-cmp
  (js/createReactClass
   #js {:componentDidMount
        (fn []
          (this-as cmp
            (let [props    (aget cmp "props")
                  focused? (aget props "isFocused")
                  dom-node (aget (aget cmp "refs") "textarea")]
              (when focused?
                (.focus dom-node)))))
        :componentDidUpdate
        (fn []
          (this-as cmp
            (let [props    (aget cmp "props")
                  focused? (aget props "isFocused")
                  dom-node (aget (aget cmp "refs") "textarea")]
              (when focused?
                (.focus dom-node)))))
        :render
        (fn []
          (this-as cmp
            (let [props       (aget cmp "props")
                  focused?    (aget props "isFocused")
                  on-input    (aget props "onInput")
                  on-key-down (aget props "onKeyDown")]
              (el "textarea"
                  #js {:key       "textarea"
                       :ref       "textarea"
                       :style     #js {:opacity 0
                                       :padding "0px"
                                       :border  "none"
                                       :height  "0px"
                                       :width   "0px"}
                       :onInput   (fn [evt]
                                    (let [e   (.-target evt)
                                          val (.-value e)]
                                      (set! (.-value e) "")
                                      (on-input val)))
                       :onKeyDown on-key-down}))))}))

(def editor-cmp
  (js/createReactClass
   #js {:shouldComponentUpdate
        (fn [new-props new-state]
          (this-as this
            (let [old-props ($ this :props)]
              (not= (aget old-props "editorState") (aget new-props "editorState")))))

        :render
        (fn []
          (this-as cmp
            (let [props             ($ cmp :props)
                  state             ($ props :editorState)
                  {:keys [on-input on-mouse-down on-drag-selection on-resize on-scroll on-focus on-key-down]
                   :as   callbacks} ($ props :callbacks)
                  *bindings         ($ cmp :bindings)]
              (if (ready-to-view? state)
                (el "div"
                    #js {:key      "editor"
                         :style    #js {:display "flex"
                                        :flex    "1"
                                        :cursor  "text"
                                        :outline "transparent"}
                         :tabIndex -1
                         :onFocus  on-focus}
                    #js [(el scroll
                             (js-obj "key" "viewport"
                                     "props"
                                     {:child        (el editor-viewport
                                                        #js {:key             "editor-viewport"
                                                             :editorState     state
                                                             :onMouseDown     on-mouse-down
                                                             :onDragSelection on-drag-selection})
                                      :onResize     on-resize
                                      :onMouseWheel on-scroll}))
                         (el hidden-text-area-cmp
                             #js {:key       "textarea"
                                  :isFocused (get-in state [:viewport :focused?])
                                  :onInput   on-input
                                  :onKeyDown on-key-down})])
                (el "div" #js {:key "editor-placeholder"} #js ["EDITOR PLACEHOLDER"])))))}))

(defn editor-view [editor-state callbacks]
  (el editor-cmp
      #js {:editorState editor-state
           :callbacks   callbacks
           :key         "editor"}))
