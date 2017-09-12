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
            [andel.tree :as tree])
  (:require-macros [reagent.interop :refer [$ $!]]
                   [cljs.core.async.macros :refer [go]]))


(defn font->str [font-name height]
  (str height "px " font-name))


(defn measure [font-name height spacing]
  (let [canvas (js/document.createElement "canvas")
        ctx (.getContext canvas "2d")]
    (set! (.-font ctx) (font->str font-name height))
    (let [width (.-width (.measureText ctx "X"))]
      {:width width
       :height height
       :spacing spacing
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
      (dotimes [pr (map styles/include-script ["codemirror/addon/runmode/runmode-standalone.js"
                                               "codemirror/addon/runmode/runmode-standalone.js"
                                               "codemirror/mode/javascript/javascript.js"
                                               "codemirror/mode/clike/clike.js"
                                               "codemirror/mode/clojure/clojure.js"])]
        (a/<! pr))
      (let [{:keys [height font-name] :as font-metrics} (a/<! (measure-async "Fira Code" 16 3))]
        (styles/defstyle (garden.stylesheet/at-keyframes "blinker"
                                                         ["50%" {:opacity "0"}]))
        (styles/defstyle :line-text [:.line-text {:position :absolute
                                                  :padding "0px"
                                                  :margin "0px"
                                                  :background "inherit"
                                                  :border-radius "0px"
                                                  :border-width "0px"
                                                  :font-family font-name
                                                  :font-size (styles/px height)
                                                  :color theme/foreground
                                                  :left 0
                                                  :user-select :none
                                                  :top 0}])
        (a/>! loaded {:font-metrics font-metrics})))
    loaded))


(defonce *editors-common (load-editors-common!))


(defn deliver-lexems! [{:keys [req-ts tokens index text]} state-ref]
  (let [res (swap! state-ref
                   (fn [{:keys [document] :as state}]
                     (let [{:keys [timestamp]} document]
                       (if (= timestamp req-ts)
                         (-> state
                             (assoc-in [:document :lines index :tokens] tokens)
                             (assoc-in [:document :hashes (hash text)] tokens)
                             (assoc-in [:document :first-invalid] (inc index)))
                         state))))]
       (= (get-in res [:document :timestamp]) req-ts)))


(defn run-lexer-loop! [state-ref]
  (let [{:keys [document] :as state} @state-ref
        {:keys [modespec lexer-broker]} document
        {:keys [input output]} (lexer/new-lexer-worker modespec)]
    (go
      (loop [state nil
             line 0
             start-time 0]
        (let [elapsed (- (.getTime (js/Date.)) start-time)
              next-text (when (< line (text/lines-count (get-in state [:document :text])))
                          (some-> state :document :text (text/line-text line)))
              [val port] (a/alts! (cond-> [lexer-broker output]
                                    (some? next-text) (conj [input {:index line
                                                                    :text next-text
                                                                    :req-ts (get-in state [:document :timestamp])}]))
                                  :priority true)]
          (let [start-time' (if (< 10 elapsed)
                              (do (a/<! (a/timeout 1))
                                  (.getTime (js/Date.)))
                              start-time)]
            (cond
              (= port lexer-broker) (recur val (get-in val [:document :first-invalid]) start-time')
              (= port output) (let [delivered?  (deliver-lexems! val state-ref)]
                                (recur state (if delivered? (inc line) line) start-time'))
              (= port input) (recur state line start-time'))))))))


(defn attach-lexer! [state-ref]
  (run-lexer-loop! state-ref)
  (add-watch state-ref :lexer
             (fn [_ _ old-s new-s]
               (let [old-ts (get-in old-s [:document :timestamp])
                     new-ts (get-in new-s [:document :timestamp])
                     broker (get-in new-s [:document :lexer-broker])]
                 (when (not= old-ts new-ts)
                   (a/put! broker new-s))))))



;; proto-marker-map -> marker-record
(defn- create-marker [proto-marker]
  (letfn [(classes-by-keys [ks styles]
                           (let [classes (->> styles
                                              (map (fn [style]
                                                     (let [style (select-keys style ks)]
                                                       (when (not-empty style)
                                                         (styles/style->class style)))) )
                                              (filter some?))]
                             (when (not-empty classes)
                               (->> classes
                                    (interpose " ")
                                    (apply str)))))]
    (-> proto-marker
        intervals/map->Marker
        (assoc :foreground (classes-by-keys
                             [:color
                              :font-weight
                              :font-style]
                             (:style proto-marker)))
        (assoc :background (classes-by-keys
                             [:background-color
                              :border-bottom-style
                              :border-color
                              :border-width
                              :border-radius]
                             (:style proto-marker))))))

(defn ready-to-view? [editor-state]
  (some? (get-in editor-state [:viewport :metrics])))

;; renderring

(defn el
  ([tag props]
   (.createElement js/React tag props))
  ([tag props children]
   (.createElement js/React tag props children)))


(defn style [m]
  (reduce-kv (fn [s k v]
               (str s (name k) ":" (if (keyword? v) (name v) v) ";")) nil m))

(defn render-attrs [m]
  (reduce-kv (fn [s k v]
               (str s " " (name k) "=\"" (if (keyword? v) (name v) v) "\"")) nil m))

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

(defn infinity? [x] (keyword? x))

(defn render-selection [[from to] {:keys [width] :as metrics}]
  #js [:div
       {:style
        (style {:background-color theme/selection
                :height (styles/px (utils/line-height metrics))
                :position :absolute
                :top (styles/px 0)
                :left (if (infinity? to)
                        0
                        (styles/px (* from width)))
                :margin-left (when (infinity? to) (styles/px (* from width)))
                :width (if (infinity? to)
                         "100%"
                         (styles/px (* (- to from) width)))})}])


(defn render-caret [col {:keys [width] :as metrics}]
  #js [:div {}
       #js [:div {:style (style {:height (styles/px (inc (utils/line-height metrics)))
                                 :width "100%"
                                 :background-color (:bg-05 theme/zenburn)
                                 :position :absolute
                                 :left 0
                                 :top 0
                                 :z-index "-1"})}]

       #js [:div {:style (style {:width (styles/px 1)
                                 :animation "blinker 1s cubic-bezier(0.68, -0.55, 0.27, 1.55) infinite"
                                 :top 0
                                 :background-color "white"
                                 :position :absolute
                                 :left (styles/px (* col width))
                                 :height (styles/px (inc (utils/line-height metrics)))})}]])

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

(defn push! [a x]
  (.push a x)
  a)

(defn measure-time [name f]
  (fn [& args]
    (let [start (str name "-start")
          end   (str name "-end")]
      (js/performance.mark start)
      (let [result (apply f args)]
        (js/performance.mark end)
        (js/performance.measure (str name) start end)
        result))))

(defn render-text' [text]
    (fn [xf]
      (let [pendings (array)
            *last-pos (atom 0)
            next-pending (fn [pendings] (reduce (fn [c p] (if (or (nil? c) (< (.-to p) (.-to c))) p c)) nil pendings))
            js-disj! (fn [arr v]
                                  (let [idx (.indexOf arr v)]
                                    (if (< -1 idx)
                                      (do
                                        (.splice arr idx 1)
                                        arr)
                                      arr)))]
        (fn
          ([] (xf))
          ([res m]
           (loop [res res]
             (let [p (next-pending pendings)
                   last-pos @*last-pos
                   new-class (->> (map (fn [m] (.-foreground m)) pendings)
                                  (interpose " ")
                                  (apply str))]
               (if (or (nil? p) (< (.-from m) (.-to p)))
                 (do
                   (push! pendings m)
                   (reset! *last-pos (.-from m))
                   (if (== last-pos (.-from m))
                     res
                     (xf res #js [:span {:class new-class}
                                  (subs text last-pos (.-from m))])))

                 (do
                   (js-disj! pendings p)
                   (reset! *last-pos (.-to p))
                   (if (== last-pos (.-to p))
                     (recur res)
                     (recur (xf res #js [:span {:class new-class}
                                         (subs text last-pos (.-to p))]))))))))
          ([res]
           (loop [res res]
             (let [p (next-pending pendings)
                   last-pos @*last-pos
                   new-class (->> (map (fn [m] (.-foreground m)) pendings)
                                  (interpose " ")
                                  (apply str))]
               (cond
                 (some? p)
                 (do
                   (js-disj! pendings p)
                   (reset! *last-pos (.-to p))
                   (if (== last-pos (.-to p))
                     (recur res)
                     (recur (xf res #js [:span {:class new-class}
                                         (subs text last-pos (.-to p))]))))

                 (not= last-pos (count text))
                 (xf res #js [:span {} (subs text last-pos (count text))])

                 :else res))))))))

(defn render-background-markup [{:keys [height width spacing]}]
  (comp (filter (fn [m] (some? (.-background m))))
        (map (fn [m]
               (let [from (.-from m)
                     to (.-to m)
                     background (.-background m)]
                 #js [:div {:class background
                            :style (style {:left (styles/px (* from width))
                                           :width (styles/px (* width (- to from)))
                                           :height (styles/px height)
                                           :position :absolute})}])))))

(def real-dom
  (js/createReactClass
   #js {:componentWillUpdate
        (fn [next-props next-state]
          (this-as this
            (let [elt ($ next-props :dom)
                  node (.findDOMNode js/ReactDOM this)
                  child (.-firstChild node)]
              (when child
                (.removeChild node child))
              (.appendChild node elt))))
        :componentDidMount
        (fn []
          (this-as this
            (let [elt ($ ($ this :props) :dom)
                  node (.findDOMNode js/ReactDOM this)]
              (.appendChild node elt))))

        :render
        (fn [_] (el "div" #js {:key "realDOM"}))}))

(defrecord LineInfo [lineText lineTokens lineMarkup selection caretIndex index])

(defn dom [el]
  (let  [tag (aget el 0)
         attrs-map (aget el 1)
         children (.slice el 2)]
    (assert (some? el))
    (let [len (.-length children)
          el-with-children (loop [i 0
                                  n (make-node tag)]
                             (if (< i len)
                               (if-let [c (aget children i)]
                                 (recur (inc i)
                                        (conj-child! n (if (string? c)
                                                         (make-text-node c)
                                                         (dom c))))
                                 (recur (inc i) n))
                               n))]
      (reduce-kv (fn [n a v]
                   (assoc-attr! n a v))
                 el-with-children
                 attrs-map))))

(defn def-fun [f]
  (js/createReactClass
   #js {:shouldComponentUpdate
        (fn [new-props new-state]
          (this-as this
            (let [old-props ($ this :props)]
              (not= (aget old-props "props") (aget new-props "props")))))
        :render (fn [_]
                  (this-as this
                    (f (aget (aget this "props") "props"))))}))

(def render-line
  (def-fun
    (fn [props]
      (this-as this
        (let [line-info (props :line-info)
              metrics (props :metrics)
              line-text (.-lineText line-info)
              line-tokens (.-lineTokens line-info)
              line-markup (.-lineMarkup line-info)
              selection (.-selection line-info)
              caret-index (.-caretIndex line-info)]
          (el real-dom #js {:dom (dom
                                  #js [:div {:class :render-line}
                                       (transduce (render-background-markup metrics) push! #js [:div {}] line-markup)
                                       (render-selection selection metrics)
                                       (transduce (render-text' line-text) push! #js [:pre {:class :line-text}] line-markup)
                                       (when caret-index (render-caret caret-index metrics))])}))))))


(defn line-selection [[from to] [line-start-offset line-end-offset]]
  (cond (and (< from line-start-offset) (< line-start-offset to))
        (if (< line-end-offset to)
          [0 :infinity]
          [0 (- to line-start-offset)])
        (and (<= line-start-offset from) (<= from line-end-offset))
        [(- from line-start-offset) (if (<= to line-end-offset)
                                      (- to line-start-offset)
                                      :infinity)]
        :else nil))

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
            (let [props (aget (aget cmp "props") "props")
                  node (.findDOMNode js/ReactDOM cmp)
                  on-resize (fn []
                              (let [height ($ node :clientHeight)
                                    width ($ node :clientWidth)]
                                 ;; paint flashing on linux when viewport bigger than screen size
                                ((props :onResize) (- width 0) (- height 3))))]
              (on-resize)
              (.addEventListener
               js/window ;; replace with erd
               "resize"
               on-resize))))
        :render (fn [_]
                  (this-as this
                    (let [props (aget (aget this "props") "props")
                          child (props :child)
                          on-resize (props :onResize)
                          on-mouse-wheel (props :onMouseWheel)]
                      (el "div" #js {:key "scroll"
                                     :style #js {:display "flex"
                                                 :flex "1"
                                                 :overflow :hidden}
                                     :onWheel (fn [evt]
                                                (on-mouse-wheel (.-deltaX evt) (.-deltaY evt))
                                                (.preventDefault evt))}
                          [child]))))}))



(defn prepare-markup [markup from to]
  (->> markup
       (filter (fn [marker]
                 (and (<= (.-from marker) to)
                      (<= from (.-to marker)))))
       (map (fn [marker]
              (intervals/->Marker (max 0 (- (.-from marker) from))
                                  (max 0 (- (.-to marker) from))
                                  false
                                  false
                                  (.-background marker)
                                  (.-foreground marker))))))

(defn editor-viewport [props]
  (let [state ($ props :editorState)
        on-mouse-down (aget props "onMouseDown")
        on-drag-selection (aget props "onDragSelection")
        {:keys [editor document viewport]} state
        {:keys [pos view-size metrics]} viewport
        line-height (utils/line-height metrics)
        {:keys [text lines hashes]} document
        {:keys [caret selection]} editor
        [_ from-y-offset] pos
        [w h] view-size
        left-columnt 0
        top-line (int (/ from-y-offset line-height))
        bottom-line (+ top-line (/ h line-height))
        y-shift (- (* line-height (- (/ from-y-offset line-height) top-line)))
        line-zipper (text/scan-to-line (text/zipper text) top-line)
        from-offset (text/offset line-zipper)
        to-offset (dec (text/offset (text/scan-to-line line-zipper (inc bottom-line))))
        markers-zipper (tree/scan (intervals/zipper (:markup document)) (intervals/by-offset from-offset))
        caret-offset (get caret :offset)
        markup (intervals/query-intervals  {:from from-offset :to to-offset})
        _ (styles/defstyle :render-line [:.render-line {:height (styles/px (utils/line-height metrics))
                                                        :position :relative
                                                        :overflow :hidden}])
        ->line-info (fn [start-loc]
                      (let [index (utils/line-number start-loc)
                            start-offset (text/offset start-loc)
                            next-line-loc (utils/scan-to-next-line start-loc)
                            end-offset (text/offset next-line-loc)
                            length (- end-offset start-offset)
                            text (text/text start-loc length)
                            selection (line-selection selection [start-offset end-offset])
                            caret (when (and (<= start-offset caret-offset) (<= caret-offset end-offset))
                                         (- caret-offset start-offset))
                            line-tokens (or (get hashes (hash text)) (:tokens (get lines index)))

                            line-markup (prepare-markup markup start-offset end-offset)]
                        (LineInfo. text line-tokens line-markup selection caret index)))

        children (transduce
                   (comp (take (- bottom-line top-line))
                         (map ->line-info)
                         (map (fn [line-info]
                                (let [index (.-index line-info)]
                                  (el "div" (js-obj "key" index
                                                    "style" (js-obj "transform" (str "translate3d(0px, " (styles/px y-shift) ", 0px)")))
                                      #js [(el render-line #js {:key index
                                                                :props {:line-info line-info
                                                                        :metrics metrics}})])))))
                  push!
                  #js []
                  (iterate utils/scan-to-next-line line-zipper))]
    (el "div" #js {:style #js {:background theme/background
                               :width "100%"
                               :overflow "hidden"}
                   :key "viewport"
                   :onMouseDown (fn [event]
                                  (when on-mouse-down
                                    (let [x (- ($ event :clientX) (-> event (.-currentTarget) (.-offsetLeft)))
                                          y (- ($ event :clientY) (-> event (.-currentTarget) (.-offsetTop)))]
                                      (on-mouse-down x y))))
                   :onMouseMove  (fn [event]
                                   (when on-drag-selection
                                     (when (= ($ event :buttons) 1)
                                       (let [x (- ($ event :clientX) (-> event (.-currentTarget) (.-offsetLeft)))
                                             y (- ($ event :clientY) (-> event (.-currentTarget) (.-offsetTop)))]
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
            (let [props    (aget cmp "props")
                  focused? (aget props "isFocused")
                  on-input (aget props "onInput")
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
   #js {;;:componentDidMount
        #_(fn []
          (this-as cmp
            (let [*state ($ ($ cmp :props) :editorState)
                  *scheduled? (atom false) ]
              (aset cmp "bindings" *bindings)
              #_(add-watch *state :editor-view
                         (fn [_ _ old-state new-state]
                           (when (and (not= old-state new-state) (not @*scheduled?))
                             (reset! *scheduled? true)
                             (next-tick (fn [time]
                                          (reset! *scheduled? false)
                                          ($ cmp forceUpdate))))))
             #_(when (not (ready-to-view? @*state))
                (go
                  (let [metrics (:font-metrics (a/<! *editors-common))]
                    (js/console.log "METRICS: " metrics)
                    (swap! *state assoc-in [:viewport :metrics] metrics)))))))

        ;;:componentWillUnmount
        #_(fn []
          (this-as cmp
            (let [*state ($ ($ cmp :props) :editorState)]
              (remove-watch *state :editor-view))))

        :shouldComponentUpdate
        (fn [new-props new-state]
          (this-as this
                   (let [old-props ($ this :props)]
                     (not= (aget old-props "editorState") (aget new-props "editorState")))))

         :render
        (fn []
          (this-as cmp
            (let [props ($ cmp :props)
                  state ($ props :editorState)
                  {:keys [on-input on-mouse-down on-drag-selection on-resize on-scroll on-focus on-key-down] :as callbacks} ($ props :callbacks)
                  *bindings ($ cmp :bindings)]
              (if (ready-to-view? state)
                (el "div" #js {:key "editor"
                               :style #js {:display "flex"
                                           :flex "1"
                                           :outline "transparent"}
                               :tabIndex -1
                               :onFocus on-focus}
                    #js [(el scroll (js-obj "key" "viewport"
                                            "props" {:child (el editor-viewport
                                                                #js {:key             "editor-viewport"
                                                                     :editorState     state
                                                                     :onMouseDown     on-mouse-down
                                                                     :onDragSelection on-drag-selection})
                                                     :onResize on-resize
                                                     :onMouseWheel on-scroll}))
                         (el hidden-text-area-cmp
                             #js {:key "textarea"
                                  :isFocused (get-in state [:viewport :focused?])
                                  :onInput on-input
                                  :onKeyDown on-key-down})])
                (el "div" #js {:key "editor-placeholder"} #js ["EDITOR PLACEHOLDER"])))))}))

(defn editor-view [editor-state callbacks]
  (el editor-cmp #js {:editorState editor-state
                      :callbacks callbacks
                      :key "editor"}))
