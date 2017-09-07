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
            [andel.controller :as controller]
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

(defn make-editor-state []
  (let [*editor-state (atom {:document {:text (text/make-text "")
                                        :markup (intervals/make-interval-tree)
                                        :lexer-broker (a/chan)
                                        :modespec "text/x-java"
                                        :timestamp 0
                                        :lines []
                                        :first-invalid 0}
                             :editor {:caret {:offset 0 :v-col 0}
                                      :selection [0 0]
                                      ;; :carets [{:caret 0 :virtual-col 0 :selection {:from 0 :to 0}}]
                                      }
                             :viewport {:pos [0 0]
                                        :view-size [0 0]
                                        :metrics nil
                                        :focused? false}})]
    (attach-lexer! *editor-state)
    *editor-state))

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

(comment
  (defn intersect-markers
    "Takes possibly intersecting markers; returns markers without intersection"
    [markers]
    (when (seq markers)
      (loop [[m1 & rest] markers
             result (transient [])
             pending (sorted-map)]
        (let [m2 (min-key :from (first rest) (second (first pending)))
              {from1 :from to1 :to} m1
              {from2 :from to2 :to} m2]
          (cond
            (nil? m2) (persistent! (conj! result m1))
            (<= to1 from2) (recur rest (conj! result m1) pending)
            :else (recur rest
                         (conj! result {:from from1
                                        :to from2
                                        :foreground (get m1 :foreground)})
                         (assoc pending
                           from2 {:from from2
                                  :to to1
                                  :foreground (str (:foreground m1) " " (:foreground m2))}
                           to1 {:from to1
                                :to to2
                                :foreground (:foreground m2)}))
            ))
        ))
    )

  )

(def render-text
  (measure-time "render-text"
                (fn [text tokens markup {:keys [height]}]
                  (let [markup (filter :foreground markup)
                        events (concat [{:pos 0 :add #{}} {:pos (count text) :remove #{}}]
                                       (mapcat (fn [m]
                                                 [{:pos (:from m) :add (:foreground m)}
                                                  {:pos (:to m) :remove (:foreground m)}]) markup)
                                       (second (reduce (fn [[i res] [len tt]]
                                                         [(+ i len)
                                                          (conj res {:pos i :add (token-class tt)}
                                                                {:pos (+ i len) :remove (token-class tt)})])
                                                       [0 []] tokens)))
                        events' (->> events
                                     (sort-by :pos)
                                     (partition-by :pos)
                                     (map (fn [es]
                                            {:pos (:pos (first es))
                                             :add (set (map :add es))
                                             :remove (set (map :remove es))})))]
                    (:res (reduce (fn [{:keys [prev res styles]} {:keys [pos add remove]}]
                                    {:prev pos
                                     :styles (clojure.set/union (clojure.set/difference styles remove) add)
                                     :res (push! res #js [:span {:class (->> styles
                                                                             (interpose " ")
                                                                             (apply str))}
                                                          (subs text prev pos)])})
                                  {:prev 0
                                   :styles (:add (first events'))
                                   :res #js [:pre {:class :line-text}]}
                                  (next events')))))))

(defn render-background-markup [markup {:keys [height width spacing]}]
  (reduce (fn [res {:keys [from to background]}]
            (if background
              (push! res #js [:div {:class background
                                    :style (style {:left (styles/px (* from width))
                                                   :width (styles/px (* width (- to from)))
                                                   :height (styles/px height)
                                                   :position :absolute})}])
              res))
          #js [:div {}]
          markup))

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
                                       (render-background-markup line-markup metrics)
                                       (render-selection selection metrics)
                                       (render-text line-text line-tokens line-markup metrics)
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
        {:keys [editor document viewport]} @state
        {:keys [pos view-size metrics]} viewport
        line-height (utils/line-height metrics)
        {:keys [text lines hashes]} document
        {:keys [caret selection]} editor
        [_ from-y-offset] pos
        [w h] view-size
        top-line (int (/ from-y-offset line-height))
        bottom-line (+ top-line (/ h line-height))
        y-shift (- (* line-height (- (/ from-y-offset line-height) top-line)))
        line-zipper (text/scan-to-line (text/zipper text) top-line)
        from-offset (text/offset line-zipper)
        to-offset (dec (text/offset (text/scan-to-line line-zipper (inc bottom-line))))
        caret-offset (get caret :offset)
        markup (intervals/query-intervals (:markup document) {:from from-offset :to to-offset})
        _ (styles/defstyle :render-line [:.render-line {:height (styles/px (utils/line-height metrics))
                                                        :position :relative
                                                        :overflow :hidden}])
        [_ hiccup] (reduce
                    (fn [[line-start res] index]
                      (let [next-line (text/scan-to-line line-start (inc index))
                            line-start-offset (text/offset line-start)
                            next-line-offset (text/offset next-line)
                            len (- next-line-offset
                                   line-start-offset)
                            len (if (tree/end? next-line)
                                  len
                                  (dec len))
                            line-text (text/text line-start len)
                            line-end-offset (+ line-start-offset len)
                            line-sel (line-selection selection [line-start-offset line-end-offset])
                            line-caret (when (and (<= line-start-offset caret-offset) (<= caret-offset line-end-offset))
                                         (- caret-offset line-start-offset))
                            line-tokens (or (get hashes (hash line-text)) (:tokens (get lines index)))
                            line-markup (prepare-markup markup line-start-offset line-end-offset)
                            line-info (LineInfo. line-text line-tokens line-markup line-sel line-caret index)]
                        [next-line (conj! res
                                          (el "div" (js-obj "key" index
                                                            "style" (js-obj "transform" (str "translate3d(0px, " (styles/px y-shift) ", 0px)")))
                                              [(el render-line #js {:key index
                                                                    :props {:line-info line-info
                                                                            :metrics metrics}})]))]))
                    [line-zipper
                     (transient [])]
                    (range top-line bottom-line))]
    (el "div" #js {:style #js {:background theme/background
                               :width "100%"
                               :overflow "hidden"}
                   :key "viewport"
                   :onMouseDown (fn [event]
                                  (when on-mouse-down
                                    (let [x (- ($ event :clientX) (-> event .-currentTarget .-offsetLeft))
                                          y (- ($ event :clientY) (-> event .-currentTarget .-offsetTop))]
                                      (on-mouse-down x y))))
                   :onMouseMove  (fn [event]
                                   (when on-drag-selection
                                     (when (= ($ event :buttons) 1)
                                       (let [x (- ($ event :clientX) (-> event .-currentTarget .-offsetLeft))
                                             y (- ($ event :clientY) (-> event .-currentTarget .-offsetTop))]
                                         (on-drag-selection x y)))))}
        (persistent! hiccup))))

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
                  on-input (aget props "onInput")]
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
                                      (on-input val)))}))))}))

(def editor-cmp
  (js/createReactClass
   #js {:componentDidMount
        (fn []
          (this-as cmp
            (let [*state ($ ($ cmp :props) :editorState)
                  *scheduled? (atom false)
                  *bindings (atom (keybind/make-bindings (:keymap @*state)))]
              (aset cmp "bindings" *bindings)
              (add-watch *state :editor-view
                         (fn [_ _ old-state new-state]
                           (when (and (not= old-state new-state) (not @*scheduled?))
                             (reset! *scheduled? true)
                             (next-tick (fn [time]
                                          (reset! *scheduled? false)
                                          ($ cmp forceUpdate))))))
              (when (not (ready-to-view? @*state))
                (go
                  (let [metrics (:font-metrics (a/<! *editors-common))]
                    (js/console.log "METRICS: " metrics)
                    (swap! *state assoc-in [:viewport :metrics] metrics)))))))

        :componentWillUnmount
        (fn []
          (this-as cmp
            (let [*state ($ ($ cmp :props) :editorState)]
              (remove-watch *state :editor-view))))

        :shouldComponentUpdate
        (fn []
          false)

        :render
        (fn []
          (this-as cmp
            (let [props ($ cmp :props)
                  *state ($ props :editorState)
                  {:keys [on-input on-mouse-down on-drag-selection on-resize on-scroll on-focus] :as callbacks} ($ props :callbacks)
                  *bindings ($ cmp :bindings)]
              (if (ready-to-view? @*state)
                (el "div" #js {:key "editor"
                               :style #js {:display "flex"
                                           :flex "1"
                                           :outline "transparent"}
                               :tabIndex -1
                               :onFocus on-focus}
                    #js [(el scroll (js-obj "key" "viewport"
                                            "props" {:child (el editor-viewport
                                                                #js {:key             "editor-viewport"
                                                                     :editorState     *state
                                                                     :onMouseDown     on-mouse-down
                                                                     :onDragSelection on-drag-selection})
                                                     :onResize on-resize
                                                     :onMouseWheel on-scroll}))
                         (el hidden-text-area-cmp
                             #js {:key "textarea"
                                  :isFocused (get-in @*state [:viewport :focused?])
                                  :onInput on-input})])
                (el "div" #js {:key "editor-placeholder"} #js ["EDITOR PLACEHOLDER"])))))}))

(defn editor-view [*editor-state callbacks]
  (el editor-cmp #js {:editorState *editor-state
                      :callbacks callbacks
                      :key "editor"}))
