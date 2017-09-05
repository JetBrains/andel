(ns andel.editor
  (:require [clojure.core.async :as a]
            [clojure.set]

            [create-react-class :as create-react-class]
            [react-dom :as react-dom]
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
      (dotimes [pr (map styles/include-script ["resources/public/codemirror/addon/runmode/runmode-standalone.js"
                                               "resources/public/codemirror/addon/runmode/runmode-standalone.js"
                                               "resources/public/codemirror/mode/javascript/javascript.js"
                                               "resources/public/codemirror/mode/clike/clike.js"
                                               "resources/public/codemirror/mode/clojure/clojure.js"])]
        (a/<! pr))
      (let [{:keys [height font-name] :as font-metrics} (a/<! (measure-async "Fira Code" 16 3))]
        (styles/defstyle :editor
          [:pre
           {:font-family font-name
            :font-size   (styles/px height)
            :color       theme/foreground
            :margin      "0px"}])
        (styles/defstyle (garden.stylesheet/at-keyframes "blinker"
                                                         ["50%" {:opacity "0"}]))
        (styles/defstyle :line-text [:.line-text {:position :absolute
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
  (let [promise (a/promise-chan)]
    (go
      (let [editors-common (a/<! *editors-common)
            *editor-state (atom {:document {:text (text/make-text "")
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
                                            :metrics (:font-metrics editors-common)}})]
        (attach-lexer! *editor-state)
        (a/>! promise *editor-state)))
    promise))


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

(defn aset! [coll key value]
  (aset coll key value)
  coll)

(defrecord HighlightEvent [pos add remove])

(defrecord EventAcc [prev styles res])

(defn event-comp [e1 e2]
  (< (.-pos e1) (.-pos e2)))

(defn partition-by-pos [events]
  (reduce (fn [acc e]
            (if e
              (let [pos (.-pos e)
                    add (.-add e)
                    remove (.-remove e)]
                (if (aget acc pos)
                  (aset! acc pos (push! (aget acc pos) e))
                  (aset! acc pos #js [e])))))
          #js []
          events))

(defn render-text [text tokens foreground-markup {:keys [height]}]
  (let [markup-events (reduce (fn [res m]
                                (push! res (HighlightEvent. (:from m) (:foreground m) nil))
                                (push! res (HighlightEvent. (:to m) nil (:foreground m))))
                              #js [] foreground-markup)
        token-events (second (reduce (fn [[i res] [len tt]]
                                       [(+ i len)
                                        (push! res (HighlightEvent. i (token-class tt) nil))
                                        (push! res (HighlightEvent. (+ i len) nil (token-class tt)))])
                                     [0 #js []] tokens))
        events (.concat markup-events token-events)
        events' (->> events
                     ((fn [es] (.sort es event-comp)))
                     partition-by-pos

                     ((fn [es] (.map es (fn [e]
                                          (HighlightEvent. (.-pos (first e))
                                                           (set (map #(.-add %) e))
                                                           (set (map #(.-remove %) e))))))))]
    (.-res (reduce (fn [event-acc event]
                    (if event
                      (let [prev (.-prev event-acc)
                            res (.-res event-acc)
                            styles (.-styles event-acc)
                            pos (.-pos event)
                            add (.-add event)
                            remove (.-remove event)]
                        (EventAcc. pos
                                   (clojure.set/union (clojure.set/difference styles remove) add)
                                   (push! res #js [:span {:class (->> styles
                                                                      (interpose " ")
                                                                      (apply str))}
                                                   (subs text prev pos)])))
                      event-acc))
                  (EventAcc. 0 (some-> (aget events' 0) .-add) #js [:pre {:class :line-text}])
                  (.slice events' 1)))))

(defn render-background-markup [background-markup {:keys [height width spacing]}]
  (reduce (fn [res {:keys [from to background]}]
            (push! res #js [:div {:class background
                                  :style (style {:left (styles/px (* from width))
                                                 :width (styles/px (* width (- to from)))
                                                 :height (styles/px height)
                                                 :position :absolute})}])
            res)
          #js [:pre {}]
          background-markup))

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

(defrecord LineInfo [lineText lineTokens lineForegroundMarkup lineBackgroundMarkup selection caretIndex index])

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
              line-foreground-markup (.-lineForegroundMarkup line-info)
              line-background-markup (.-lineBackgroundMarkup line-info)
              selection (.-selection line-info)
              caret-index (.-caretIndex line-info)]
          (el real-dom #js {:dom (dom
                                  #js [:div {:class :render-line}
                                       (render-background-markup line-background-markup metrics)
                                       (render-selection selection metrics)
                                       (render-text line-text line-tokens line-foreground-markup metrics)
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
                                     :onWheel on-mouse-wheel}
                          [child]))))}))

;; proto-marker -> (foreground markup, background markup)
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
                                  (.-foreground marker))))
       ((fn [ms] [(filter :foreground ms)
                  (filter :background ms)]))))

(defn editor-viewport [props]
  (let [state ($ props :editorState)
        {:keys [editor document viewport]} @state
        {:keys [pos view-size metrics]} viewport
        line-height (utils/line-height metrics)
        {:keys [text lines hashes]} document
        {:keys [caret selection]} editor
        [_ from-y-offset] pos
        [w h] view-size
        from (int (/ from-y-offset line-height))
        to (+ from (/ h line-height))
        y-shift (- (* line-height (- (/ from-y-offset line-height) from)))
        line-zipper (text/scan-to-line (text/zipper text) from)
        from-offset (text/offset line-zipper)
        to-offset (dec (text/offset (text/scan-to-line line-zipper (inc to))))
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
                            [line-foreground-markup line-background-markup] (prepare-markup markup line-start-offset line-end-offset)
                            line-info (LineInfo. line-text line-tokens line-foreground-markup line-background-markup line-sel line-caret index)]
                        [next-line (conj! res
                                          (el "div" (js-obj "key" index
                                                            "style" (js-obj "transform" (str "translate3d(0px, " (styles/px y-shift) ", 0px)")))
                                              [(el render-line #js {:key index
                                                                    :props {:line-info line-info
                                                                            :metrics metrics}})]))]))
                    [line-zipper
                     (transient [])]
                    (range from to))]
    (el "div" #js {:style #js {:background theme/background
                               :width "100%"}
                   :key "viewport"
                   :onMouseDown (fn [event]
                                  (let [x ($ event :clientX)
                                        y ($ event :clientY)
                                        line-col (utils/pixels->grid-position [x y] from y-shift metrics)]
                                    (swap! state #(controller/set-caret-at-grid-pos % line-col false))))
                   :onMouseMove  (fn [event]
                                   (when (= ($ event :buttons) 1)
                                     (let [x ($ event :clientX)
                                           y ($ event :clientY)
                                           line-col (utils/pixels->grid-position [x y] from y-shift metrics)]
                                       (swap! state #(controller/set-caret-at-grid-pos % line-col true)))))}
        (persistent! hiccup))))

(defn scroll-on-event [state]
  (fn [evt]
    (let [{:keys [viewport document]} @state
          screen-height (get-in viewport [:view-size 1])
          line-height (utils/line-height (:metrics viewport))
          lines-count (text/lines-count (get document :text))
          document-height (- (* lines-count line-height) (/ screen-height 2))]
      (swap! state
             #(update-in % [:viewport :pos]
                         (fn [[x y]]
                           (let [dx (.-deltaX evt)
                                 dy (.-deltaY evt)]
                          (if (< (js/Math.abs dx) (js/Math.abs dy))
                            [x (min document-height (max 0 (+ y dy)))]
                            [(max 0 (+ x dx)) y])))))
      (.preventDefault evt))))

(defn set-viewport-size [state width height]
  (swap! state #(assoc-in % [:viewport :view-size] [width height])))

(def next-tick
  (let [w js/window]
    (or ($ w :requestAnimationFrame)
        ($ w :webkitRequestAnimationFrame)
        ($ w :mozRequestAnimationFrame)
        ($ w :msRequestAnimationFrame))))

(def editor-cmp
  (js/createReactClass
   #js {:componentDidMount
        (fn []
          (this-as cmp
            (let [*state ($ ($ cmp :props) :editorState)
                  *scheduled? (atom false)]
              (add-watch *state :editor-view
                         (fn [_ _ old-state new-state]
                           (when (and (not= old-state new-state) (not @*scheduled?))
                             (reset! *scheduled? true)
                             (next-tick (fn []
                                          (reset! *scheduled? false)
                                          ($ cmp forceUpdate)))))))))

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
            (let [*state ($ ($ cmp :props) :editorState)]
              (el "div" #js {:key "editor"
                             :style #js {:display "flex"
                                         :flex "1"}
                             :tabIndex -1
                             :onFocus (fn []
                                        (let [ta ($ ($ cmp :refs) :textarea)]
                                          (when ta (.focus ta))))}
                  #js [(el scroll (js-obj "key" "viewport"
                                          "props" {:child (el editor-viewport
                                                              #js {:key "editor-viewport"
                                                                   :editorState *state})
                                                   :onResize (partial set-viewport-size *state)
                                                   :onMouseWheel (scroll-on-event *state)}))
                       (el "textarea"
                           #js {:key "textarea"
                                :ref "textarea"
                                :autoFocus true
                                :style #js {:opacity 0
                                            :padding  "0px"
                                            :border  "none"
                                            :height  "0px"
                                            :width   "0px"}
                                :onInput (fn [evt]
                                           (let [e   (.-target evt)
                                                 val (.-value e)]
                                             (set! (.-value e) "")
                                             (swap! *state controller/type-in val)))})]))))}))

(defn editor-view [*editor-state]
  (el editor-cmp #js {:editorState *editor-state
                      :key "editor"}))
