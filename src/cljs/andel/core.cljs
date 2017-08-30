(ns andel.core
    (:require [andel.lexer :as lexer]
              [andel.theme :as theme]
              [andel.throttling :as throttling]
              [andel.controller :as contr]
              [andel.utils :as utils]
              [andel.intervals :as intervals]
              [andel.keybind :as keybind]
              [garden.core :as g]
              [garden.stylesheet :refer [at-keyframes]]
              [clojure.core.async :as a]
              [cljs-http.client :as http]
              [andel.text :as text]
              [andel.tree :as tree]
              [clojure.core.reducers :as r]
              [clojure.set]
              [clojure.tools.reader.edn :as edn]

              [create-react-class :as create-react-class]
              [react-dom :as react-dom])
    (:require-macros [reagent.interop :refer [$ $!]]
                     [cljs.core.async.macros :refer [go]]))

(defn el
  ([tag props]
   (.createElement js/React tag props))
  ([tag props children]
   (.createElement js/React tag props children)))

(defn head []
  (aget (js/document.getElementsByTagName "head") 0))

(defn body []
  js/document.body)

(defn- defstyle-impl
  ([key style]
   (let [id  (str "style_" (if key (name key) (hash-coll style)))
         e   (or (js/document.getElementById id)
                 (let [e (js/document.createElement "style")]
                   (aset e "id" id)
                   (aset e "type" "text/css")
                   (.appendChild (head) e)
                   e))
         css (g/css style)]
     (aset e "innerHTML" css)))
  ([style]
   (defstyle-impl nil style)))

(defonce defstyle (memoize defstyle-impl))

(defstyle :body
  [:body {:background theme/background}])

(defn px [x]
  (str x "px"))

(defn make-editor-state []
  {:document {:text (text/make-text "")
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
              :metrics {:height 0 :width 0 :spacing 0}}})

(def swap-editor! swap!)

(defonce state (atom (make-editor-state)))

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
                :height (px (utils/line-height metrics))
                :position :absolute
                :top (px 0)
                :left (if (infinity? to)
                        0
                        (px (* from width)))
                :margin-left (when (infinity? to) (px (* from width)))
                :width (if (infinity? to)
                         "100%"
                         (px (* (- to from) width)))})}])

(defstyle (garden.stylesheet/at-keyframes "blinker"
                                          ["50%" {:opacity "0"}]))

(defn render-caret [col {:keys [width] :as metrics}]
  #js [:div {}
       #js [:div {:style (style {:height (px (inc (utils/line-height metrics)))
                                 :width "100%"
                                 :background-color (:bg-05 theme/zenburn)
                                 :position :absolute
                                 :left 0
                                 :top 0
                                 :z-index "-1"})}]

       #js [:div {:style (style {:width (px 1)
                                 :animation "blinker 1s cubic-bezier(0.68, -0.55, 0.27, 1.55) infinite"
                                 :top 0
                                 :background-color "white"
                                 :position :absolute
                                 :left (px (* col width))
                                 :height (px (inc (utils/line-height metrics)))})}]])

(def token-class
  (let [tokens-cache #js {}]
    (fn [tt]
      (when tt
        (if-let [c (aget tokens-cache (name tt))]
          c
          (let [class (name tt)]
            (defstyle tt [(str "." class) (theme/token-styles tt)])
            (aset tokens-cache (name tt) class)
            class))))))

(defstyle :line-text [:.line-text {:position :absolute
                                   :left 0
                                   :user-select :none
                                   :top 0}])

(defn push! [a x]
  (.push a x)
  a)

(defn render-text [text tokens markup {:keys [height]}]
  (let [markup (filter :foreground markup)
        events (concat (mapcat (fn [m]
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
                             :remove (set (map :remove es))})))
        res (:res (reduce (fn [{:keys [prev res styles]} {:keys [pos add remove]}]
                              {:prev pos
                               :styles (clojure.set/union (clojure.set/difference styles remove) add)
                               :res (push! res #js [:span {:class (->> styles
                                                                       (interpose " ")
                                                                       (apply str))}
                                                    (subs text prev pos)])})
                            {:prev 0
                             :styles (:add (first events'))
                             :res #js [:pre {:class :line-text}]}
                            (next events')))]
    res))

(defn render-background-markup [markup {:keys [height width spacing]}]
  (reduce (fn [res {:keys [from to background]}]
            (if background
              (push! res #js [:div {:class background
                                    :style (style {:left (px (* from width))
                                                   :width (px (* width (- to from)))
                                                   :height (px (+ height spacing))
                                                   ;:z-index "-1"
                                                   :position :absolute})}])
              res))
          #js [:pre {}]
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
                                     :onWheel on-mouse-wheel}
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
        _ (defstyle :render-line [:.render-line {:height (px (utils/line-height metrics))
                                                 :position :relative}])
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
                                                            "style" (js-obj "transform" (str "translate3d(0px, " (px y-shift) ", 0px)")))
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
                                    (swap-editor! state #(contr/set-caret-at-grid-pos % line-col false))))
                   :onMouseMove  (fn [event]
                                   (when (= ($ event :buttons) 1)
                                     (let [x ($ event :clientX)
                                           y ($ event :clientY)
                                           line-col (utils/pixels->grid-position [x y] from y-shift metrics)]
                                       (swap-editor! state #(contr/set-caret-at-grid-pos % line-col true)))))}
        (persistent! hiccup))))

(defn scroll-on-event [state]
  (fn [evt]
    (let [{:keys [viewport document]} @state
          screen-height (get-in viewport [:view-size 1])
          line-height (utils/line-height (:metrics viewport))
          lines-count (text/lines-count (get document :text))
          document-height (- (* lines-count line-height) (/ screen-height 2))]
      (swap-editor! state
                    #(update-in % [:viewport :pos]
                      (fn [[x y]]
                        (let [dx (.-deltaX evt)
                              dy (.-deltaY evt)]
                          (if (< (js/Math.abs dx) (js/Math.abs dy))
                            [x (min document-height (max 0 (+ y dy)))]
                            [(max 0 (+ x dx)) y])))))
      (.preventDefault evt))))

(defn set-viewport-size [state width height]
  (swap-editor! state #(assoc-in % [:viewport :view-size] [width height])))

(defn foo []
  (swap! state update :foo #(if (nil? %) 1 (inc %))))

(def next-tick
  (let [w js/window]
    (or ($ w :requestAnimationFrame)
        ($ w :webkitRequestAnimationFrame)
        ($ w :mozRequestAnimationFrame)
        ($ w :msRequestAnimationFrame))))

(def editor
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
                                                                   :editorState state})
                                                   :onResize (partial set-viewport-size *state)
                                                   :onMouseWheel (scroll-on-event *state)}))
                       (el "textarea"
                           #js {:key "textarea"
                                :ref "textarea"
                                :autoFocus true
                                :style #js {:opacity 0
                                            :pading  "0px"
                                            :border  :none
                                            :height  "0px"
                                            :width   "0px"}
                                :onInput (fn [evt]
                                           (let [e   (.-target evt)
                                                 val (.-value e)]
                                             (set! (.-value e) "")
                                             (swap-editor! *state contr/type-in val)))})]))))}))

(def main  (el "div"
               #js {:style #js {:display "flex"
                                :flex "1"}
                         :key "main"}
               [(el editor #js {:editorState state
                                :key "editor"
                                })]))

(defn include-script [src]
  (let [e (js/document.createElement "script")
        res (a/promise-chan)]
    (aset e "onload" #(a/put! res :done))
    (doto e
      (.setAttribute "type" "text/javascript")
      (.setAttribute "src" src))
    (.appendChild (head) e)
    res))

(defn include-style [src cb]
  (let [e (js/document.createElement "link")]
    (aset e "onload" cb)
    (doto e
      (.setAttribute "type" "text/css")
      (.setAttribute "rel" "stylesheet")
      (.setAttribute "href" src))
    (.appendChild (head) e)))

(defn deliver-lexems! [{:keys [req-ts tokens index text]} state-ref]
  (let [res (swap-editor! state-ref
                         (fn [{:keys [document] :as state}]
                           (let [{:keys [timestamp]} document]
                             (if (= timestamp req-ts)
                               (-> state
                                   (assoc-in [:document :lines index :tokens] tokens)
                                   (assoc-in [:document :hashes (hash text)] tokens)
                                   (assoc-in [:document :first-invalid] (inc index)))
                               state))))]
       (= (get-in res [:document :timestamp]) req-ts)))

(defn attach-lexer! [state-ref]
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

(defn wait-for-all [cs]
  (let [m (a/merge cs)]
    (go (dotimes [i (count cs)] (a/<! m)))))

(defn font->str [font-name height]
  (str height "px " font-name))

(defn measure [font-name height spacing]
  (let [canvas (js/document.createElement "canvas")
        ctx (.getContext canvas "2d")]
    (set! (.-font ctx) (font->str font-name height))
    (let [width (.-width (.measureText ctx "X"))]
        {:width width :height height :spacing spacing})))

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

(defn setup-font! [*state font-type font-size spacing]
  (go
    (let [metrics (a/<! (measure-async font-type font-size spacing))]
      (swap-editor! *state
                    (fn [state]
                      (let [{:keys [height]} metrics]
                        (defstyle :editor
                          [:pre
                           {:font-family font-type
                            :font-size   (px height)
                            :color       theme/foreground
                            :margin      "0px"}])
                        (assoc-in state [:viewport :metrics] metrics)))))))

(def editor-text "public class Main {
    public static void main(String[] args) {
    }
}")

(defn style->class [style]
  (let [name (str "style__" (hash-coll style))]
    (defstyle [(str "." name) style])
    name))

;; proto-marker-map -> marker-record
(defn create-marker [proto-marker]
  (letfn [(class-by-keys [ks style]
            (let [style (select-keys style ks)]
              (when (not-empty style)
                (style->class style))))
          (classes-by-keys [ks styles]
            (let [classes (->> styles
                               (map (partial class-by-keys ks))
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

(defn load! []
  (js/window.addEventListener "keydown" (keybind/dispatcher) true)
  (let [loaded (a/promise-chan)]
    (go
      ;load CodeMirror first
      (a/<! (wait-for-all (map include-script ["resources/public/codemirror/addon/runmode/runmode-standalone.js"
                                               "resources/public/codemirror/addon/runmode/runmode-standalone.js"
                                               "resources/public/codemirror/mode/javascript/javascript.js"
                                               "resources/public/codemirror/mode/clike/clike.js"
                                               "resources/public/codemirror/mode/clojure/clojure.js"])))
      (a/<! (setup-font! state "Fira Code" 16 3))
      ;run lexer worker and setup atom watcher that will run lexer on changes
      (attach-lexer! state)
      (add-watch state :lexer
                 (fn [_ _ old-s new-s]
                   (let [old-ts (get-in old-s [:document :timestamp])
                         new-ts (get-in new-s [:document :timestamp])
                         broker (get-in new-s [:document :lexer-broker])]
                     (when (not= old-ts new-ts)
                       (a/put! broker new-s)))))
      ;load sample document from the internet
      (let [text (:body (a/<! (http/get "resources/public/EditorImpl.java")))]
        (swap-editor! state contr/set-text text))
      (let [markup (->> (:body (a/<! (http/get "resources/public/markup.txt")))
                        edn/read-string
                        (sort-by :from))]
        (js/console.log (str "MARKUP LOADED: " (count  markup)))
        #_(swap-editor! state (fn [s] (assoc-in s [:raw-markers] (map intervals/map->Marker markup))))
        (swap-editor! state (fn [s] (assoc-in s [:document :markup] (-> (intervals/make-interval-tree)
                                                                        (intervals/add-intervals (map create-marker markup)))))))
      ;deliver promise
      (a/>! loaded :done))
    loaded))

(defonce ready (load!))

(defn mount-root []
  (go (a/<! ready)
      (let [root (.getElementById js/document "app")]
        (.render js/ReactDOM main root))))

(defn init! []
  (mount-root))

(defn capture [f]
  (fn [evt _]
    (f)
    (.stopPropagation evt)
    (.preventDefault evt)))

(defn- bind-function! [key f & args]
  (keybind/bind! key :global (capture #(swap-editor! state (fn [s] (apply f s args))))))

(bind-function! "backspace" contr/backspace)
(bind-function! "delete" contr/delete)
(bind-function! "pgup" contr/pg-move :up false)
(bind-function! "pgdown" contr/pg-move :down false)
(bind-function! "shift-pgup" contr/pg-move :up true)
(bind-function! "shift-pgdown" contr/pg-move :down true)
(bind-function! "home" contr/home false)
(bind-function! "shift-home" contr/home true)
(bind-function! "end" contr/end false)
(bind-function! "shift-end" contr/end true)
(bind-function! "tab" (fn [state] (contr/type-in state "    ")))
(bind-function! "left" contr/move-caret :left false)
(bind-function! "right" contr/move-caret :right false)
(bind-function! "up" contr/move-caret :up false)
(bind-function! "down" contr/move-caret :down false)
(bind-function! "shift-left" contr/move-caret :left true)
(bind-function! "shift-right" contr/move-caret :right true)
(bind-function! "shift-up" contr/move-caret :up true)
(bind-function! "shift-down" contr/move-caret :down true)
(bind-function! "esc" contr/drop-selection-on-esc)
(bind-function! "enter" contr/on-enter)



;; benchmarks

(defn current-time! []
  (.now js/Date))

(defn text-tree-info [t]
  (loop [acc {:nodes 0 :leafs 0}
         loc (text/zipper t)]
    (if (tree/end? loc)
      (js/console.log (str "TEXT: " acc))
      (if (tree/node? (tree/node loc))
        (recur (update acc :nodes inc) (tree/next loc))
        (recur (update acc :leafs inc) (tree/next loc))))))

(defn intervals-tree-info [t]
  (loop [acc {:nodes 0 :leafs 0}
         loc (intervals/zipper t)]
    (if (tree/end? loc)
      (js/console.log (str "INTERVALS: " acc))
      (if (tree/node? (tree/node loc))
        (recur (update acc :nodes inc) (tree/next loc))
        (recur (update acc :leafs inc) (tree/next loc))))))

(defn bench [name f & {:keys [count] :or {count 10}}]
  (let [start-time (current-time!)]
    (js/console.log (str "START BENCH " name))
    (mapv (fn [f] (f)) (repeat count f))
    (let [end-time (current-time!)
          total-time (- end-time start-time)]
      (js/console.log (str "END BENCH: " name " "
                            {:count count
                             :total total-time
                             :average (/ total-time count)})))))

(defn bench-insert [markup]
  (bench "TREE INSERT"
         (fn []
           (-> (intervals/make-interval-tree)
               (intervals/add-intervals markup)))
         :count 1))

(defn bench-insert-base [markup]
  (bench "BASE INSERT"
   (fn []
     (mapv (fn [m] (update m :from inc)) markup))
   :count 100))

(defn bench-query [markup]
  (let [itree (-> (intervals/make-interval-tree)
                  (intervals/add-intervals markup))]
    (bench "TREE QUERY"
           (fn []
             (let [from (rand-int 160000)
                   to (+ from 3200)]
               (intervals/query-intervals itree {:from from :to to})))
           :count 10000)))

(defn play-query [model {:keys [from to]}]
  (vec (filter #(intervals/intersect % {:from from :to to}) model)))

(defn bench-query-base [markup]
  (bench "QUERY BASE"
         (fn []
           (let [from (rand-int 160000)
                 to (+ from 3200)]
             (play-query markup {:from from :to to})))
         :count 1000))

(defn bench-type-in [markup]
  (let [itree (-> (intervals/make-interval-tree)
                  (intervals/add-intervals markup))]
    (bench "TYPE-IN BENCH"
           (fn []
             (let [offset (rand-int 160000)
                   size 1]
               (intervals/type-in itree [offset size])))
           :count 1000)))

(defn bench-delete [markup]
  (let [itree (-> (intervals/make-interval-tree)
                  (intervals/add-intervals markup))]
    (bench "DELETE BENCH"
           (fn []
             (let [offset (rand-int 160000)
                   size 1]
               (intervals/delete-range itree [offset size])))
           :count 1)))

(defn bench-editing [markup]
  (let [itree (-> (intervals/make-interval-tree)
                  (intervals/add-intervals markup))]
    (bench "TREE EDITING"
           (fn []
             (let [cmd (rand-nth [:insert :delete])]
               (case cmd
                 :insert ))))))

(bind-function! "ctrl-b" (fn [s]
                           (let [markup (:raw-markers s)
                                 interval-tree (get-in s [:document :markup])
                                 text-tree (get-in s [:document :text])]
                             #_(text-tree-info text-tree)
                             (intervals-tree-info interval-tree)
                             #_(bench-insert markup)
                             #_(bench-insert-base markup)
                             #_(bench-query markup)
                             #_(bench-query-base markup)
                             #_(bench-type-in markup)
                             #_(bench-delete markup))
                           (js/alert "BENCH DONE")
                           s))
