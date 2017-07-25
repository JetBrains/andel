(ns andel.core
    (:require [andel.lexer :as lexer]
              [andel.theme :as theme]
              [andel.throttling :as throttling]
              [andel.controller :as contr]
              [andel.utils :as utils]
              [reagent.core :as reagent]
              [reagent.ratom :refer [track]]
              [reagent.session :as session]
              [andel.keybind :as keybind]
              [garden.core :as g]
              [clojure.core.async :as a]
              [cljs-http.client :as http]
              [hiccups.runtime :as hiccups]
              [andel.text :as text]
              [andel.tree :as tree]
              [clojure.core.reducers :as r])
    (:require-macros [reagent.interop :refer [$ $!]]
                     [reagent.ratom :refer [reaction]]
                     [cljs.core.async.macros :refer [go]]))

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

(defn font->str [font-name height]
  (str height "px " font-name))

(defn measure [font-name height spacing]
  (let [canvas (js/document.createElement "canvas")
        ctx (.getContext canvas "2d")]
    (set! (.-font ctx) (font->str font-name height))
    (let [width (.-width (.measureText ctx "X"))]
        {:width width :height height :spacing spacing})))

(defn after-font-loaded [font-name size spacing callback]
  (measure font-name size spacing)
  (if (.. js/document
          -fonts
          (check (font->str font-name size)))
    (callback)
    (js/setTimeout #(after-font-loaded font-name size callback) 100)))

(defn make-editor-state []
  {:document {:text (text/make-text "")
              :lexer-broker (a/chan)
              :modespec "text/x-java"
              :timestamp 0
              :lines []
              :first-invalid 0}
   :editor {:caret {:offset 0 :v-col 0}
            :selection [0 0]}
   :viewport {:ready? false
              :pos [0 0]
              :view-size [0 0]
              :metrics {:height 0 :width 0 :spacing 0}}})

(def swap-editor! swap!)

(defonce state (reagent/atom (make-editor-state)))

(let [font {:type "Fira Code"
            :size 16
            :spacing 3}]
  (after-font-loaded (:type font) (:size font) (:spacing font)
                     (fn []
                       (swap-editor! state
                                     (fn [editor]
                                       (let [{:keys [width height] :as metrics} (measure (:type font) (:size font) (:spacing font))]
                                         (defstyle :editor
                                           [:pre
                                            {:font-family (:type font)
                                             :font-size   (px height)
                                             :color       theme/foreground
                                             :margin      "0px"}])
                                         (-> editor
                                             (assoc-in [:viewport :ready?] true)
                                             (assoc-in [:viewport :metrics] metrics))))))))

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

(defn dom [[tag x & r :as el]]
  (assert (some? el))
  (let [[attrs-map children] (if (map? x) [x r] [nil (cons x r)])]
    (reduce (fn [n [a v]]
              (assoc-attr! n a v))
            (reduce (fn [n c]
                      (if (some? c)
                        (conj-child! n (if (string? c)
                                         (make-text-node c)
                                         (dom c)))
                        n))
                    (make-node tag)
                    children)
            attrs-map)))

(defn infinity? [x] (keyword? x))

(defn render-selection [[from to] {:keys [width height spacing]}]
  #js [:div
       {:style
        (style {:background-color theme/selection
                :height (px (+ spacing height))
                :position :absolute
                :top (px 0)
                :left (if (infinity? to)
                        0
                        (px (* from width)))
                :margin-left (when (infinity? to) (px (* from width)))
                :width (if (infinity? to)
                         "100%"
                         (px (* (- to from) width)))})}])

(defn render-caret [col {:keys [width height spacing]}]
  #js [:div {:style (style {:width "1px"
                            :animation "blinker 1s cubic-bezier(0.68, -0.55, 0.27, 1.55) infinite"
                            :top 0
                            :background-color "red"
                            :position :absolute
                            :left (px (* col width))
                            :height (px (inc (+ spacing height)))})}])



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
                                   :top 0}])

(defn push! [a x]
  (.push a x)
  a)

(defn render-text [text tokens {:keys [height]}]
  (let [[i res] (reduce (fn [[i res] [len tt]]
                          [(+ i len)
                           (push! res #js [:span {:class (token-class tt)}
                                           (subs text i (+ i len))])])
                        [0 #js [:pre {:class :line-text}]]
                        tokens)]
    (push! res #js [:span (subs text i)])))

(def real-dom
  (reagent/create-class
   {:component-will-update
    (fn [this [_ elt]]
      (let [node (reagent/dom-node this)
            child (.-firstChild node)]
        (when child
          (.removeChild node child))
        (.appendChild node elt)))
    :component-did-mount
    (fn [this]
      (let [[_ elt] (reagent/argv this)
            node (reagent/dom-node this)]
        (.appendChild node elt)))
    :render (fn [_] [:div])}))

(defrecord LineInfo [line-text line-tokens selection caret-index index])

(defn render-line [{:keys [line-text line-tokens selection caret-index] :as line-info} {:keys [height spacing] :as metrics}]
  (let [_ (defstyle :render-line [:.render-line {:height (px (+ spacing height))
                                                 :position :relative}])]
    [real-dom (dom
                #js [:div {:class :render-line}
                     (render-selection selection metrics)
                     (render-text line-text line-tokens metrics)
                     (when caret-index (render-caret caret-index metrics))])]))

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

(defn on-mouse-action [state line-col selection?]
  (swap-editor! state #(contr/set-caret-at-line-col % line-col selection?)))

(defn init-viewport [state]
  (fn [width height]
    (swap-editor! state #(assoc-in % [:viewport :view-size] [width height]))))

(defn scroll-on-event [state]
  (fn [evt]
    (swap-editor! state
                  #(update-in % [:viewport :pos]
                              (fn [[x y]]
                                (let [dx (/ (.-wheelDeltaX evt) 2)
                                      dy (/ (.-wheelDeltaY evt) 2)]
                                  (if (< (js/Math.abs dx) (js/Math.abs dy))
                                    [x (max 0 (- y dy))]
                                    [(max 0 (- x dx)) y])))))
    (.preventDefault evt)))

(defn scroll [viewport init scroll-on-event]
  (let [once (atom true)]
    (fn []
      [:div {:style {:display :flex
                     :flex "1"
                     :overflow :hidden}
             :ref (fn [e]
                    (when e
                      (init (.-clientWidth e) (.-clientHeight e)))
                    (when (and @once (some? e))
                      (reset! once false)
                      (.addEventListener
                       e
                       "mousewheel"
                       scroll-on-event)))}
       [viewport]])))

;; Todo: untangle all this spaghetti bindings
(defn editor-viewport [state]
  (fn []
    (let [{:keys [editor document viewport]} @state
          {:keys [pos view-size metrics]} viewport
          {:keys [height]} metrics
          {:keys [text lines]} document
          {:keys [caret selection]} editor
          [_ from-y-offset] pos
          [w h] view-size
          from (int (/ from-y-offset height))
          to (+ 5 (+ from (/ h height)))
          y-shift (- (* height (- (/ from-y-offset height) from)))
          caret-offset (get caret :offset)
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
                              line-tokens (:tokens (get lines index))
                              line-info (LineInfo. line-text line-tokens line-sel line-caret index)]
                          [next-line (conj! res
                                            ^{:key index}
                                            [render-line line-info metrics])]))
                      [(text/scan-to-line (text/zipper text) from)
                       (transient [:div {:style
                                         {:background theme/background
                                          :width "100%"
                                          :transform (str "translate3d(0px, " y-shift "px, 0px)")}
                                         :onMouseDown (fn [event]
                                                        (let [x ($ event :clientX)
                                                              y ($ event :clientY)]
                                                          (on-mouse-action state (utils/pixels->line-col [x y] from y-shift metrics)
                                                                            false)))
                                         :onMouseMove  (fn [event]
                                                         (when (= ($ event :buttons) 1)
                                                           (let [x ($ event :clientX)
                                                                 y ($ event :clientY)]
                                                             (on-mouse-action state (utils/pixels->line-col [x y] from y-shift metrics)
                                                                               true))))}])]
                      (range from to))]
      (persistent! hiccup))))

(defn editor [state]
  (if-not (get-in @state [:viewport :ready?])
    [:h1 "Font not loaded yet..."]
    (let [dom-input (atom nil)
          listener (atom nil)]
      (fn []
        [:div {:style {:display :flex
                       :flex 1}
               :tab-index -1
               :ref (fn [this]
                      (when-let [node (reagent/dom-node this)]
                        (reset! listener true)
                        (.addEventListener node "focus"
                                           (fn []
                                             (when @dom-input
                                               (.focus @dom-input))))))}
         [scroll (editor-viewport state) (init-viewport state) (scroll-on-event state)]
         [:textarea
          {:ref (fn [this]
                  (when-let [dom-node (reagent/dom-node this)]
                    (reset! dom-input dom-node)))
           :auto-focus true
           :style {:opacity 0
                   :pading "0px"
                   :border :none
                   :height "0px"
                   :width "0px"}
           :on-input (fn [evt]
                       (let [e (.-target evt)
                             val (.-value e)]
                         (set! (.-value e) "")
                         (swap-editor! state contr/type-in val)))}]]))))

(defn main []
  [:div {:style {:display :flex
                 :flex "1"}}
   [editor state]])

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

(defn deliver-lexems! [{:keys [req-ts tokens index]} state-ref]
  (let [res (swap-editor! state-ref
                         (fn [{:keys [document] :as state}]
                           (let [{:keys [timestamp]} document]
                             (if (= timestamp req-ts)
                               (-> state
                                   (assoc-in [:document :lines index :tokens] tokens)
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

(defn load! []
  (js/window.addEventListener "keydown" (keybind/dispatcher) true)
  (let [loaded (a/promise-chan)]
    (go
      ;load CodeMirror first
      (a/<! (wait-for-all (map include-script ["/codemirror/addon/runmode/runmode-standalone.js"
                                               "/codemirror/addon/runmode/runmode-standalone.js"
                                               "/codemirror/mode/javascript/javascript.js"
                                               "/codemirror/mode/clike/clike.js"
                                               "/codemirror/mode/clojure/clojure.js"])))
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
      (let [text (:body (a/<! (http/get "/EditorImpl.java")))]
        (swap-editor! state contr/set-text text))
      ;deliver promise
      (a/>! loaded :done))
    loaded))

(defonce ready (load!))

(defn mount-root []
  (go (a/<! ready)
      (let [root (.getElementById js/document "app")]
        (reagent/render [main] root))))

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
