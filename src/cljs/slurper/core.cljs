(ns slurper.core
    (:require [slurper.lexer :as lexer]
              [slurper.theme :as theme]
              [slurper.throttling :as throttling]
              [reagent.core :as reagent]
              [reagent.ratom :refer [track]]
              [reagent.session :as session]
              [slurper.keybind :as keybind]
              [garden.core :as g]
              [clojure.core.async :as a]
              [cljs-http.client :as http]
              [hiccups.runtime :as hiccups]
              [slurper.text :as text]
              [slurper.tree :as tree]
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

(def line-h 19)

(defstyle :editor
  [:pre {:font-family "Menlo"
         :color theme/foreground
         :margin "0px"}])

(defstyle :body
  [:body {:background theme/background}])

(defn measure [s]
  (let [canvas (js/document.createElement "canvas")
        ctx (.getContext canvas "2d")]
    (set! (.-font ctx) "16px Menlo")
    (let [res {:width (.-width (.measureText ctx s)) :height line-h}]
      (js/console.log (:width res) " measured")
      res)))

(defn make-editor-state []
  (let [ch (a/chan)]
    {:text (text/make-text "")
     :selection [49 4956]
     :caret 0
     :lexer-broker ch
     :modespec "text/x-java"
     :timestamp 0
     :lines []
     :first-invalid 0}))

(defn px [x]
  (str x "px"))

(defonce state (reagent/atom (make-editor-state)))

(defonce on-keydown (keybind/dispatcher))

(defonce keys-dispatcher (js/window.addEventListener "keydown" on-keydown true))

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

(defn scroll [size viewport]
  (let [pos (reagent/atom [0 0])
        view-size (reagent/atom [0 0])
        once (atom true)]
    (fn []
      [:div {:style {:display :flex
                     :flex "1"
                     :overflow :hidden}
             :ref (fn [e]
                    (when e
                      (reset! view-size [(.-clientWidth e)
                                         (.-clientHeight e)]) 100)
                    (when (and @once (some? e))
                      (reset! once false)
                      (.addEventListener
                       e
                       "mousewheel"
                       (fn [evt]
                         (swap! pos
                                (fn [[x y]]
                                  (let [dx (/ (.-wheelDeltaX evt) 2)
                                        dy (/ (.-wheelDeltaY evt) 2)
                                        [width height] @size]
                                    (if (< (js/Math.abs dx) (js/Math.abs dy))
                                      [x (max 0 (- y dy))]
                                      [(max 0 (- x dx)) y]))))
                         (.preventDefault evt)))))}
       [viewport pos view-size]])))

(defn infinity? [x] (keyword? x))

(defn render-selection [[from to] {:keys [width height]}]
  #js [:div
       {:style
        (style {:background-color theme/selection
                :height (px height)
                :position :absolute
                :top (px 0)
                :left (if (infinity? to)
                        0
                        (px (* from width)))
                :margin-left (when (infinity? to) (px (* from width)))
                :width (if (infinity? to)
                         "100%"
                         (px (* (- to from) width)))})}])

(def metrics (measure "x"))

(defn render-caret [col {:keys [width height]}]
  #js [:div {:style (style {:width "1px"
                            :animation "blinker 1s cubic-bezier(0.68, -0.55, 0.27, 1.55) infinite"
                            :top 0
                            :background-color "red"
                            :position :absolute
                            :left (px (* col width))
                            :height (px (inc height))})}])



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

(defstyle :render-line [:.render-line {:height (px line-h)
                                       :position :relative}])

(defrecord LineInfo [line-text line-tokens selection caret-index index])

(defn render-line [{:keys [line-text line-tokens selection caret-index] :as line-info} {:keys [height] :as metrics}]
  [real-dom (dom
               #js [:div {:class :render-line}
                    (render-selection selection metrics)
                    (render-text line-text line-tokens metrics)
                    (when caret-index (render-caret caret-index metrics))])])

(defn line-selection [[from to] [line-start-offset line-end-offset]]
  (cond (and (< from line-start-offset) (< line-start-offset to))
        (if (< line-end-offset to)
          [0 :infinity]
          [0 (- to line-start-offset)])
        (and (<= line-start-offset from) (<= from line-end-offset))
        [(- from line-start-offset) (if (< to line-end-offset)
                                      (- to line-start-offset)
                                      :infinity)]
        :else nil))

(defn editor-viewport [state]
  (fn [pos size]
    (let [dims (reaction
                (let [[_ from-y-offset] @pos
                      [w h] @size
                      from-idx (int (/ from-y-offset line-h))]
                  {:from-idx from-idx
                   :to-idx (+ 5 (+ from-idx (/ h line-h)))
                   :y-shift (- (* line-h (- (/ from-y-offset line-h) from-idx)))}))]
      (fn []
        (let [from (:from-idx @dims)
              to (:to-idx @dims)
              {:keys [text caret selection lines] :as state} @state
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
                                      line-caret (when (and (<= line-start-offset caret) (<= caret line-end-offset))
                                                   (- caret line-start-offset))
                                      line-tokens (:tokens (get lines index))
                                      line-info (LineInfo. line-text line-tokens line-sel line-caret index)]
                                  [next-line (conj! res
                                                    ^{:key index}
                                                    [render-line line-info metrics])]))
                              [(text/scan-to-line (text/zipper text) from)
                               (transient [:div {:style
                                                 {:background theme/background
                                                  :width "100%"
                                                  :transform (str "translate3d(0px, " (:y-shift @dims) "px, 0px)")}}])]
                              (range from to))]
          (persistent! hiccup))))))

(defn edit-at [{:keys [text] :as state} offset f]
  (let [edit-point (-> (text/zipper text)
                       (text/scan-to-offset offset))]
    (-> state
        (assoc :text (-> edit-point
                         (f)
                         (text/root)))
        (update :timestamp inc)
        (update :first-invalid min (text/line edit-point)))))

(defn type-in [{:keys [caret text] :as state} s]
  (-> state
      (edit-at caret #(text/insert % s))
      (update :caret + (count s))))

(defn editor [state]
  (let [size (reagent/atom [2000 30000])
        dom-input (atom nil)
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
       [scroll size
        (editor-viewport state)]
       [:textarea
        {:ref (fn [this]
                (when-let [dom-node (reagent/dom-node this)]
                  (.addEventListener dom-node "focus" (fn [] (js/console.log "focus input")))
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
                       (swap! state type-in val)))}]])))

(defn main []
  [:div {:style {:display :flex
                 :flex "1"}}
   [editor state]])

(defn include-script [src cb]
  (let [e (js/document.createElement "script")]
    (aset e "onload" cb)
    (doto e
          (.setAttribute "type" "text/javascript")
          (.setAttribute "src" src))
    (.appendChild (head) e)))

(defn include-style [src cb]
  (let [e (js/document.createElement "link")]
    (aset e "onload" cb)
    (doto e
      (.setAttribute "type" "text/css")
      (.setAttribute "rel" "stylesheet")
      (.setAttribute "href" src))
    (.appendChild (head) e)))

(defn set-text [state text]
  (assoc state :text (text/make-text text)))

(defonce editor-impl (atom nil))

(defonce modification-watcher
  (do (add-watch state :lexer
                 (fn [_ _ {old-ts :timestamp} {new-ts :timestamp
                                              broker :lexer-broker :as s}]
                   (when (not= old-ts new-ts)
                     (a/put! broker s))))
      true))

(defn deliver-lexems! [{:keys [req-ts tokens index]}]
  (swap! state
         (fn [{:keys [timestamp] :as state}]
           (if (= timestamp req-ts)
             (-> state
                 (assoc-in [:lines index :tokens] tokens)
                 (assoc :first-invalid (inc index)))
             state)))
  (= (:timestamp @state) req-ts))

(defn attach-lexer! [{:keys [modespec lexer-broker]}]
  (let [{:keys [input output]} (lexer/new-lexer-worker modespec)]
    (go
      (loop [state nil
             line 0
             start-time 0]
        (let [elapsed (- (.getTime (js/Date.)) start-time)
              next-text (when (< line (text/lines-count (:text state)))
                          (some-> state :text (text/line-text line)))
              [val port] (a/alts! (cond-> [lexer-broker output]
                                    (some? next-text) (conj [input {:index line
                                                                    :text next-text
                                                                    :req-ts (:timestamp state)}]))
                                  :priority true)]
          (let [start-time' (if (< 10 elapsed)
                              (do (a/<! (a/timeout 1))
                                  (.getTime (js/Date.)))
                              start-time)]
            (cond
              (= port lexer-broker) (recur val (:first-invalid val) start-time')
              (= port output) (let [delivered?  (deliver-lexems! val)]
                                (recur state (if delivered? (inc line) line) start-time'))
              (= port input) (recur state line start-time'))))))))

(defonce *codemirror-state (atom :initial))

(defn with-codemirror [cb]
  (if (= @*codemirror-state :ready)
    (cb)
    (do
      (if (= @*codemirror-state :scheduled)
        nil
        (do
          (reset! *codemirror-state :scheduled)
          (include-script "/codemirror/addon/runmode/runmode-standalone.js"
                          (fn []
                            (include-script "/codemirror/mode/javascript/javascript.js"
                                            (fn [] (js/console.log "js load")))
                            (include-script "/codemirror/mode/clike/clike.js"
                                            (fn [] (js/console.log "clike load")))
                            (include-script "/codemirror/mode/clojure/clojure.js"
                                            (fn [] (js/console.log "clojure load")))
                            (cb))))))))

(defn load-text [cb]
  (go
    (let [text (:body (a/<! (http/get "/EditorImpl.java")))]
      (reset! editor-impl text)
      (with-codemirror (fn []
                         (reset! *codemirror-state :ready)
                         (swap! state set-text text)
                         (attach-lexer! @state)
                         (cb))))))

(defn mount-root []
  (load-text (fn []
               (let [root (.getElementById js/document "app")]
                 (reagent/render [main] root)))))

(defn init! []
  (mount-root))

(defn capture [f]
  (fn [evt _]
    (f)
    (.stopPropagation evt)
    (.preventDefault evt)))

(defn- bind-function! [key f & args]
  (keybind/bind! key :global (capture #(swap! state (fn [s] (apply f s args))))))

(defn move-caret [{:keys [caret] :as state} dir]
  (let [caret' (case dir
                 :left (if (= caret 0)
                         (caret)
                         (dec caret))
                 :right (inc caret))]
    (assoc state :caret caret')))

(defn right [state]
  (js/console.log (get state :caret))
  (move-caret state :right))

(defn left [state]
  (js/console.log (get state :caret))
  (move-caret state :left))

(bind-function! "left" left)
(bind-function! "right" right)

(defn backspace [{:keys [text caret timestamp] :as state}]
  (if (< 0 caret)
    (-> state
        (edit-at (dec caret) #(text/delete % 1))
        (assoc :caret (dec caret)))
    state))

(defn delete [{:keys [text caret timestamp] :as state}]
  (if (< caret (text/text-length text))
    (-> state
        (edit-at caret #(text/delete % 1)))
    state))

(bind-function! "backspace" backspace)
(bind-function! "delete" delete)
