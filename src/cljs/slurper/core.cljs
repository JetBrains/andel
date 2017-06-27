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
              [slurper.tree :as tree])
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

(defstyle :editor
  [:pre {:font-family "Menlo, monospace"
         :color theme/foreground
         :margin "0px"}])

(defn measure [s]
  (let [canvas (js/document.createElement "canvas")
        ctx (.getContext canvas "2d")]
    (set! (.-font ctx) "Menlo")
    (let [res {:width (.-width (.measureText ctx s)) :height 18}]
      (js/console.log (:width res))
      res)))

(defn make-editor-state []
  (let [ch (a/chan)]
    {:text (text/make-text "")
     :selection [49 4956]
     :caret 49}))

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

(defn html [[tag & rest :as el]]
  (when el
    (if (string? el) (goog.string/htmlEscape el)
        (let [[attrs? & children :as rest] (if (string? rest) nil rest)
              html-tag (str "<" (name tag) " "
                            (when (map? attrs?)
                              (render-attrs attrs?)) ">")
              children (map html (if (map? attrs?) children rest))
              closing-tag (str "</" (name tag) ">")]
          (apply str html-tag (concat children [closing-tag]))))))

(def line-h 19)

(defn lines-viewport [line]
  (fn [pos size]
    (let [dims (reaction
                (let [[_ from-y-offset] @pos
                      [w h] @size
                      from-idx (int (/ from-y-offset line-h))]
                  {:from-idx from-idx
                   :to-idx (+ 5 (+ from-idx (/ h line-h)))
                   :y-shift (- (* line-h (- (/ from-y-offset line-h) from-idx)))}))]
      (fn []
        (into
         [:div {:style
                {:background theme/background
                 :transform (str "translate3d(0px, " (:y-shift @dims) "px, 0px)")}}]
         (map (fn [i]
                (with-meta 
                  [line i]
                  {:key i}))
              (range (:from-idx @dims) (:to-idx @dims))))))))

(defn scroll [size viewport]
  (let [pos (reagent/atom [0 0])
        view-size (reagent/atom [0 0])
        once (atom true)]
    (fn []
      [:div {:style {:width "100%"
                     :height "100%"
                     :overflow :hidden}
             :ref (fn [e]
                    (when e
                      (reset! view-size [(.-clientWidth e)
                                         (.-clientHeight e)]))
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

(comment (measure "x"))

(defn compare-offsets [x y]
  (cond (= x y) 0
        (= x :infinity) 1
        (= y :infinity) -1
        (< x y) -1
        (< y x) 1))

(defn subtract-offsets [x y]
  (assert (not= y :infinity))
  (if (= x :infinity)
    :infinity
    (- x y)))

(defn add-offsets [x y]
  (if (or (= x :infinity) (= y :infinity))
    :infinity
    (+ x y)))

(defn render-selection [[from to] {:keys [width height]}]
  [:div
   {:style
    (style (merge {:background-color theme/selection
                   :height (px height)
                   :position :absolute
                   :top (px 0)}
                  (if (= to :infinity)
                    {:left 0
                     :margin-left (px (* from width))
                     :width "100%"}
                    {:left (px (* from width))
                     :width (px (* (- to from) width))})))}])

(def metrics {:width 10 :height line-h})

(defn render-caret [col {:keys [width height]}]
  [:div {:style (style {:width "1px"
                        :animation "blinker 1s cubic-bezier(0.68, -0.55, 0.27, 1.55) infinite"
                        :top 0
                        :background-color "red"
                        :position :absolute
                        :left (px (* col width))
                        :height (px (inc height))})}])

(defn render-line [line-text selection caret-index {:keys [height]}]
  [:div
   {:dangerouslySetInnerHTML
    {:__html
     (html
      [:div
       {:style (style {:height (px height)
                       :position :relative})}
       (render-selection selection metrics)
       [:pre {:style (style {:position :absolute
                             :top (px 0) 
                             :height (px height)})} line-text]
       (when caret-index (render-caret caret-index metrics))])}}])

(defn line-selection [[from to] [line-start-offset line-end-offset]]
  (cond (< from line-start-offset to)
        (if (< line-end-offset to)
          [0 :infinity]
          [0 (- to line-start-offset)])
        (<= line-start-offset from line-end-offset)
        [(- from line-start-offset) (if (< to line-end-offset)
                                      (- to line-start-offset)
                                      :infinity)]
        :else nil))

(defn line [{:keys [text caret selection]} index]
  (let [line-start (text/scan-to-line (text/zipper text) index)
        next-line (text/scan-to-line line-start (inc index))
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
        line-caret (when (<= line-start-offset caret line-end-offset)
                     (- caret line-start-offset))]
    [render-line line-text line-sel line-caret metrics]))

(defn type-in [{:keys [caret text] :as state} s]
  (assoc state
         :text (-> (text/zipper text)
                   (text/scan-to-offset caret)
                   (text/insert s)
                   (text/root))
         :caret (+ caret (count s))))

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
                                             (.focus @dom-input)))))
                    )}
       [scroll size 
        (lines-viewport
         (fn [index]
           (with-meta 
             [line @state index]
             {:key index})))]
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

(defn load-text [cb]
  (go
    (let [text (:body (a/<! (http/get "/EditorImpl.java")))]
      (reset! editor-impl text)
      (swap! state set-text text)
      (cb))))

(defonce *codemirror-state (atom :initial))

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


