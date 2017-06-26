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
              [slurper.text :as text])
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
    (set! (.-font ctx) "16px Fira Code")
    (let [res {:width (.-width (.measureText ctx s)) :height 18}]
      (js/console.log (:width res))
      res)))

(defn make-editor-state []
  (let [ch (a/chan)]
    {:text (text/make-text "")}))

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

(def line-h 17.78)

(defn lines-viewport [line]
  (fn [pos size]
    (let [dims (reaction
                (let [[_ from-y-offset] @pos
                      [w h] @size
                      from-idx (int (/ from-y-offset line-h))]
                  {:from-idx from-idx
                   :to-idx (+ from-idx (/ h line-h))
                   :y-shift (- (* line-h (- (/ from-y-offset line-h) from-idx)))}))]
      (fn []
        (into
         [:div {:style
                {:transform (str "translate3d(0px, " (:y-shift @dims) "px, 0px)")}}]
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

(defn line [state index]
  (let [s (text/line-text (:text @state) index)]
    [:pre {:style {:height line-h}} s]))

(defn editor [state]
  [scroll (reagent/atom [2000 30000])
   (lines-viewport
    (fn [index]
      (with-meta 
        [line state index]
        {:key index})))])

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


