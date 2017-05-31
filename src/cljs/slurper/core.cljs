(ns slurper.core
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [slurper.keybind :as keybind]
              [garden.core :as g])
    (:require-macros [reagent.interop :refer [$ $!]]))

;; -------------------------
;; Views

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
  [:pre {:font-family "Fira Code, monospace"}])

(defn measure [s]
  (let [elt (js/document.createElement "div")]
    (.setAttribute elt "style" "font-family : Fira Code;")
    (set! (.-innerHTML elt) (str "<span>" s "</span>"))
    (.appendChild js/document.body elt)
    (let [rect (-> elt
                   (.-children)
                   (aget 0)
                   (.getBoundingClientRect))
          result {:width ($ rect :width)
                  :height ($ rect :height)}]
      (.remove elt)
      result)))

(defn make-editor-state []
  {:lines (vec (take 500 (repeat "hello world")))
   :caret [0 0]
   :font {:font-family "Fira Code"}})

(defn px [x]
  (str x "px"))

(defonce state (reagent/atom (make-editor-state)))

(defonce on-keydown (keybind/dispatcher))

(defonce keys-dispatcher (js/window.addEventListener "keydown" on-keydown true))

(defn type-in [{[line col] :caret :as state} val]
  (-> state
   (update-in [:lines line]
              (fn [s]
                (str (subs s 0 col) val (subs s col))))
   (update :caret (fn [[line col]] [line (inc col)]))))

(defn editor [state]
  (let [{line-height :height
         ch-width :width} (measure "X")
        dom-input (atom nil)
        listener (atom false)]
    [:div {:style {:display :flex
                   :flex 1}}
     [:textarea
      {:ref (fn [this]
              (when-let [dom-node (reagent/dom-node this)]
                (.addEventListener dom-node "focus" (fn [] (js/console.log "focus input")))
                (reset! dom-input dom-node)))
       :auto-focus true
       :style {:opacity 0
               :height "0px"
               :width "0px"}
       :on-input (fn [evt]
                   (let [e (.-target evt)
                         val (.-value e)]
                     (set! (.-value e) "")
                     (swap! state type-in val)))}]
     [:> js/ReactVirtualized.AutoSizer
      (fn [m]
        (reagent/as-element
         [:> js/ReactVirtualized.List
          
          {:ref (fn [this] 
                  (when-not @listener
                    (when-let [node (reagent/dom-node this)]
                      (reset! listener true)
                      (.addEventListener node "focus"
                                          (fn []
                                              (when @dom-input
                                                (.focus @dom-input)))))))
           :height ($ m :height)
           :width ($ m :width)
           :font-family (:font-family (:font @state))
           :rowCount (count (:lines @state))
           :rowHeight (inc line-height)
           :rowRenderer (fn [s]
                          (let [;{:keys [index style isVisible isScrolling]} (js->clj s)
                                index ($ s :index)
                                style ($ s :style)]
                            (reagent/as-element
                             ^{:key index}
                             [(fn []
                                (let [[line col] (:caret @state)]
                                  [:pre {:style style}
                                   (when (= index line)
                                     [:div {:style {:width "1px"
                                                    :background-color "red"
                                                    :position :absolute
                                                    :left (px (* col ch-width))
                                                    :height (px line-height)}}])
                                   (nth (:lines @state) index)]))])))
           :noRowsRenderer (fn []
                             (reagent/as-element [:div "hello empty"]))}]))]]))

(defn right []
  (swap! state update-in [:caret 1] inc))

(defn left []
  (swap! state update-in [:caret 1] dec))

(defn up []
  (swap! state update-in [:caret 0] dec))

(defn down []
  (swap! state update-in [:caret 0] inc))

(defn main []
  [:div {:style {:display :flex
                 :flex "1"}}
   [editor state]])

(defn include-script [src cb]
  (let [e (js/document.createElement "script")]
    (doto e
      (.setAttribute "type" "text/javascript")
      (.setAttribute "src" src))
    (aset e "onload" cb)
    (.appendChild (head) e)))

(defn include-style [src cb]
  (let [e (js/document.createElement "link")]
    (doto e
      (.setAttribute "type" "text/css")
      (.setAttribute "rel" "stylesheet")
      (.setAttribute "href" src))
    (aset e "onload" cb)
    (.appendChild (head) e)))

(defonce *virtualized-state (atom :initial))

(defn with-virtualized [cb]
  (if (= @*virtualized-state :ready)
    (cb)
    (do
      (if (= @*virtualized-state :scheduled)
        nil
        (do
          (reset! *virtualized-state :scheduled)
          (include-script "/react-virtualized.js"
                          (fn []
                            (include-style "/firacode/fira_code.css"
                                           (fn []
                                             (measure "X")
                                             (js/setTimeout
                                              (fn []
                                                (reset! *virtualized-state :ready)
                                                (cb))
                                              0))))))))))

(defn mount-root []
  (with-virtualized 
    #(reagent/render [main] (.getElementById js/document "app"))))

(defn init! []
  (mount-root))

(defn capture [f]
  (fn [evt _]
    (f)
    (.stopPropagation evt)
    (.preventDefault evt)))

(keybind/bind! "left" :global (capture left))
(keybind/bind! "right" :global (capture right))
(keybind/bind! "up" :global (capture up))
(keybind/bind! "down" :global (capture down))
