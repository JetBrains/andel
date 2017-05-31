(ns slurper.core
    (:require [slurper.lexer]
              [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [slurper.keybind :as keybind]
              [garden.core :as g]
              [clojure.core.async :as a]
              [cljs-http.client :as http])
    (:require-macros [reagent.interop :refer [$ $!]]
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
  [:pre {:font-family "Fira Code, monospace"
         :margin "0px"}])

(defn measure [s]
  (let [canvas (js/document.createElement "canvas")
        ctx (.getContext canvas "2d")]
    (set! (.-font ctx) "16px Fira Code")
    (let [res {:width (.-width (.measureText ctx s)) :height 18}]
      (js/console.log (:width res))
      res)))

(defn make-editor-state []
  {:lines []
   :caret [0 0]
   :selection [[0 2] [1 5]]
   :font {:font-family "Fira Code"}})

(defn px [x]
  (str x "px"))

(defonce state (reagent/atom (make-editor-state)))

(defonce on-keydown (keybind/dispatcher))

(defonce keys-dispatcher (js/window.addEventListener "keydown" on-keydown true))

(defn type-in [{[line col] :caret :as state} val]
  (-> state
   (update-in [:lines line :text]
              (fn [s]
                (str (subs s 0 col) val (subs s col))))
   (update :caret (fn [[line col]] [line (inc col)]))))

(defn line-selection [selection line]
  (let [[[from-line from-col] [to-line to-col]] selection]
    (when (<= from-line line to-line)
      [(if (= line from-line) from-col 0)
       (if (= line to-line) to-col :infinity)])))

(defn fragments [s fragments]
  (second
   (reduce (fn [[s res] frag]
             (if (= frag :infinity)
               (reduced [nil (conj res s)])
               [(subs s frag) (conj res (subs s 0 frag))]))
           [s []] fragments)))

(defn line-renderer [state index style metrics]
  (let [{line-height :height
         ch-width :width} metrics
        [line col] (:caret @state)
        text (:text (nth (:lines @state) index))
        [from to :as sel] (line-selection (:selection @state) index)]
    [:div {:style style}
     (when sel
       [:div {:style (merge {:background-color "blue"
                             :height (px line-height)
                             :position :absolute
                             :top 0}
                            (if (= to :infinity)
                              {:left 0
                               :margin-left (px (* from ch-width))
                               :width "100%"}
                              {:left (px (* from ch-width))
                               :width (px (* (- to from) ch-width))}))}])
     (into
      [:pre {:style
             {:position :absolute
              :left 0
              :top 0}}]
      (let [tokens (if (some? sel)
                     (if (= to :infinity)
                       [[:none from] [:selected :infinity]]
                       [[:none from] [:selected (- to from)] [:none :infinity]])
                     [[:none :infinity]])
            frags (fragments text (map second tokens))]
        (map (fn [s ttype]
               [:span {:style (case ttype
                                :none nil
                                :selected {:color :white})}
                s]) frags (map first tokens))))
     (when (= index line)
       [:div {:style {:width "1px"
                      :top 0
                      :background-color "red"
                      :position :absolute
                      :left (px (* col ch-width))
                      :height (px line-height)}}])]))

(defn editor [state]
  (let [{line-height :height
         ch-width :width :as metrics} (measure "X")
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
     [:> (-> js/window ($ :ReactVirtualized) ($ :AutoSizer))
      (fn [m]
        (reagent/as-element
         [:> (-> js/window ($ :ReactVirtualized) ($ :List))

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
           :rowHeight line-height
           :rowRenderer (fn [s]
                          (let [index ($ s :index)
                                style ($ s :style)]
                            (reagent/as-element
                             ^{:key index}
                             [line-renderer state index style metrics])))
           :noRowsRenderer (fn [] (reagent/as-element [:div "hello empty"]))}]))]]))

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
    (aset e "onload" cb)
    (doto e
          (.setAttribute "type" "text/javascript")
          (.setAttribute "src" src))
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

(defn set-text [state text]
  (assoc state :lines (mapv (fn [s] {:text s}) (clojure.string/split-lines text))))

(defn fake-lexems [state]
  (assoc-in state [:lines 3 :tokens] [[1 :ws] [1 :comment] [1 :ws] [8 :kw] [1 :ws] []]))
  

(defn with-virtualized [cb]
  (if (= @*virtualized-state :ready)
    (cb)
    (do
      (if (= @*virtualized-state :scheduled)
        nil
        (do
          (reset! *virtualized-state :scheduled)
          (include-script
           "/react-virtualized.js"
           (fn []
             (include-style
              "/firacode/fira_code.css"
              (fn []
                (measure "X")
                (js/setTimeout
                 (fn []
                   (go
                     (let [text (:body (a/<! (http/get "/EditorImpl.java")))]
                       (reset! *virtualized-state :ready)
                       (swap! state (fn [state] (-> state
                                                   (set-text text)
                                                   (fake-lexems))))
                       (cb)
                       )))
                 100))))))))))

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

(defn mount-root []
  (with-virtualized
   #(with-codemirror
     (fn []
        (reagent/render [main] (.getElementById js/document "app"))))))

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
