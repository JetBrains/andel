(ns slurper.core
    (:require [slurper.lexer :as lexer]
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
   :selection [[3 0] [3 5]]
   :font {:font-family "Fira Code"}})

(defn px [x]
  (str x "px"))

(defonce state (reagent/atom (make-editor-state)))

(defonce on-keydown (keybind/dispatcher))

(defonce keys-dispatcher (js/window.addEventListener "keydown" on-keydown true))

(defn update-line-lexems [{:keys [state text] :as line}]
  (let [{:keys [tokens state]} (lexer/lex "text/x-java" text nil)]
    (assoc line
           :tokens tokens
           :state state)))

(defn update-lexems-upto [state line]
  (update state :lines
          (fn [lines]
            (let [[before after] (split-at (inc line) lines)]
              (into (mapv update-line-lexems before) after)))))

(defn delete-insert [state delete insert]
  (let [[line col] (:caret state)]
    (update-in state [:lines line :text]
               (fn [s]
                 (str (subs s 0 (- col delete)) insert (subs s col))))))

(defn type-in [{[line col] :caret :as state} val]
  (-> state
      (delete-insert 0 val)
      (update :caret (fn [[line col]] [line (inc col)]))
      (update-lexems-upto line)))

(defn backspace-in [{[line col] :caret :as state}]
  (-> state
      (delete-insert 1 "")
      (update :caret (fn [[line col]] [line (dec col)]))
      (update-lexems-upto line)))

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

(def token-styles {:keyword {:color :magenta}
                   :comment {:color :cyan}
                   :ws {}
                   :whatever {:color :yellow}
                   :selected {:color :white
                              :layer 100}})

#_{:range [from to]
 :layer 5
 :style {}}

#_[[1 nil] [5 style]]

#_{:offset 5
   :type :start
   :layer 5
   :style {}}

#_{:type :end}

(defn merge-styles [{l1 :layer :as s1 :or {l1 0}} {l2 :layer :as s2 :or {l2 0}}]
  (merge-with (fn [x y]
                (if (< l1 l2) y x))
              s1 s2))

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

(defn shred [ranges]
  (->> ranges
       (mapcat (fn [{[from to] :range :keys [style] :as r}]
                 [{:offset from
                   :type :start
                   :style style} {:offset to
                                  :type :end
                                  :style style}]))
       (sort-by :offset compare-offsets)
       (reduce
        (fn [{:keys [merged-style style-start styles] :as s} {:keys [type style offset]}]
          (let [styles' (if (= type :start)
                          (conj styles style)
                          (disj styles style))
                style' (if (= type :start)
                         (merge-styles merged-style style)
                         (reduce merge-styles nil styles'))]
            (if (= merged-style style')
              (assoc s :styles styles')
              (-> s
                  (assoc :styles styles'
                         :merged-style style'
                         :style-start offset)
                  (cond-> (not= offset style-start)
                          (update :result conj! [(subtract-offsets offset style-start) merged-style]))))))
        {:style-start 0
         :merged-style nil
         :styles #{}
         :result (transient [])})
       :result
       (persistent!)))

(comment

  (shred [{:range [0 2]
           :style {:color :green}}
          {:range [1 5]
           :style {:color :red}}
          {:range [3 :infinity]
           :style {:color :black}}
          ])
  )

(defn render-selection [[from to] {:keys [width height]}]
  [:div
   {:style
    (merge {:background-color "blue"
            :height (px height)
            :position :absolute
            :top 0}
           (if (= to :infinity)
             {:left 0
              :margin-left (px (* from width))
              :width "100%"}
             {:left (px (* from width))
              :width (px (* (- to from) width))}))}])

(defn render-text [tokens text]
  (into
   [:pre {:style
          {:position :absolute
           :left 0
           :top 0}}]
   (let [frags (fragments text (concat (map first tokens) [:infinity]))]
     (map (fn [s style]
            [:span {:style style} s])
          frags
          (concat (map second tokens) (repeat nil))))))

(defn shred-selection-with-tokens [sel-tokens tokens]
  (let [token-ranges  (first
                       (reduce (fn [[result offset] [len ttype]]
                                 [(conj result
                                        {:range [offset (+ offset len)]
                                         :style (get token-styles ttype)})
                                  (+ offset len)]) [[] 0] tokens))
        sel-ranges (first
                    (reduce (fn [[result offset] [len ttype]]
                              [(conj result
                                     {:range [offset (+ offset len)]
                                      :style (get token-styles ttype)})
                               (+ offset len)]) [[] 0] sel-tokens))]
    (shred (concat token-ranges sel-ranges))))

(defn render-caret [col {:keys [width height]}]
  [:div {:style {:width "1px"
                 :top 0
                 :background-color "red"
                 :position :absolute
                 :left (px (* col width))
                 :height (px height)}}])

(defn line-renderer [state index style metrics]
  (let [[caret-line caret-col] (:caret @state)
        {:keys [text tokens] :as line} (nth (:lines @state) index)
        sel (line-selection (:selection @state) index)]
    [:div {:style style}
     (when sel
       (render-selection sel metrics))
     (let [tokens (let [sel-tokens (when-let [[from to] sel]
                                     [[from nil] [(subtract-offsets to from) :selected]])]
                    (if (and (seq tokens) (some? sel-tokens))
                      (shred-selection-with-tokens sel-tokens tokens)
                      (map (fn [[len ttype]] [len (get token-styles ttype)]) (or tokens sel-tokens [[:infinity nil]]))))]
       (render-text tokens text))
     (when (= index caret-line)       
       (render-caret caret-col metrics))]))

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

(defn- move-caret-by [state [drow dcol]]
  (letfn [(clamp [v hi] (min (max v 0) hi))
          (move [[row col]]
            (let [new-row (clamp (+ row drow) (dec (count (:lines state))))
                  line-len (count (get-in state [:lines new-row :text]))
                  new-col (clamp (+ col dcol) line-len)]
              [new-row new-col]))]
    (update-in state [:caret] move)))

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
  (assoc-in state [:lines 3 :tokens] [[1 :ws] [1 :comment] [1 :ws] [8 :keyword] [1 :ws] [5 :whatever]]))
  

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

(defn- bind-function! [key f]
  (keybind/bind! key :global (capture #(swap! state f))))

(defn- bind-movement! [key amount]
  (bind-function! key #(move-caret-by % amount)))

(bind-movement! "left" [0 -1])
(bind-movement! "home" [0 -10000])

(bind-movement! "right" [0 1])
(bind-movement! "end"   [0 10000])

(bind-movement! "up"   [-1 0])
(bind-movement! "pgup" [-10 0])

(bind-movement! "down"   [1 0])
(bind-movement! "pgdown" [10 0])

(defn backspace [state]
  (type-in state "X"))

(bind-function! "backspace" backspace-in)
