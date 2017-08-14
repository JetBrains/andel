(ns andel.core
    (:require [andel.lexer :as lexer]
              [andel.theme :as theme]
              [andel.throttling :as throttling]
              [andel.controller :as contr]
              [andel.utils :as utils]
              [andel.intervals :as intervals]
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

(defonce state (reagent/atom (make-editor-state)))

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

(defn render-caret [col {:keys [width] :as metrics}]
  #js [:div {:style (style {:width "1px"
                            :animation "blinker 1s cubic-bezier(0.68, -0.55, 0.27, 1.55) infinite"
                            :top 0
                            :background-color "red"
                            :position :absolute
                            :left (px (* col width))
                            :height (px (inc (utils/line-height metrics)))})}])

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

(defn render-markup [markup {:keys [height width spacing]}]
  (let [res (reduce (fn [res {:keys [from to]}]
                      (push! res #js [:div {:style (style {:background-color "red"
                                                           :left (px (* from width))
                                                           :width (px (* width (- to from)))
                                                           :height (px (+ height spacing))
                                                           :position :absolute})}]))
                    #js [:pre {:class :line-markup}]
                    markup)]
    res))

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

(defn translate3d [{:keys [x y z] :or {x 0 y 0 z 0}} c]
  [:div
   {:style {:transform (str "translate3d(" (px x) ", " (px y) ", " (px z))}}
   c])

(defrecord LineInfo [line-text line-tokens line-markup selection caret-index index])

(defn render-line [{:keys [line-text line-tokens line-markup selection caret-index] :as line-info} metrics]
  (let [_ (defstyle :render-line [:.render-line {:height (px (utils/line-height metrics))
                                                 :position :relative}])]
    [real-dom (dom
                #js [:div {:class :render-line}
                     (render-selection selection metrics)
                     (render-text line-text line-tokens metrics)
                     (when caret-index (render-caret caret-index metrics))
                     (render-markup line-markup metrics)
                     ])]))

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

(defn init-viewport [state]
  (fn [width height]
    (swap-editor! state #(assoc-in % [:viewport :view-size] [width height]))))

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
                        (let [dx (/ (.-wheelDeltaX evt) 2)
                              dy (/ (.-wheelDeltaY evt) 2)]
                          (if (< (js/Math.abs dx) (js/Math.abs dy))
                            [x (min document-height (max 0 (- y dy)))]
                            [(max 0 (- x dx)) y])))))
      (.preventDefault evt))))


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

(defn prepare-markup [markup from to]
  (->> markup
      (filter (fn [marker]
                (and (<= (:from marker) to)
                     (<= from (:to marker)))))
      (mapv (fn [marker]
              (intervals/->Marker (max 0 (- (:from marker) from))
                                  (max 0 (- (:to marker) from))
                                  nil
                                  nil)))))

;; Todo: untangle all this spaghetti bindings
(defn editor-viewport [state]
  (fn []
    (let [{:keys [editor document viewport]} @state
          {:keys [pos view-size metrics]} viewport
          line-height (utils/line-height metrics)
          {:keys [text lines]} document
          {:keys [caret selection]} editor
          [_ from-y-offset] pos
          [w h] view-size
          from (int (/ from-y-offset line-height))
          to (+ 5 (+ from (/ h line-height)))
          y-shift (- (* line-height (- (/ from-y-offset line-height) from)))
          line-zipper (text/scan-to-line (text/zipper text) from)
          from-offset (text/offset line-zipper)
          to-offset (dec (text/offset (text/scan-to-line line-zipper (inc to))))
          caret-offset (get caret :offset)
          markup (intervals/query-intervals (:markup document) (intervals/map->Marker {:from from-offset :to to-offset}))
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
                              line-markup (prepare-markup markup line-start-offset line-end-offset)
                              line-info (LineInfo. line-text line-tokens line-markup line-sel line-caret index)]
                          [next-line (conj! res
                                            ^{:key index}
                                            [translate3d {:y y-shift} [render-line line-info metrics]])]))
                      [(text/scan-to-line (text/zipper text) from)
                       (transient [:div {:style
                                         {:background theme/background
                                          :width "100%"}
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
                                                             (swap-editor! state #(contr/set-caret-at-grid-pos % line-col true)))))}])]
                      (range from to))]
      (persistent! hiccup))))

(defn editor [state]
  (let [dom-input (atom nil)
        listener  (atom nil)]
    (fn []
      [:div
       {:style     {:display :flex
                    :flex    1}
        :tab-index -1
        :ref       (fn [this]
                     (when-let [node (reagent/dom-node this)]
                       (reset! listener true)
                       (.addEventListener node "focus"
                                          (fn []
                                            (when @dom-input
                                              (.focus @dom-input))))))}
       [scroll (editor-viewport state) (init-viewport state) (scroll-on-event state)]
       [:textarea
        {:ref        (fn [this]
                       (when-let [dom-node (reagent/dom-node this)]
                         (reset! dom-input dom-node)))
         :auto-focus true
         :style      {:opacity 0
                      :pading  "0px"
                      :border  :none
                      :height  "0px"
                      :width   "0px"}
         :on-input   (fn [evt]
                       (let [e   (.-target evt)
                             val (.-value e)]
                         (set! (.-value e) "")
                         (swap-editor! state contr/type-in val)))}]])))

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
      (let [text (:body (a/<! (http/get "/EditorImpl.java")))]
        (swap-editor! state contr/set-text text))
      (let [markup (->> (:body (a/<! (http/get "/markup.edn")))
                        (sort-by :from))]
        (js/console.log (str "MARKUP LOADED: " (count markup)))
        (swap-editor! state (fn [s] (assoc-in s [:raw-markers] markup)))
        (swap-editor! state (fn [s] (assoc-in s [:document :markup] (-> (intervals/make-interval-tree)
                                                                        (intervals/add-intervals markup))))))
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
(bind-function! "esc" contr/drop-selection-on-esc)


;; benchmarks

(defn current-time! []
  (.now js/Date))

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
         :count 100))

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
               (intervals/query-intervals itree (intervals/map->Marker {:from from :to to}))))
           :count 10000)))

(defn play-query [model {:keys [from to]}]
  (vec (filter #(intervals/intersect % (intervals/map->Marker {:from from :to to})) model)))

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
           :count 1000)))



(defn bench-editing [markup]
  (let [itree (-> (intervals/make-interval-tree)
                  (intervals/add-intervals markup))]
    (bench "TREE EDITING"
           (fn []
             (let [cmd (rand-nth [:insert :delete])]
               (case cmd
                 :insert ))))))

(bind-function! "ctrl-b" (fn [s]
                           (let [markup (:raw-markers s)]
                             #_(bench-insert markup)
                             #_(bench-insert-base markup)
                             (bench-query markup)
                             #_(bench-query-base markup)
                             #_(bench-type-in markup)
                             #_(bench-delete markup))
                           (js/alert "BENCH DONE")
                           s))
