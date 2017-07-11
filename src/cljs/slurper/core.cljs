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
    {:width (.-width (.measureText ctx s)) :height line-h}))

(defn make-editor-state []
  (let [ch (a/chan)]
    {:text (text/make-text "")
     :selection [49 4956]
     :caret {:offset 0 :v-col 0}
     :lexer-broker ch
     :modespec "text/x-java"
     :timestamp 0
     :lines []
     :first-invalid 0}))

(defn make-editor-viewport []
  {:pos [0 0]
   :view-size [0 0]
   :once true})

(defn px [x]
  (str x "px"))

(defonce state (reagent/atom (make-editor-state)))

(defonce viewport (reagent/atom (make-editor-viewport)))

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
        [(- from line-start-offset) (if (<= to line-end-offset)
                                      (- to line-start-offset)
                                      :infinity)]
        :else nil))

(defn absolute->line-ch [client-x client-y {:keys [:height :width]} from to y-shift]
  (let [x client-x
        y (- (- client-y y-shift) (/ height 2))]
    [(+ from (Math/round (/ y height))) (Math/round (/ x width))]))

(defn set-caret [{:keys [caret selection text] :as state} line col selection?]
  (let [[sel-from sel-to] selection
        {caret-offset :offset} caret
        line-loc (text/scan-to-line (text/zipper text) line)
        line-len (text/line-length line-loc)
        line-off (text/offset line-loc)
        caret-offset' (+ line-off (min col line-len))]
    (-> state
        (assoc :caret {:offset caret-offset' :v-col 0})
        (assoc :selection (cond (not selection?) [caret-offset' caret-offset']
                                (= caret-offset sel-from) [(min caret-offset' sel-to) (max caret-offset' sel-to)]
                                (= caret-offset sel-to) [(min sel-from caret-offset') (max sel-from caret-offset')]
                                :else [(min caret-offset caret-offset') (max caret-offset' caret-offset')])))))


(defn on-mouse-action! [[line col] selection?]
  (swap! state #(set-caret % line col selection?)))

(defn edit-at [{:keys [text] :as state} offset f]
  (let [edit-point (-> (text/zipper text)
                       (text/scan-to-offset offset))]
    (-> state
        (assoc :text (-> edit-point
                         (f)
                         (text/root)))
        (update :timestamp inc)
        (update :first-invalid min (text/line edit-point)))))

(defn delete-under-selection [state [sel-from sel-to] sel-len]
  (-> state
      (edit-at sel-from #(text/delete % sel-len))
      (assoc-in [:caret :offset] sel-from)
      (assoc-in [:caret :v-col] 0)
      (assoc :selection [sel-from sel-from])))

(defn type-in [{:keys [selection] :as state} s]
  (let [[sel-from sel-to] selection
        sel-len (- sel-to sel-from)
        state (if (< 0 sel-len)
                (delete-under-selection state selection sel-len)
                state)
        caret-offset (get-in state [:caret :offset])]
    (-> state
        (edit-at caret-offset #(text/insert % s))
        (update-in [:caret :offset] + (count s))
        (assoc :selection [(+ caret-offset (count s)) (+ caret-offset (count s))]))))

(defn scroll [viewport-fn]
  (fn []
    [:div {:style {:display :flex
                   :flex "1"
                   :overflow :hidden}
           :ref (fn [e]
                  (when e
                    (swap! viewport #(assoc % :view-size [(.-clientWidth e) (.-clientHeight e)])) 100)
                  (when (and (:once @viewport) (some? e))
                    (swap! viewport #(assoc % :once false))
                    (.addEventListener
                     e
                     "mousewheel"
                     (fn [evt]
                       (swap! viewport
                              #(update % :pos
                                      (fn [[x y]]
                                        (let [dx (/ (.-wheelDeltaX evt) 2)
                                              dy (/ (.-wheelDeltaY evt) 2)]
                                          (if (< (js/Math.abs dx) (js/Math.abs dy))
                                            [x (max 0 (- y dy))]
                                            [(max 0 (- x dx)) y])))))
                       (.preventDefault evt)))))}
     [viewport-fn (reagent/cursor viewport [:pos]) (reagent/cursor viewport [:view-size])]]))

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
              caret-offset (:offset caret)
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
                                                  :transform (str "translate3d(0px, " (:y-shift @dims) "px, 0px)")}
                                                 :onMouseDown (fn [event]
                                                                (let [x ($ event :clientX)
                                                                      y ($ event :clientY)]
                                                                  (on-mouse-action! (absolute->line-ch x y metrics from to (:y-shift @dims))
                                                                                    false)))
                                                 :onMouseMove  (fn [event]
                                                                 (when (= ($ event :buttons) 1)
                                                                   (let [x ($ event :clientX)
                                                                         y ($ event :clientY)]
                                                                     (on-mouse-action! (absolute->line-ch x y metrics from to (:y-shift @dims))
                                                                                       true))))}])]
                              (range from to))]
               (persistent! hiccup))))))

(defn editor [state]
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
       [scroll (editor-viewport state)]
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
                       (swap! state type-in val)))}]])))

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

(defn set-text [state text]
  (-> state
      (assoc :text (text/make-text text)
             :first-invalid 0)
      (update :timestamp inc)))

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

(defn wait-for-all [cs]
  (let [m (a/merge cs)]
    (go (dotimes [i (count cs)] (a/<! m)))))

(defn load! []
  (js/window.addEventListener "keydown" (keybind/dispatcher) true)
  (let [loaded (a/promise-chan)]
    (go
      ;load CodeMirror first
      (wait-for-all (map include-script ["/codemirror/addon/runmode/runmode-standalone.js"
                                         "/codemirror/addon/runmode/runmode-standalone.js"
                                         "/codemirror/mode/javascript/javascript.js"
                                         "/codemirror/mode/clike/clike.js"
                                         "/codemirror/mode/clojure/clojure.js"]))
      ;run lexer worker and setup atom watcher that will run lexer on changes
      (attach-lexer! @state)
      (add-watch state :lexer
                 (fn [_ _ {old-ts :timestamp} {new-ts :timestamp
                                              broker :lexer-broker :as s}]
                   (when (not= old-ts new-ts)
                     (a/put! broker s))))
      ;load sample document from the internet
      (let [text (:body (a/<! (http/get "/EditorImpl.java")))]
        (swap! state set-text text))
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
  (keybind/bind! key :global (capture #(swap! state (fn [s] (apply f s args))))))

(defn move-caret [{:keys [caret text selection] :as state} dir selection?]
  (let [{caret-offset :offset v-col :v-col} caret
        [sel-from sel-to] selection
        {caret-offset' :offset :as caret'} (case dir
                 :left (if (< 0 caret-offset)
                         {:offset (dec caret-offset) :v-col 0}
                         caret)
                 :right (if (< caret-offset (text/text-length text))
                          {:offset (inc caret-offset) :v-col 0}
                          caret)
                 :up (let [cur-loc (text/scan-to-offset (text/zipper text) caret-offset)
                           cur-line (text/line cur-loc)
                           cur-begin (text/offset (text/scan-to-line (text/zipper text) cur-line))
                           cur-col (- caret-offset cur-begin)
                           prev-end (dec cur-begin)
                           prev-line (text/line (text/scan-to-offset (text/zipper text) prev-end))
                           prev-begin (text/offset (text/scan-to-line (text/zipper text) prev-line))
                           new-v-col (max v-col cur-col)]
                       (if (< 0 cur-line)
                         {:offset (min prev-end (+ prev-begin new-v-col)) :v-col new-v-col}
                         caret))
                 :down (let [zipper (text/zipper text)
                             cur-loc (text/scan-to-offset (text/zipper text) caret-offset)
                             cur-line (text/line cur-loc)
                             cur-begin (text/offset (text/scan-to-line zipper cur-line))
                             cur-col (- caret-offset cur-begin)
                             next-line-loc (text/scan-to-line zipper (inc cur-line))
                             next-begin (text/offset next-line-loc)
                             next-end (+ next-begin (text/line-length next-line-loc))
                             new-v-col (max v-col cur-col)]
                         (if (tree/end? next-line-loc)
                           caret
                           {:offset (min next-end (+ next-begin new-v-col)) :v-col new-v-col})))
        selection' (cond
                     (not selection?) [caret-offset' caret-offset']
                     (= caret-offset sel-from) [(min caret-offset' sel-to) (max caret-offset' sel-to)]
                     (= caret-offset sel-to) [(min sel-from caret-offset') (max sel-from caret-offset')]
                     :else [(min caret-offset caret-offset') (max caret-offset' caret-offset')])]
    (-> state
        (assoc :caret caret')
        (assoc :selection selection'))))

(defn right [state]
  (move-caret state :right false))

(defn left [state]
  (move-caret state :left false))

(defn up [state]
  (move-caret state :up false))

(defn down [state]
  (move-caret state :down false))

(defn shift-right [state]
  (move-caret state :right true))

(defn shift-left [state]
  (move-caret state :left true))

(defn shift-up [state]
  (move-caret state :up true))

(defn shift-down [state]
  (move-caret state :down true))

(bind-function! "left" left)
(bind-function! "right" right)
(bind-function! "up" up)
(bind-function! "down" down)
(bind-function! "shift-left" shift-left)
(bind-function! "shift-right" shift-right)
(bind-function! "shift-up" shift-up)
(bind-function! "shift-down" shift-down)

(defn backspace [{:keys [text caret selection] :as state}]
  (let [{caret-offset :offset} caret
        [sel-from sel-to] selection
        sel-len (- sel-to sel-from)]
    (cond (< 0 sel-len)
          (delete-under-selection state selection sel-len)

          (< 0 caret-offset)
          (-> state
              (edit-at (dec caret-offset) #(text/delete % 1))
              (assoc-in [:caret :offset] (dec caret-offset))
              (assoc-in [:caret :v-col] 0)
              (assoc :selection [(dec caret-offset) (dec caret-offset)]))

          :else state)))

(defn delete [{:keys [text caret selection] :as state}]
  (let [{caret-offset :offset} caret
        [sel-from sel-to] selection
        sel-len (- sel-to sel-from)]
    (cond (< 0 sel-len)
          (delete-under-selection state selection sel-len)

          (< caret-offset (text/text-length text))
          (edit-at state caret-offset #(text/delete % 1))

          :else state)))

(bind-function! "backspace" backspace)
(bind-function! "delete" delete)

(defn pg-move! [{:keys [caret] :as state} dir selection?]
  (let [{:keys [pos view-size]} @viewport
        [_ view-size] view-size
        [_ view-init-pos] pos
        view-new-pos (case dir
                       :up (max 0 (- view-init-pos view-size))
                       :down (min 1000000000 (+ view-init-pos view-size))) ;; fix-me
        view-new-col (int (/ view-new-pos line-h))
        caret-pos (case dir
                    :up 0
                    :down (/ view-size line-h))
        [text-line text-col] (absolute->line-ch 0 view-new-pos metrics caret-pos 0 0)]
    (swap! viewport #(assoc % :pos [0 view-new-pos]))
    (set-caret state text-line 0 selection?)))

(bind-function! "pgup" pg-move! :up false)
(bind-function! "pgdown" pg-move! :down false)
(bind-function! "shift-pgup" pg-move! :up true)
(bind-function! "shift-pgdown" pg-move! :down true)
