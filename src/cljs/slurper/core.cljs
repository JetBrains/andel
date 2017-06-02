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
              [hiccups.runtime :as hiccups])
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
  [:pre {:font-family "Fira Code, monospace"
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
    {:lines []
     :caret [0 0]
     :selection [[3 0] [3 5]]
     :font {:font-family "Fira Code"}
     :lexer-broker ch
     :first-invalid 0
     :next-id 0
     :modespec "text/x-java"
     :timestamp 0}))

(defn px [x]
  (str x "px"))

(defonce state (reagent/atom (make-editor-state)))

(defonce on-keydown (keybind/dispatcher))

(defonce keys-dispatcher (js/window.addEventListener "keydown" on-keydown true))

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
              next-text (some-> state :lines (get line) :text)
              
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
              (= port output) (do
                           
                                (let [delivered?  (deliver-lexems! val)]
                                  (recur state (if delivered? (inc line) line) start-time')))
              (= port input) (recur state line start-time'))))))
    ))

(comment

  (:timestamp @state)
  )

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


(defn invalidate-lines [state line]
  (-> state
      (update :first-invalid min line)
      (update :timestamp inc)))

(defonce modification-watcher
  (do (add-watch state :lexer
                 (fn [_ _ {old-ts :timestamp} {new-ts :timestamp
                                              broker :lexer-broker :as s}]                   
                   (when (not= old-ts new-ts)
                     (a/put! broker s))))
      true))

(defn delete-insert [state delete insert]
  (let [[line col] (:caret state)]
    (update-in state [:lines line :text]
               (fn [s]
                 (str (subs s 0 (- col delete)) insert (subs s col))))))

(defn type-in [{[line col] :caret :as state} val]
  (-> state
      (delete-insert 0 val)
      (update :caret (fn [[line col]] [line (+ col (count val))]))
      (invalidate-lines line)))

(defn delete-symbol [{[line col] :caret :as state}]
  (-> state
      (delete-insert 1 "")
      (update :caret (fn [[line col]] [line (max 0 (dec col))]))
      (invalidate-lines line)))

(defn split-line [{:keys [text id] :as line} column new-id]
  [{:text (subs text 0 column)
    :id id}
   {:text (subs text column (count text))
    :id new-id}])

(defn concat-lines [line1 line2]
  {:text (str (:text line1) (:text line2))
   :id (:id line1)})

(defn delete-line [{[line-number col] :caret :as state}]
  (let [lines (:lines state)
        [before after] (split-at (dec line-number) lines)
        [to-concat rest] (split-at 2 after)
        new-lines (vec (concat before
                               [(apply concat-lines to-concat)]
                               rest))]
    (-> state
        (assoc :lines new-lines)
        (assoc :caret [(dec line-number) (count (:text (first to-concat)))])
        (invalidate-lines (dec line-number)))))

(defn indent-like [{sample-text :text :as sample} {subject-text :text :as subject}]
  (let [prefix (re-find #"\s*" sample-text)]
    [(assoc subject :text (str prefix (clojure.string/triml subject-text))) (count prefix)]))

(defn insert-line [{:keys [next-id lines caret] :as state}]
  (let [[line-number column] caret
        [lines-before lines-after] (split-at line-number lines)
        current-line (first lines-after)
        [old-line new-line] (split-line current-line column next-id)
        [new-line indentation] (indent-like old-line new-line)
        new-lines (vec (concat lines-before
                               [old-line
                                new-line]
                               (rest lines-after)))]
    (-> state
        (update :next-id inc)
        (assoc :lines new-lines)
        (assoc :caret [(inc line-number) indentation])
        (invalidate-lines line-number))))

(defn on-backspace [{[line col] :caret :as state}]
  (cond
    (and (zero? line) (zero? col)) state
    (zero? col) (delete-line state)
    :else (delete-symbol state)))

(defn on-enter [state]
  (insert-line state))

(defn line-selection [selection line]
  (let [[[from-line from-col] [to-line to-col]] selection]
    (when (<= from-line line to-line)
      [(if (= line from-line) from-col 0)
       (if (= line to-line) to-col :infinity)])))

(defn fragments [s fragments]
  (->> fragments
       (reduce (fn [[s res] frag]
                 (if (= frag :infinity)
                   (reduced [nil (conj! res s)])
                   [(subs s frag) (conj! res (subs s 0 frag))]))
               [s (transient [])])
       (second)
       (persistent!)))

(defn <pos [[l1 c1] [l2 c2]]
  (if (= l1 l2)
    (< c1 c2)
    (< l1 l2)))

(defn min-pos [p1 p2]
  (if (<pos p1 p2) p1 p2))

(defn max-pos [p1 p2]
  (if (<pos p1 p2) p2 p1))

(defn move-caret [{:keys [lines caret selection] :as state} dir selection?]
  (let [[sel-from sel-to] selection
        [line col] caret
        prev-line  (:text (get lines (dec line)))
        current-line (:text (get lines line))
        next-line (:text (get lines (inc line)))
        caret' (case dir
                 :left (if (= col 0)
                         [(max 0 (dec line)) (or (some-> prev-line count) 0)]
                         [line (dec col)])
                 :right (if (= col (count current-line))
                          [(min (dec (count lines)) (inc line)) (if (some? next-line)
                                                                  0
                                                                  col)]
                          [line (inc col)])
                 :up [(max 0 (dec line)) (min col (or (some-> prev-line (count)) col))]
                 :down [(min (dec (count lines)) (inc line)) (min col (or (some-> next-line (count)) col))])
        selection' (cond
                     (not selection?) [caret' caret']
                     (= caret sel-from) [(min-pos caret' sel-to) (max-pos caret' sel-to)]
                     (= caret sel-to) [(min-pos sel-from caret') (max-pos sel-from caret')]
                     :else [(min-pos caret caret') (max-pos caret caret')])]
    (-> state
        (assoc :caret caret')
        (assoc :selection selection'))))

(defn set-caret [state [line column]]
  (let [max-column (count (get-in state [:lines line :text]))
        max-line (dec (count (:lines state)))
        caret [(min line max-line) (min column max-column)]]
    (-> state
        (assoc :caret caret)
        (assoc :selection [caret caret]))))

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

(defn add-offsets [x y]
  (if (or (= x :infinity) (= y :infinity))
    :infinity
    (+ x y)))

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

(defn style [m]
  (reduce-kv (fn [s k v]
               (str s (name k) ":" (if (keyword? v) (name v) v) ";")) nil m))

(defn render-attrs [m]
  (reduce-kv (fn [s k v]
               (str s " " (name k) "=\"" (if (keyword? v) (name v) v) "\"")) nil m))

(defn html [[tag & rest :as el]]
  (when el
    (if (string? el) el
        (let [[attrs? & children :as rest] (if (string? rest) nil rest)
              html-tag (str "<" (name tag) " "
                            (when (map? attrs?)
                              (render-attrs attrs?)) ">")
              children (map html (if (map? attrs?) children rest))
              closing-tag (str "</" (name tag) ">")]
          (apply str html-tag (concat children [closing-tag])))))) 

(html [:div
       {:style
        (style {:background-color :red
                :height "5px"
                :position :absolute
                :top 0}
               )}
       [:pre "fuck"]])

(defn render-selection [[from to] {:keys [width height]}]
  [:div
   {:style
    (style (merge {:background-color theme/selection
                   :height (px height)
                   :position :absolute
                   :top 0}
                  (if (= to :infinity)
                    {:left 0
                     :margin-left (px (* from width))
                     :width "100%"}
                    {:left (px (* from width))
                     :width (px (* (- to from) width))})))}])

(defn render-text [tokens text]
  (into
   [:pre {:style
          (style {:position :absolute
                    :left 0
                    :top 0})}]
   (let [frags (fragments text (concat (map first tokens) [:infinity]))]
     (map (fn [s style-map]
            [:span {:style (style style-map)} s])
          frags
          (concat (map second tokens) (repeat nil))))))

(defn shred-selection-with-tokens [sel-tokens tokens]
  (let [token-ranges  (first
                       (reduce (fn [[result offset] [len ttype]]
                                 [(conj result
                                        {:range [offset (add-offsets offset len)]
                                         :style (get theme/token-styles ttype)})
                                  (add-offsets offset len)]) [[] 0] tokens))
        sel-ranges (first
                    (reduce (fn [[result offset] [len ttype]]
                              [(conj result
                                     {:range [offset (add-offsets offset len)]
                                      :style (get theme/token-styles ttype)})
                               (add-offsets offset len)]) [[] 0] sel-tokens))]
    (shred (concat token-ranges sel-ranges))))

(defn render-caret [col {:keys [width height]}]
  [:div {:style (style {:width "1px"
                          :top 0
                          :background-color "red"
                          :position :absolute
                          :left (px (* col width))
                          :height (px height)})}])




(defn line-renderer-very-slow [{:keys [text tokens] :as line} *caret caret-here? line-selection metrics]
  [:div {:dangerouslySetInnerHTML
         {:__html
          (html
           [:div
            (when line-selection
              (render-selection line-selection metrics))
            (let [tokens (let [sel-tokens (when-let [[from to] line-selection]
                                            [[from nil] [(subtract-offsets to from) :selected]])]
                           (if (and (seq tokens) (some? sel-tokens))
                             (shred-selection-with-tokens sel-tokens tokens)
                             (map (fn [[len ttype]] [len (get theme/token-styles ttype)]) (or tokens sel-tokens [[:infinity nil]]))))]
              (render-text tokens text))
            (when caret-here?
              (let [[_ caret-col] @*caret]
                (render-caret caret-col metrics)))])
          }}])

(defn line-renderer [state index metrics]
  (let [*caret (reaction (:caret @state))]
    (fn [_ index metrics]
      (let [caret-here? (= index (first @*caret))
            line (nth (:lines @state) index)
            line-selection (line-selection (:selection @state) index)]
        [line-renderer-very-slow line *caret caret-here? line-selection metrics]))))

(defn on-mouse-click [line column]
  (swap! state #(set-caret % [line column])))

"transform: translate3d(0px, -5904px, 0px);"

(defn editor [state]
  (let [{line-height :height
         ch-width :width :as metrics} (measure "X")
        dom-input (atom nil)
        lines-count (reaction (count (:lines @state)))
        caret-line (reaction (get-in @state [:caret 0]))
        view-region #js{}]
    [:div {:style {:display :flex
                   :background-color theme/background
                   :cursor :text
                   :left 0
                   :flex 1}}
     [:> (-> js/window ($ :ReactVirtualized) ($ :AutoSizer))
      (fn [m]
        (reagent/as-element
         [(fn []
            #_(prn "NEW COMP!")
            (let [listener (clojure.core/atom false)]
            [:> (-> js/window ($ :ReactVirtualized) ($ :List))
             {:ref (fn [this]
                     #_(prn "LISTENER: " @listener)
                     (when-not @listener
                       #_(prn "ADD LISTENER " @listener)

                       (when-let [node (reagent/dom-node this)]
                         #_(prn node)
                         (reset! listener true)
                         (.addEventListener node "focus"
                                            (fn []
                                              (when @dom-input
                                                (.focus @dom-input))))
                         (.addEventListener node "click"
                                            (fn [event]
                                              (let [view-region ($ view-region :value)
                                                    client-x ($ event :pageX)
                                                    client-y ($ event :pageY)
                                                    x client-x
                                                    y (- (+ client-y ($ view-region :scrollTop)) (/ line-height 2))]
                                                (on-mouse-click (Math/round (/ y line-height))
                                                                (Math/round (/ x ch-width))))
                                              (.stopPropagation event)
                                              (.preventDefault event))))))
              :height ($ m :height)
              :width ($ m :width)
              :scrollToIndex @caret-line
              :rowCount @lines-count
              :onScroll (fn [vr]
                          ($! view-region :value vr))
              :rowHeight line-height
              :overscanRowCount 100
              :rowRenderer (fn [s]
                             (let [index ($ s :index)
                                   style ($ s :style)
                                   id  (:id (nth (:lines @state) index))]
                               (reagent/as-element
                                [:div {:style style
                                       :key id}
                                 [line-renderer state index metrics]])))
              :noRowsRenderer (fn [] (reagent/as-element [:div "hello empty"]))}]))]))]
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
                     (swap! state type-in val)))}]]))

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
  (let [[next-id lines!] (reduce (fn [[next-id lines] s]
                           [(inc next-id) (conj! lines {:text s
                                                        :id next-id})])
                         [(:next-id state) (transient [])]
                         (clojure.string/split-lines text))]
    (-> state
        (assoc :lines (persistent! lines!))
        (assoc :next-id next-id)
        (assoc :first-invalid 0)
        (update :timestamp inc))))


(defn fake-text [] "public static void main() {\n return 0; \n }")

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
                       (attach-lexer! @state)
                       (swap! state set-text (fake-text) #_text)
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
  (with-codemirror
    #(with-virtualized
       (fn []
         (reagent/render [main] (.getElementById js/document "app"))))))

(defn init! []
  (mount-root))

(defn capture [f]
  (fn [evt _]
    (f)
    (.stopPropagation evt)
    (.preventDefault evt)))

(defn- bind-function! [key f & args]
  (keybind/bind! key :global (capture #(swap! state (fn [s] (apply f s args))))))

(bind-function! "shift-left" move-caret :left true)
(bind-function! "shift-right" move-caret :right true)
(bind-function! "shift-up" move-caret :up true)
(bind-function! "shift-down" move-caret :down true)

(bind-function! "left" move-caret :left false)
(bind-function! "down" move-caret :down false)
(bind-function! "right" move-caret :right false)
(bind-function! "up" move-caret :up false)
(bind-function! "tab" (fn [state] (type-in state "    ")))

(bind-function! "enter" on-enter)

(bind-function! "backspace" on-backspace)

(defn bench [state]
  (js/console.log "BENCH LEXING")
  (let [{:keys [input output :as worker]} (lexer/new-lexer-worker "text/x-java")
        start-time ($ js/Date now)]
    (go
     (doseq [[idx {:keys [text]}] (map vector (range) (:lines state))]
       (a/>! input {:index idx :text text})
       (let [resp (a/<! output)]
         nil
         #_(prn "RESP " resp)))
     (js/console.log "FILE LEXING TIME: " (- ($ js/Date now) start-time))))
  state)

(bind-function! "ctrl-l" bench)
