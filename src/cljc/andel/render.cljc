(ns andel.render
  (:require [andel.text :as text]
            [andel.intervals :as intervals]
            [andel.tree :as tree]
            [andel.theme :as theme]
            [andel.utils :as utils])
  (:import [andel.intervals Marker Attrs]
           [java.awt Color]
           [com.intellij.openapi.editor.markup TextAttributes]
           [java.util TreeSet Comparator]
           [com.intellij.openapi.editor.ex.util LexerAndelHighlighter]))

(defn infinity? [x] (keyword? x))

(defn style [m]
  (reduce-kv
   (fn [s k v]
     (str s (name k) ":" (if (keyword? v) (name v) v) ";"))
   nil m))

(defn selection-style [[from to] {:keys [width] :as metrics}]
  {:background-color theme/selection
   :height           (str  (utils/line-height metrics) "px")
   :position         :absolute
   :top              "0px"
   :left             (if (infinity? to)
                       0
                       (str (* from width) "px"))
   :margin-left      (when (infinity? to) (str (* from width) "px"))
   :width            (if (infinity? to)
                       "100%"
                       (str (* (- to from) width) "px"))})

(defn active-line-style [metrics]
  {:height           (str (inc (utils/line-height metrics)) "px")
   :width            "100%"
   :background-color (:bg-05 theme/zenburn)
   :position         :absolute
   :left             0
   :top              0
   :z-index          "-1"})

(defn caret-style [col {:keys [width] :as metrics}]
  {:width            "1px"
   :animation        "blinker 1s cubic-bezier(0.68, -0.55, 0.27, 1.55) infinite"
   :top              0
   :background-color "white"
   :position         :absolute
   :left             (str (* col width) "px")
   :height           (str (inc (utils/line-height metrics)) "px")})

#?(:cljs
   (defn make-pendings []
     (array))
   :clj
   (defn make-pendings []
     (TreeSet. ^Comparator (comparator (fn [^Marker a ^Marker b]
                                       (< (.-to a) (.-to b)))))))

#?(:cljs 
   (defn next-pending [pendings]
     (reduce (fn [c p] (if (or (nil? c) (< (.-to p) (.-to c))) p c)) nil pendings))
   :clj
   (defn next-pending [^TreeSet pendings]
     (if (.isEmpty pendings)
       nil
       (.first pendings))))

#?(:cljs 
   (defn remove-pending! [arr p]
     (let [idx (.indexOf arr p)]
       (if (< -1 idx)
         (do
           (.splice arr idx 1)
           arr)
         arr)))
   :clj
   (defn remove-pending! [^TreeSet pendings p]
     (.remove pendings p)))

#?(:cljs 
   (def add-pending! push!)
   :clj
   (defn add-pending! [^TreeSet pendings p]
     (.add pendings p)))

(defn shred-markup [type]
  (fn [rf]
        (let
          [pendings (make-pendings)
           *last-pos (atom 0)
           join-classes (fn [markers]
                          (->> markers
                               (map (fn [^Marker m]
                                      (case type
                                        :background (some-> m ^Attrs (.-attrs) (.-background))
                                        :foreground (some-> m ^Attrs (.-attrs) (.-foreground)))))
                               (clojure.string/join " ")))]
      (fn
        ([] (rf))
        ([res ^Marker m]
         (loop [res res]
           (let [^Marker p (next-pending pendings)
                 last-pos  @*last-pos
                 new-class (join-classes pendings)]
             (if (or (nil? p) (< (.-from m) (.-to p)))
               (do
                 (add-pending! pendings m)
                 (reset! *last-pos (.-from m))
                 (if (identical? last-pos (.-from m))
                   res
                   (rf res (- (.-from m) last-pos) new-class)))

               (do
                 (remove-pending! pendings p)
                 (reset! *last-pos (.-to p))
                 (if (identical? last-pos (.-to p))
                   (recur res)
                   (recur (rf res (- (.-to p) last-pos) new-class))))))))
        ([res]
         (rf
           (loop [res res]
             (let [^Marker p (next-pending pendings)
                   last-pos  @*last-pos
                   new-class (join-classes pendings)]
               (if (some? p)
                 (do
                   (remove-pending! pendings p)
                   (reset! *last-pos (.-to p))
                   (if (identical? last-pos (.-to p))
                     (recur res)
                     (recur (rf res (- (.-to p) last-pos) new-class))))
                 res)))))))))

(defn multiplex [rf1 rf2]
  (fn [rf]
    (fn
      ([] (transient [(rf1) (rf2)]))
      ([result input]
       (assoc! result
               0 (rf1 (get result 0) input)
               1 (rf2 (get result 1) input)))
      ([result] (rf (rf) [(rf1 (get result 0)) (rf2 (get result 1))])))))

(defn transduce2
  ([xform f coll]
   (let [r-f (xform f)]
     (r-f (reduce r-f (r-f) coll))))
  ([xform f init coll]
   (transduce2 xform
               (fn
                 ([] init)
                 ([acc input] (f acc input)))
               coll)))

(defrecord LineInfo
  [text
   caret
   selection
   foreground
   background])

(def collect-to-array
  (fn
    ([] (tree/into-array-list []))
    ([r a b]
     (doto r
       (tree/push! a)
       (tree/push! b)))
    ([r] r)))

(defn merge-tokens [lexer-markers]
  (fn [rf]
    (let [lexer-markers-count (count lexer-markers)
          *i                  (atom 0)]
      (fn
        ([] (rf))
        ([acc m]
         (loop [i   @*i
                acc acc]
           (if (and (< i lexer-markers-count) (< (.-from (aget lexer-markers i)) (.-from m)))
             (recur (inc i) (rf acc (aget lexer-markers i)))
             (do
               (reset! *i i)
               (rf acc m)))))
        ([acc]
         (loop [i   @*i
                acc acc]
           (if (< i lexer-markers-count)
             (recur (inc i)
                    (rf acc (aget lexer-markers i)))
             (rf acc))))))))

(defn ^LineInfo build-line-info [{:keys [caret selection markers-zipper start-offset end-offset deleted-markers tokens text-zipper]}]
  (let [markup (intervals/xquery-intervals markers-zipper start-offset end-offset)
        text (text/text text-zipper (- end-offset start-offset))
        text-length (count text)
        to-relative-offsets (map
                              (fn [^Marker marker]
                                (intervals/->Marker (min text-length (max 0 (- (.-from marker) start-offset)))
                                                    (min text-length (max 0 (- (.-to marker) start-offset)))
                                                    false
                                                    false
                                                    (.-attrs marker))))
        bg-xf (comp
               (filter (fn [^Marker marker] (.-background ^Attrs (.-attrs marker))))
               (shred-markup :background))
        fg-xf (comp
               (filter (fn [^Marker marker] (.-foreground ^Attrs (.-attrs marker))))
               (shred-markup :foreground))]

    (transduce2
     (comp
      (remove (fn [^Marker m] (contains? deleted-markers (.-id ^Attrs (.-attrs m)))))
      to-relative-offsets
      (merge-tokens tokens)
      (multiplex (bg-xf collect-to-array)
                 (fg-xf collect-to-array)))
     (fn [acc [bg fg]]
       (LineInfo. text caret selection fg bg))
     nil
     markup)))

(defn line-props-equiv? [old new]
  (let [end-offset (max (:end-offset old) (:end-offset new))]
    (and (= (- (:end-offset old) (:start-offset old)) (- (:end-offset new) (:start-offset new)))
         (tree/compare-zippers (:text-zipper old)
                               (:text-zipper new)
                               (text/by-offset end-offset))
         (tree/compare-zippers (:markers-zipper old)
                               (:markers-zipper new)
                               (intervals/by-offset end-offset))
         (= (:selection old) (:selection new))
         (= (:caret old) (:caret new))
         (identical? (:deleted-markers old) (:deleted-markers new))
         (identical? (:tokens old) (:tokens new)))))

(defn line-selection [[from to] line-start-offset line-end-offset]
  (cond (and (< from line-start-offset) (< line-start-offset to))
        (if (< line-end-offset to)
          [0 :infinity]
          [0 (- to line-start-offset)])
        (and (<= line-start-offset from) (<= from line-end-offset))
        [(- from line-start-offset)
         (if (<= to line-end-offset)
           (- to line-start-offset)
           :infinity)]
        :else nil))

(defn viewport-info [{metrics :metrics
                      [w h] :view-size
                      [_ from-y-offset] :pos :as viewport}]
  (let [line-height (utils/line-height metrics)
        top-line (int (/ from-y-offset line-height))]
    {:top-line top-line
     :bottom-line (+ top-line (int (/ h line-height)))
     :y-shift (double (- (* line-height (- (/ from-y-offset line-height) top-line))))}))


(defn to-hex [^Color color]
  (str "#" (subs (Integer/toHexString (.getRGB color)) 2)))


(defn text-attrs [^TextAttributes attrs]
  #:text-attrs {:foreground (some-> (.getForegroundColor attrs) (to-hex))
                :background (some-> (.getBackgroundColor attrs) (to-hex))
                :effectColor (some-> (.getEffectColor attrs) (to-hex))
                :effectType (or (some-> (.getEffectType attrs) (.ordinal)) -1)
                :fontType (.getFontType attrs)})

(defn- underwave [color]
  {:border-color color
   :border-bottom-style :dotted
   :border-bottom-width "1px"})

(def attrs->styles
  (memoize
    (fn [{:text-attrs/keys [background foreground effectColor effectType fontType]}]
      (let [result (-> {:foreground {}
                        :background {}}
                       (cond->
                         foreground (assoc-in [:foreground :color] (str foreground " !important"))
                         background (assoc-in [:background :background-color] background)
                         (or (identical? fontType 1)
                             (identical? fontType 3)) (assoc-in [:foreground :font-weight] "500" ;;font-weight-bold
                                                                )

                         (or (identical? fontType 2)
                             (identical? fontType 3)) (assoc-in [:foreground :font-style] "italic")

                         (and effectColor (= effectType 0)) (update :background merge {:border-bottom-style :solid
                                                                                       :border-color effectColor
                                                                                       :border-width "1px"}) ;;underscore

                         (and effectColor (= effectType 1)) (update :background merge
                                                                    (underwave effectColor)
                                                                    {:position :relative}) ;;wave-underscore

                         (and effectColor (= effectType 2)) (update :background merge {:border-style :solid
                                                                                       :border-color effectColor
                                                                                       :border-width "1px"}) ;;boxed

                         (and effectColor (= effectType 4)) (update :background merge {:border-bottom-style :solid
                                                                                       :border-color effectColor
                                                                                       :border-width "2px"}) ;; bold-underscore

                         (and effectColor (= effectType 5)) (update :background merge  {:border-bottom-style :dotted
                                                                                        :border-color effectColor
                                                                                        :border-width "2px"}) ;; bold-dotted

                         (and effectColor (= effectType 7)) (update :background merge  {:border-style :solid
                                                                                        :border-radius "3px"
                                                                                        :border-color effectColor
                                                                                        :border-width "1px"})))]
        (cond-> result
                (empty? (:foreground result)) (dissoc :foreground)
                (empty? (:background result)) (dissoc :background))))))

(def attrs->class
  (let [cache (atom {})]
    (fn [attrs layer]
      (let [{:text-attrs/keys [background foreground effectColor effectType fontType]} attrs
            k (str layer background foreground effectColor effectType fontType)]
        (if-let [t (find @cache k)]
          (val t)
          (let [style (attrs->styles attrs)
                v (some-> style (get layer) (onair.frontend.styles/style->class))]
            (swap! cache assoc k v)
            v))))))

(defn token-range [^com.intellij.openapi.editor.ex.util.LexerAndelHighlighter$TokensContainer tokens from to]
    (let [idx-from (.getTokenIndexByOffset tokens from)
          idx-to (.getTokenIndexByOffset tokens to)
          tokens-count (.getTokensCount tokens)
          len (- to from)]
      (loop [result []
             i idx-from]
        (if (and (<= i idx-to) (< i tokens-count))
            (let [start (-> (.getStart tokens i) (- from) (max 0))
                  end (-> (.getEnd tokens i) (- from) (min len))
                  attrs (text-attrs (.getTextAttributes tokens i))
                  m (intervals/->Marker start end false false (intervals/->Attrs nil (attrs->class attrs :background) (attrs->class attrs :foreground) 0))]
              (recur (conj result m) (inc i)))
            (into-array result)))))

(defn viewport-lines [state viewport-info]
  (let [{{:keys [text lines markup hashes deleted-markers tokens]} :document
         {:keys [caret selection]} :editor} state
        {:keys [top-line bottom-line]} viewport-info
        caret-offset (get caret :offset)]
    (tree/reducible
     (fn [f init]
       (loop [text-zipper    (text/scan-to-line-start (text/zipper text) top-line)
              markers-zipper (intervals/zipper markup)
              line-number    top-line
              result         init
              result-empty?  true]
         (if (and (or (>= line-number bottom-line) (tree/end? text-zipper))
                  (not result-empty?))
           result
           (let [start-offset          (text/offset text-zipper)
                 next-line-text-zipper (text/scan-to-line-start text-zipper (inc line-number))
                 end-offset            (cond-> (text/offset next-line-text-zipper)
                                         (not (tree/end? next-line-text-zipper)) (dec))
                 intersects?           (intervals/by-intersect start-offset end-offset)
                 overscans?            (intervals/by-offset end-offset)
                 markers-zipper        (tree/scan markers-zipper
                                                  (fn [acc metrics]
                                                    (or (intersects? acc metrics)
                                                        (overscans? acc metrics))))]
             (recur next-line-text-zipper
                    markers-zipper
                    (inc line-number)
                    (f result {:text-zipper     text-zipper
                               :line-number     line-number
                               :markers-zipper  markers-zipper
                               :tokens          (token-range tokens start-offset end-offset)
                               :start-offset    start-offset
                               :selection       (line-selection selection start-offset end-offset)
                               :caret           (when (and (<= start-offset caret-offset) (<= caret-offset end-offset))
                                                  (- caret-offset start-offset))
                               :end-offset      end-offset
                               :deleted-markers deleted-markers})
                    false))))))))
