(ns andel.render
  (:require [andel.text :as text]
            [andel.intervals :as intervals]
            [andel.array-list :as al]
            [andel.tree :as tree]
            [andel.theme :as theme]
            [andel.utils :as utils])
  (:import [andel.intervals Marker Attrs]
           #?(:clj  [java.util TreeSet Comparator])))

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

(defonce records
  (do
    (defrecord LineInfo
        [text
         caret
         selection
         foreground
         background])
    :done))

(def collect-to-array
  (fn
    ([] (al/into-array-list []))
    ([r a b]
     (doto r
       (al/conj! a)
       (al/conj! b)))
    ([r] r)))

(defn merge-tokens [^"[Ljava.lang.Object;" lexer-markers]
  (fn [rf]
    (let [lexer-markers-count (count lexer-markers)
          *i                  (atom 0)]
      (fn
        ([] (rf))
        ([acc ^Marker m]
         (loop [i   @*i
                acc acc]
           (if (and (< i lexer-markers-count) (< (.-from ^Marker (aget lexer-markers i)) (.-from ^Marker m)))
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

(defn ^LineInfo build-line-info [{:keys [caret lexer-state selection markers-zipper start-offset end-offset deleted-markers text-zipper]}]
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
               (shred-markup :foreground))
        tokens (intervals/lexemes lexer-state start-offset end-offset)]
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
         (identical? (:lexer-state old) (:lexer-state new)))))

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

(defn widget-pixels-position [{{:keys [metrics] :as viewport} :viewport
                               {:keys [text]} :document :as state} {:keys [grid-position] :as widget}]
  (let [[x y] (utils/grid-position->pixels grid-position viewport)]
    [x (+ y (utils/line-height metrics))]))

(defn viewport-lines [state viewport-info]
  (let [{{:keys [text lines markup hashes deleted-markers lexer]} :document
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
                               :lexer-state     lexer
                               :start-offset    start-offset
                               :selection       (line-selection selection start-offset end-offset)
                               :caret           (when (and (<= start-offset caret-offset) (<= caret-offset end-offset))
                                                  (- caret-offset start-offset))
                               :end-offset      end-offset
                               :deleted-markers deleted-markers})
                    false))))))))
