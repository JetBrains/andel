(ns andel.render
  (:require [andel.text :as text]
            [andel.intervals :as intervals]
            [andel.array-list :as al]
            [andel.tree :as tree]
            [andel.theme :as theme]
            [andel.utils :as utils]
            [andel.controller :as controller])
  (:import [andel.intervals Marker Attrs]
           #?(:clj  [java.util PriorityQueue Comparator])))

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
  {:height           (str (utils/line-height metrics) "px")
   :width            "100%"
   :background-color (:bg-05 theme/zenburn)
   :position         :absolute
   :left             0
   :top              0})

(defn caret-style [col {:keys [width] :as metrics}]
  {:width            "1px"
   :top              0
   :position         :absolute
   :left             (str (* col width) "px")
   :height           (str (utils/line-height metrics) "px")})

#?(:cljs
   (defn make-pendings []
     (array))
   :clj
   (defn make-pendings []
     (PriorityQueue. 10 ^Comparator (comparator (fn [^Marker a ^Marker b]
                                                  (< (.-to a) (.-to b)))))))

#?(:cljs
   (defn next-pending [pendings]
     (reduce (fn [c p] (if (or (nil? c) (< (.-to p) (.-to c))) p c)) nil pendings))
   :clj
   (defn next-pending [^PriorityQueue pendings]
     (if (.isEmpty pendings)
       nil
       (.peek pendings))))

#?(:cljs
   (defn remove-pending! [arr p]
     (let [idx (.indexOf arr p)]
       (if (< -1 idx)
         (do
           (.splice arr idx 1)
           arr)
         arr)))
   :clj
   (defn remove-pending! [^PriorityQueue pendings p]
     (.remove pendings p)))

(defn add-pending! [^PriorityQueue pendings p]
  (.add pendings p))

(defn shred-markup
  "Consumes andel.intervals.Marker. Transducer with a quirk: it will call downstream with (r-f state token-length token-class)"
  [type]
  (let [marker-class (fn [^Marker m]
                       (case type
                         :background (some-> m ^Attrs (.-attrs) (.-background))
                         :foreground (some-> m ^Attrs (.-attrs) (.-foreground))))
        join-classes (fn [^PriorityQueue markers]
                       (let [size (.size markers)]
                         (cond
                           (= 0 size) nil
                           (= 1 size) (marker-class (.peek markers))
                           :else (transduce (comp
                                             (map marker-class)
                                             (interpose " "))
                                            (completing
                                             (fn [^java.lang.StringBuilder sb ^java.lang.String i]
                                               (.append sb i))
                                             str)
                                            (java.lang.StringBuilder.)
                                            markers))))]
    (fn [rf]
      (let [pendings (make-pendings)
            *last-pos (atom 0)]
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
                    (if (= last-pos (.-to p))
                      (recur res)
                      (recur (rf res (- (.-to p) last-pos) new-class))))
                  res))))))))))

(defn multiplex [& rfs]
  (let [rfs (into [] rfs)]
    (fn [r-f]
      (let [states (into [] (map (fn [f] (volatile! (f)))) rfs)]
        (fn
          ([] (r-f))
          ([r i]
           (dotimes [idx (count rfs)]
             (let [s (nth states idx)
                   rf (nth rfs idx)]
               (vswap! s rf i)))
           r)
          ([r] (r-f (r-f r (into []
                                 (map-indexed (fn [idx rf]
                                                (let [s (nth states idx)]
                                                  (rf (deref s)))))
                                 rfs)))))))))

(defonce records
  (do
    (defrecord LineInfo
        [text
         caret
         selection
         foreground
         background
         widgets])
    :done))

(def collect-to-array
  (fn
    ([] (al/into-array-list []))
    ([r a]
     (doto r (al/conj! a)))
    ([r a b]
     (doto r
       (al/conj! a)
       (al/conj! b)))
    ([r] r)))

(defn merge-tokens [^java.util.ArrayList lexer-markers]
  (fn [rf]
    (let [lexer-markers-count (.size lexer-markers)
          *i                  (atom 0)]
      (fn
        ([] (rf))
        ([acc ^Marker m]
         (loop [i   (long @*i)
                acc acc]
           (if (and (< i lexer-markers-count) (< (.-from ^Marker (.get lexer-markers i)) (.-from ^Marker m)))
             (recur (inc i) (rf acc (.get lexer-markers i)))
             (do
               (reset! *i i)
               (rf acc m)))))
        ([acc]
         (loop [i   (long @*i)
                acc acc]
           (if (< i lexer-markers-count)
             (recur (inc i)
                    (rf acc (.get lexer-markers i)))
             (rf acc))))))))

(defn to-geom-offsets [zipper]
  (fn [r-f]
    (let [loc (volatile! zipper)]
      (fn
        ([] (r-f))
        ([state len class]
         (let [loc' (text/retain @loc len)
               len' (- (text/geom-offset loc') (text/geom-offset @loc))]
           (vreset! loc loc')
           (r-f state len' class)))
        ([state] (r-f state))))))

(defn ^LineInfo build-line-info [{:keys [caret
                                         lexer-state
                                         selection
                                         document-markers-zipper
                                         editor-markers-zipper
                                         ^long start-offset
                                         ^long start-geom-offset
                                         ^long end-offset
                                         text-zipper]} widgets]
  (let [document-markup (intervals/xquery-intervals document-markers-zipper start-offset end-offset)
        editor-markup (intervals/xquery-intervals editor-markers-zipper start-offset end-offset)
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
               (shred-markup :background)
               (to-geom-offsets text-zipper))
        fg-xf (comp
               (filter (fn [^Marker marker] (.-foreground ^Attrs (.-attrs marker))))
               (shred-markup :foreground))
        widgets-xf (keep (fn [^Marker marker]
                           (some-> (get widgets (.-id ^Attrs (.-attrs marker)))
                                   (assoc :ends-on-this-line? (<= start-offset (.-to marker) end-offset)
                                          :starts-on-this-line? (<= start-offset (.-from marker) end-offset)))))
        tokens (if (some? lexer-state)
                 (intervals/lexemes lexer-state start-offset end-offset)
                 (al/empty-array-list))]
    (transduce
     (comp
      (merge-tokens (al/into-array-list editor-markup))
      (multiplex (widgets-xf collect-to-array)
                ((comp to-relative-offsets
                       (merge-tokens tokens)
                       (multiplex (bg-xf collect-to-array)
                                  (fg-xf collect-to-array)))
                  (fn
                    ([] nil)
                    ([r] r)
                    ([r i] i)))))
     (completing
      (fn [acc [widgets [bg fg]]]
        (LineInfo. text caret selection fg bg widgets)))
     nil
     document-markup)))

(defn line-selection [[^long from ^long to] ^long line-start-offset ^long line-end-offset]
  (cond (= from to) nil

        (and (< from line-start-offset) (< line-start-offset to))
        (if (< line-end-offset to)
          [0 :infinity]
          [0 (- to line-start-offset)])

        (and (<= line-start-offset from) (<= from line-end-offset))
        [(- from line-start-offset)
         (if (<= to line-end-offset)
           (- to line-start-offset)
           :infinity)]

        :else nil))
