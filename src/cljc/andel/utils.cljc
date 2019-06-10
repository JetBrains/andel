(ns andel.utils
  (:require [andel.text :as text]
            [clojure.spec.alpha :as s]))

(defn line-height ^double [{:keys [^double height ^double spacing] :as metrics}]
  (+ height spacing))

(defn pixels->line-col [[^double x ^double y] text metrics]
  (let [line (int (Math/floor (/ y (line-height metrics))))
        line-zipper (-> (text/zipper text)
                        (text/scan-to-line-start line))
        col-zipper (text/skip-columns line-zipper (int (Math/round (/ x (:width metrics)))))]
    {:line line
     :col (- (text/offset col-zipper) (text/offset line-zipper))}))

(defn offset->geom-offset ^long [zipper ^long offset]
  (-> zipper
      (text/scan-to-offset offset)
      (text/geom-offset)))

(defn selection-to-geom [line-zipper [from to :as selection]]
  (let [line-start (text/offset line-zipper)
        line-start-geom (text/geom-offset line-zipper)
        from-abs (+ ^long from line-start)
        zipper-from (text/scan-to-offset line-zipper from-abs)
        from-geom (- (text/geom-offset zipper-from) line-start-geom)
        to-geom (if (= to :infinity)
                  :infitiy
                  (- (text/geom-offset (text/scan-to-offset zipper-from (+ ^long to line-start)))
                     line-start-geom))]
    [from-geom to-geom]))

(defn line->offset ^long [line text]
  (text/offset (text/scan-to-line-start (text/zipper text) line)))

(defn line-col->offset ^long [{:keys [^long line ^long col]} text]
  (+ (line->offset line text) col))

(defn line-length ^long [line text]
  (text/distance-to-EOL (text/scan-to-line-start (text/zipper text) line)))

(defn line->from-to-offsets [^long line text]
  (let [from (line->offset line text)
        length (line-length line text)]
    [from (+ from length)]))

(comment

  (.length "
aaaaaaaaaaaaaaaaaaaaaaaddddddddddfaaaaaaaa
  ((((((((((((((((()))))))))))))))))")

  (def selection (:onair.editor/selection (:onair.editor/editor (onair.db/entity db editor-view-id))))

  (def original-tree (:onair.editor/text-tree (:onair.editor/document (:onair.editor/editor (onair.db/entity db editor-view-id)))))

  (-> (text/zipper original-tree)
      (text/scan-to-offset (first selection))
      (text/delete 80)
      (text/root))


  (def offset offset)
  (def text text)

  (text/as-string text)
  (text/text-length text)

  (def db (onair.kernel/db (onair.application/local-kernel)))
  (def editor-view-id (first (onair.tags/by-tag db :onair.editor/editor-view)))

  (text/as-string )

  (into {} (onair.db/entity db editor-view-id))


  (text/scan-to-offset)

  )

(defn offset->line ^long [offset text]
  (-> (text/zipper text)
      (text/scan-to-offset offset)
      (text/line)))

(defn offset->line-col [^long offset text]
  (let [line (offset->line offset text)
        line-offset (line->offset line text)
        col (- offset line-offset)]
    {:line line
     :col col}))

(defn offset->geom-line-col [^long offset text]
  (let [offset-loc (text/scan-to-offset (text/zipper text) offset)
        offset-geom (text/geom-offset offset-loc)
        line (text/line offset-loc)
        line-loc (text/scan-to-line-start (text/zipper text) line)
        line-geom (text/geom-offset line-loc)]
    {:line line
     :col (- offset-geom line-geom)}))

(defn line->loc [line text]
   (-> text
       (text/zipper)
       (text/scan-to-line-start line)))

(defn line-col->loc [{:keys [^long line ^long col]} text]
  (let [line-loc (line->loc line text)
        line-offset (text/offset line-loc)]
    (text/scan-to-offset line-loc (+ line-offset col))))

(defn line-number ^long [loc]
  (text/line loc))

(defn scan-to-next-line [loc]
  (text/scan-to-line-start loc (inc (line-number loc))))

(defn sets-intersect? [s1 s2]
  (if (<= (count s1) (count s2))
    (reduce (fn [r x] (if (contains? s2 x) (reduced true) r)) false s1)
    (sets-intersect? s2 s1)))

(defn offset->line-start [offset text]
  (line->offset (offset->line offset text) text))
