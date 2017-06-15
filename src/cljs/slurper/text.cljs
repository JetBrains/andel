(ns slurper.text
  (:require [slurper.tree :as tree]))

(defn metrics [x]
  (cond
    (string? x) (let [l (.-length x)]
                  (array l
                         (loop [i 0
                                c 0]
                           (if (identical? i l)
                             c
                             (if (identical? (.charAt x i) \newline)
                               (recur (inc i) (inc c))
                               (recur (inc i) c))))))
    (char? x) (array 1 (if (= x \newline) 1 0))))

(let [[x y] (array 45 1)]
  [x y])

(defn r-f
  ([] (array 0 0))
  ([[x1 x2 :as acc] [y1 y2]]
   (array (+ x1 y1)
          (+ x2 y2))))


(def tree-config {::tree/reducing-fn r-f
                  ::tree/metrics-fn metrics})

(defn make-text [s]
  (-> (tree/zipper (tree/make-node s tree-config)
                   tree-config)
      (assoc-in [1 :changed?] true)
      (tree/root)))

(defn zipper [tree]
  (tree/zipper tree tree-config))

(def root tree/root)

(defn by-offset [i]
  #(< i (nth % 0)))

(defn by-line [i]
  #(<= i (nth % 1)))

(def offset (comp first tree/loc-acc))
(def line (comp second tree/loc-acc))

(defn scan-to-offset [loc i]
  (tree/scan loc (by-offset i)))

(defn retain [loc l]
  (scan-to-offset loc (+ (offset loc) l)))

(defn scan-to-line [loc i]
  (-> loc
      (tree/scan (by-line i))
      (cond-> (< 0 i) (retain 1))))

(defn insert [loc s]
  (if (tree/branch? loc)
    (recur (tree/down loc) s)
    (let [i (offset loc)
          chunk (tree/up loc)
          chunk-offset (offset chunk)
          rel-offset (- i chunk-offset)]
      (-> chunk
          (tree/edit (fn [{:keys [children]}]
                       (tree/make-node (str (subs children 0 rel-offset) s (subs children rel-offset)) tree-config)))
          (tree/jump-down (+ rel-offset (count s)))))))

(defn delete [loc l]
  (if (tree/branch? loc)
    (recur (tree/down loc) l)
    (let [i (offset loc)
          chunk (tree/up loc)
          chunk-offset (offset chunk)
          rel-offset (- i chunk-offset)
          chunk-l (count (tree/children chunk))
          end (min chunk-l (+ rel-offset l))
          next-loc  (if (and (= rel-offset 0) (= end chunk-l))
                      (tree/remove chunk)
                      (tree/edit chunk (fn [{s :children}]
                                         (tree/make-node (str (subs s 0 rel-offset) (subs s end)) tree-config))))
          next-loc (scan-to-offset next-loc i)
          deleted-c (- end rel-offset)]
      (if (< deleted-c l)
        (recur next-loc (- l deleted-c))
        next-loc))))

(defn lazy-text [loc l]
  (if (and (tree/end? loc) (< 0 l))
    (throw (ex-info "Length is out of bounds" nil))
    (if (tree/branch? loc)
      (recur (tree/down loc) l)
      (let [i (offset loc)
            parent (tree/up loc)
            parent-offset (offset parent)
            parent-s (tree/children parent)
            start (- i parent-offset)
            end (min (count parent-s) (+ start l))
            s (subs parent-s start end)
            s-len (count s)]
        (if (< s-len l)
          (cons s (lazy-seq (lazy-text (tree/skip parent) (- l s-len))))
          (list s))))))

(defn lines-count [t]
  (inc (second (:metrics t))))

(defn text-length [t]
  (first (:metrics t)))

(defn text [loc l]
  (apply str (lazy-text loc l)))

(def reset tree/reset)

(defn debug-tree [t]
  (if (array? (:children t))
    (assoc t :children (vec (map debug-tree (:children t))))
    t))

(defn play [t operation]
  (root (reduce (fn [loc [code arg]]
                  (case code
                    :retain (retain loc arg)
                    :insert (insert loc arg)
                    :delete (delete loc (if (string? arg) (count arg) arg)))) (zipper t) operation)))

(comment

  (-> (make-text "abcd\nefgh\nklmo\nprst")
      (play [[:retain 5] [:insert "xxx"] [:delete "efgh"] [:retain 10]])
      (debug-tree))
  
  (lines-count (make-text "abcd\nefgh\nklmo\nprst"))
  (->
   (make-text "abcd\nefgh\nklmo\nprst")
   (zipper)
   (scan-to-line 1)
   (delete 0))

  @tree/next-locs

  (array 1 2)

  )

