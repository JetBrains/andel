(ns slurper.text
  (:require [slurper.tree :as tree]))

#?(:clj
   (do
     (defn array [& args] (object-array args))
     (def some-array (array 1 2 3))
     (defn array? [x]
       (= (type some-array) (type x)))))

#?(:clj
   (defn metrics [x]
     (cond
       (string? x) (let [l (count x)]
                     (array l
                            (loop [i 0
                                   c 0]
                              (if (= i l)
                                c
                                (if (= (.charAt x i) \newline)
                                  (recur (inc i) (inc c))
                                  (recur (inc i) c))))))
       (char? x) (array 1 (if (= x \newline) 1 0))))
   :cljs
   (defn metrics [x]
     (assert (string? x))
     (let [l (.-length x)]
       (array l
              (loop [i 0
                     c 0]
                (if (identical? i l)
                  c
                  (if (identical? (.charAt x i) \newline)
                    (recur (inc i) (inc c))
                    (recur (inc i) c))))))))


(defn r-f
  ([] (array 0 0))
  ([[x1 x2 :as acc] [y1 y2]]
   (def acc acc)
   (def y1y2 [y1 y2])
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
          (tree/jump-down (+ rel-offset (count s))))))) ; FIXME

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
                      (if (tree/root? chunk)
                        (tree/replace chunk (tree/make-node "" tree-config))                        
                        (tree/remove chunk))
                      (-> chunk
                          (tree/edit (fn [{s :children}]
                                       (tree/make-node (str (subs s 0 rel-offset) (subs s end)) tree-config)))
                          (scan-to-offset i)))
          deleted-c (- end rel-offset)]
      (if (< deleted-c l)
        (recur next-loc (- l deleted-c))
        next-loc))))

(defn lazy-text [loc l]
  (when (< 0 l)
    (if (tree/end? loc)
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
            (list s)))))))

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

  (-> (make-text "aabbccddeeffgghh")
      (zipper)
      (retain 4)
      (delete (count "ccddeeffgg"))
      (root))
  
  @tree/merge-children-args


  tree/b-children

  (make-text "000000000000000000000000")

  (-> (make-text "1234")
      (zipper)
      (insert "0")
      (delete 2)
      (root))

  (-> (make-text "12345678")
      (zipper)
      (insert "0")
      (delete 8)      
      (root))

  (-> (make-text "m929E85n")
      (zipper)
      (delete (count "m929E85n"))
      (insert "1"))
  
  (root)

  tree/merge-children-args
      
      

  (delete (count "00"))
  (root)
      

  (-> (make-text "000000000000000000000000")
      (zipper)
      (delete (count "0000000000000000"))
      (root))
  
  #_(delete (count "00000000"))
  #_(retain 13)
  (root)

  (-> (make-text "abcd\nefgh\nklmo\nprst")
      (play [[:retain 5] [:insert "xxx"] [:delete "efgh"] [:retain 10]])
      (debug-tree))
  
  (lines-count (make-text "abcd\nefgh\nklmo\nprst"))

  
  (->
   (make-text "0000000000000000")
   (zipper)
   (delete 3)
   (retain 13))
   
  (delete 12)
  (delete (count "Vu0006R79y9U4"))

  @tree/next-locs

  (array 1 2)


  (-> (make-text "000000000000000000000000000000000000000000000000")
      (play [[:retain 4] [:delete "0000000000000000000000000000000"] [:retain 13]]))
  (zipper)
  (retain 4)
  (delete (count "0000000000000000000000000000000"))
  (retain 13)
      


  

  )

