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
                      (if (tree/root? chunk)
                        (tree/replace chunk (tree/make-node "" tree-config))
                        (tree/remove chunk))
                      (tree/edit chunk (fn [{s :children}]
                                         (tree/make-node (str (subs s 0 rel-offset) (subs s end)) tree-config))))
          next-loc (scan-to-offset next-loc i)
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

  (-> (make-text "abcd\nefgh\nklmo\nprst")
      (play [[:retain 5] [:insert "xxx"] [:delete "efgh"] [:retain 10]])
      (debug-tree))
  
  (lines-count (make-text "abcd\nefgh\nklmo\nprst"))

  [ [[:delete "000"] [:retain 13]]]
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
      


  [[clojure.zip$branch_QMARK_ invokeStatic "zip.clj" 73]
  [clojure.zip$branch_QMARK_ invoke "zip.clj" 69]
  [slurper.text$delete invokeStatic "text.cljc" 92]
  [slurper.text$delete invoke "text.cljc" 91]
  [slurper.text$play$fn__46470 invoke "text.cljc" 151]
  [clojure.lang.PersistentVector reduce "PersistentVector.java" 341]
  [clojure.core$reduce invokeStatic "core.clj" 6703]
  [clojure.core$reduce invoke "core.clj" 6686]
  [slurper.text$play invokeStatic "text.cljc" 147]
  [slurper.text$play invoke "text.cljc" 146]
  [slurper.text_test$fn__45981 invokeStatic "text_test.clj" 56]
  [slurper.text_test$fn__45981 invoke "text_test.clj" 55]
  [clojure.lang.AFn applyToHelper "AFn.java" 154]
  [clojure.lang.AFn applyTo "AFn.java" 144]
  [clojure.core$apply invokeStatic "core.clj" 657]
  [clojure.core$apply invoke "core.clj" 652]
  [clojure.test.check.properties$apply_gen$fn__45341$fn__45342 invoke "properties.cljc" 16]
  [clojure.test.check.properties$apply_gen$fn__45341 invoke "properties.cljc" 16]
  [clojure.test.check.rose_tree$fmap invokeStatic "rose_tree.cljc" 78]
  [clojure.test.check.rose_tree$fmap invoke "rose_tree.cljc" 74]
  [clojure.test.check.generators$fmap$fn__44484 invoke "generators.cljc" 89]
  [clojure.test.check.generators$gen_fmap$fn__44458 invoke "generators.cljc" 55]
  [clojure.test.check.generators$call_gen invokeStatic "generators.cljc" 41]
  [clojure.test.check.generators$call_gen invoke "generators.cljc" 38]
  [clojure.test.check$quick_check invokeStatic "check.cljc" 62]
  [clojure.test.check$quick_check doInvoke "check.cljc" 37]
  [clojure.lang.RestFn invoke "RestFn.java" 425]
  [slurper.text_test$eval46973 invokeStatic "form-init7761917284535158678.clj" 61]
  [slurper.text_test$eval46973 invoke "form-init7761917284535158678.clj" 61]
  [clojure.lang.Compiler eval "Compiler.java" 7005]
  [clojure.lang.Compiler eval "Compiler.java" 6968]
  [clojure.core$eval invokeStatic "core.clj" 3194]
  [clojure.core$eval invoke "core.clj" 3190]
  [clojure.main$repl$read_eval_print__8372$fn__8375 invoke "main.clj" 242]
  [clojure.main$repl$read_eval_print__8372 invoke "main.clj" 242]
  [clojure.main$repl$fn__8381 invoke "main.clj" 260]
  [clojure.main$repl invokeStatic "main.clj" 260]
  [clojure.main$repl doInvoke "main.clj" 176]
  [clojure.lang.RestFn invoke "RestFn.java" 1523]
  [clojure.tools.nrepl.middleware.interruptible_eval$evaluate$fn__30698 invoke "interruptible_eval.clj" 87]
  [clojure.lang.AFn applyToHelper "AFn.java" 152]
  [clojure.lang.AFn applyTo "AFn.java" 144]
  [clojure.core$apply invokeStatic "core.clj" 657]
  [clojure.core$with_bindings_STAR_ invokeStatic "core.clj" 1970]
  [clojure.core$with_bindings_STAR_ doInvoke "core.clj" 1970]
  [clojure.lang.RestFn invoke "RestFn.java" 425]
  [clojure.tools.nrepl.middleware.interruptible_eval$evaluate invokeStatic "interruptible_eval.clj" 85]
  [clojure.tools.nrepl.middleware.interruptible_eval$evaluate invoke "interruptible_eval.clj" 55]
  [clojure.tools.nrepl.middleware.interruptible_eval$interruptible_eval$fn__30743$fn__30746 invoke "interruptible_eval.clj" 222]
  [clojure.tools.nrepl.middleware.interruptible_eval$run_next$fn__30738 invoke "interruptible_eval.clj" 190]
  [clojure.lang.AFn run "AFn.java" 22]
  [java.util.concurrent.ThreadPoolExecutor runWorker "ThreadPoolExecutor.java" 1142]
  [java.util.concurrent.ThreadPoolExecutor$Worker run "ThreadPoolExecutor.java" 617]
  [java.lang.Thread run "Thread.java" 745]]

  )

