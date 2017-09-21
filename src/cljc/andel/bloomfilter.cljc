(ns andel.bloomfilter
  (:refer-clojure :exclude [merge]))

(def default-bits (* 50 32))
(def default-hashes 2)

(defn create
  ([bits hashes]
   #?(:cljs (js/BloomFilter. bits hashes)
      :clj :bloomfilter))
  ([]
   (create default-bits default-hashes)))

(defn add! [bloom v]
  #?(:cljs (do
             (.add bloom v)
             bloom)
     :clj bloom))

(defn merge [f1 f2]
  #?(:cljs (do
             (assert (= (aget f1 "k") (aget f2 "k")))
             (assert (= (aget f1 "m") (aget f2 "m")))
             (let [b1 (aget f1 "buckets")
                   b2 (aget f2 "buckets")
                   a (js/Array.from b1)]
               (loop [i 0]
                 (when (< i (.-length a))
                   (aset a i (bit-or (aget b1 i) (aget b2 i)))
                   (recur (inc i))))
               (js/BloomFilter. a (aget f1 "k"))))
     :clj :bloomfilter))

(defn merge-many [filters]
  #?(:cljs
      (let [fs (nth filters 0)
            k (aget fs "k")
            m (aget fs "m")]
        (create
          (reduce (fn [a f]
                    (let [b (aget f "buckets")]
                      (loop [i 0]
                        (when (< i (.-length a))
                          (aset a i (bit-or (aget a i) (aget b i)))
                          (recur (inc i))))
                      a))
                  (js/Array.from (aget fs "buckets"))
                  filters)
         k))
     :clj :bloomfilter)
  )

(defn has? [bloom v]
  #?(:cljs (.test bloom v)
     :clj true))

(defn copy [bloom]
  #?(:cljs (js/BloomFilter. (aget bloom "buckets") (aget bloom "k"))
     :clj bloom))

(defn intersects? [f1 f2]
  #?(:cljs (let [b1 (aget f1 "buckets")
                 b2 (aget f2 "buckets")]
             (assert (= (aget f1 "m") (aget f2 "m")))
             (assert (= (aget f1 "k") (aget f2 "k")))
             (loop [i 0]
               (if (< i (.-length b1))
                 (if (identical? 0 (bit-and (aget b1 i) (aget b2 i)))
                   (recur (inc i))
                   true)
                 false)))
     :clj true))

;;

(defn log [x]
  #?(:clj  (Math/log x)
     :cljs (js/Math.log x)))

(defn ceil [x]
  #?(:clj  (Math/ceil x)
     :cljs (js/Math.ceil x)))

(defn pow [a b]
  #?(:clj  (Math/pow a b)
     :cljs (js/Math.pow a b)))


(defn optimal-m
  "returns optimal size of bit array"
  [cardinality fp-prob]
  (-> (* -1
        (/ (* cardinality (log fp-prob))
           (pow (log 2) 2)))
      (/ 32) ceil (* 32) long))

(defn optimal-k
  "returns optimal number of hashes"
  [cardinality m-bits]
  (-> (* (/ m-bits cardinality)
         (log 2))
      ceil long))


(comment
  (/ (optimal-m 1000 0.3) 32)
  (optimal-k 1000 (* 80 32))


  )

()