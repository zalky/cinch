(ns cinch.core
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            #?(:clj [clojure.java.io :as io]))
  #?(:clj (:import java.lang.management.ManagementFactory)))

(defn seqify
  "Coerces x into vector if isn't already sequential."
  [x]
  (if (or (sequential? x) (set? x))
    x
    [x]))

(defn setify
  "Coerces x into set if isn't already."
  [x]
  (if (set? x) x #{x}))

(defn conjs
  "Like conj but always set."
  [set & args]
  (apply conj (or set #{}) args))

(defn conjv
  "Like conj but always vec."
  [v & args]
  (apply conj (or v []) args))

#?(:clj
   (defn queue
     ([]
      (clojure.lang.PersistentQueue/EMPTY))
     ([& xs]
      (apply conj (queue) xs))))

(defn qonj
  "Enqueues x, popping if queue size exceeds n."
  [q n x]
  (cond
    (nil? q)        #?(:cljs #queue [x] :clj (queue x))
    (= n (count q)) (conj (pop q) x)
    :else           (conj q x)))

(defn conjs-any
  "Given two values, performs to-many conj."
  [v1 v2]
  (cond
    (set? v1) (conj v1 v2)
    (nil? v1) v2
    :else     (hash-set v1 v2)))

(defn assoc-nil
  "assoc iff existing value is `nil`, or not in map."
  [m attr v]
  (update m attr #(if (some? %) % v)))

(defn update-contains
  "Only update if value exists at the given attr."
  [m attr f & args]
  (if (contains? m attr)
    (apply update m attr f args)
    m))

(defn map-all
  "Like map but exhausts all colls."
  [f pad & colls]
  (letfn [(pick [xs]
            (if (seq xs)
              (first xs)
              pad))]
    (lazy-seq
     (when (some seq colls)
       (cons
        (apply f (map pick colls))
        (apply map-all f pad (map rest colls)))))))

(defn mapcat-lazy
  "Fully lazy version of mapcat."
  [f coll]
  (for [x  coll
        x* (f x)]
    x*))

(defn merge-deep
  "Like merge, but merges recusively. Maps are merged via merge
  semantics, and vectors are merged positionally."
  [& args]
  (letfn [(m [& args]
            (if (every? map? args)
              (apply merge-with m args)
              (if (every? sequential? args)
                (apply map-all (partial merge-with m) nil args)
                (last args))))]
    (apply m (remove nil? args))))

(defn remove-nth
  "If you really must, removes the nth element of a vector. But consider
  using a hashed map instead."
  [coll n]
  (vec (concat (subvec coll 0 n)
               (subvec coll (inc n)))))

(s/def ::derive-pairs
  (s/* (s/cat :child (s/alt :one keyword?
                            :many (s/coll-of keyword?))
              :parent keyword?)))

(defn derive-pairs
  "Given a seq of child - parent pairs, [c1 p1 c2 p2 ...], derives them
  into the given hierarchy, or into a new one if not supplied. The
  child and parent can be cardinality many, and will be distributed
  combinatorially."
  ([xs]
   (derive-pairs (make-hierarchy) xs))
  ([h xs]
   (s/assert ::derive-pairs xs)
   (letfn [(lift [[c p]]
             (for [c* (seqify c)
                   p* (seqify p)]
               [c* p*]))]
     (->> xs
          (partition 2)
          (mapcat lift)
          (reduce (partial apply derive) h)))))

(defn- simple-merge-hierarchies
  [& h]
  (apply merge-with (partial merge-with set/union) h))

(defn- check-cyclic-derivation
  [{a :ancestors :as h}]
  (some
   (fn [[k1 family]]
     (some
      (fn [k2]
        (when (contains? (get a k2) k1)
          (throw (ex-info "Cyclic derivation"
                          {:ids [k1 k2]}))))
      family))
   a)
  h)

(defn- extrapolate-derives
  [{p   :parents
    a   :ancestors
    d   :descendants}]
  (loop [h          (make-hierarchy)
         [c & more] (remove d (keys a))]
    (letfn [(f [h p]
              (if (get (ancestors h c) p)
                (update-in h [:parents c] conjs p)
                (derive h c p)))]
      (if c
        (if-let [parents (get p c)]
          (recur
           (reduce f h parents)
           (vec (concat more parents)))
          (recur h more))
        h))))

(defn merge-hierarchies
  "Merges hierarchies, extrapolating transitive relationships,
  and checking for cyclical derivations."
  [& h]
  (some-> (apply simple-merge-hierarchies h)
          (check-cyclic-derivation)
          (extrapolate-derives)))

(defn descendants+
  [h x]
  (when x
    (conj (or (descendants h x) #{}) x)))

(defn ancestors+
  [h x]
  (when x
    (conj (or (ancestors h x) #{}) x)))

(defn split-keys
  "Given a map, and a set of key vectors, returns a seq of maps
  corresponding to a select-keys with each key vector. A map
  containing the remaining elements that were not selected is returned
  as the final element of the sequence."
  [m & ks]
  (conj (mapv #(select-keys m %) ks)
        (apply dissoc m (flatten ks))))

(defn index-comparator
  "Given a vector, returns a comparator by element indices."
  [v]
  (fn [x y]
    (let [ix (.indexOf v x)
          iy (.indexOf v y)]
      (if (or (neg? ix) (neg? iy))
        (throw
         (ex-info
          "value not in comparator vector"
          {:comparator v
           :value      (cond
                         (neg? ix) x
                         (neg? iy) y)}))
        (< ix iy)))))

#?(:clj
   (defn get-process-name
     "Returns java process name. On most systems it will be of the form
  pid@user."
     []
     (.getName (ManagementFactory/getRuntimeMXBean))))

#?(:clj
   (defn get-process-pid
     "Returns java process pid."
     []
     (some->> (get-process-name)
              (re-find #"(\d{1,6})@\w+")
              second)))

#?(:clj
   (defn get-source-dirs
     "Get project source paths from classpath."
     []
     (let [sep       (System/getProperty "path.separator")
           classpath (System/getProperty "java.class.path")]
       (->> (str/split classpath (re-pattern sep))
            (remove (partial re-find #".jar$"))
            (map #(.getCanonicalPath (io/file %)))
            (filter #(.isDirectory (io/file %)))))))
