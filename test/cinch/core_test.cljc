(ns cinch.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [cinch.core :as util])
  #?(:cljs (:require-macros [cinch.core-test :refer [parse-ex]])))

(deftest test-derive-pairs
  (testing "Derive pairs into hierarchy"
    (is (= (util/derive-pairs nil)
           {:parents {}, :descendants {}, :ancestors {}}))
    (is (= (util/derive-pairs [])
           {:parents {}, :descendants {}, :ancestors {}}))
    (is (= (util/derive-pairs [:a :b])
           (-> (make-hierarchy)
               (derive :a :b))))
    (is (= (util/derive-pairs [:a :b :b :c])
           (-> (make-hierarchy)
               (derive :a :b)
               (derive :b :c))))
    (is (= (util/derive-pairs [[:a :b] :c])
           (-> (make-hierarchy)
               (derive :a :c)
               (derive :b :c))))
    (is (= (util/derive-pairs [:a [:b :c]])
           (-> (make-hierarchy)
               (derive :a :b)
               (derive :a :c))))
    (is (= (util/derive-pairs [[:a :b] [:c :d]])
           (-> (make-hierarchy)
               (derive :a :c)
               (derive :a :d)
               (derive :b :c)
               (derive :b :d)))))

  (testing "Error on self derive and cyclical derivation"
    (is (thrown? #?(:clj AssertionError :cljs js/Error)
                 (util/derive-pairs [:a :a])))
    (is (thrown? #?(:clj AssertionError :cljs js/Error)
                 (util/derive-pairs [[:a :b] [:b :a]])))
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (util/derive-pairs [:a :b :b :a])))
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (util/derive-pairs [:a :b :b :c :a :c])))))

(deftest test-merge-hierarchies
  (testing "Hierarchy merge basics"
    (is (= (util/merge-hierarchies nil nil)
           nil))
    (is (= (util/merge-hierarchies (make-hierarchy) nil)
           {:parents {}, :descendants {}, :ancestors {}}))
    (is (= (util/merge-hierarchies (make-hierarchy) (make-hierarchy))
           {:parents {}, :descendants {}, :ancestors {}})))

  (testing "Hierarchy merge derives"
    (is (= (util/merge-hierarchies (-> (make-hierarchy) (derive :a :b))
                                   (-> (make-hierarchy) (derive :c :d)))
           (-> (make-hierarchy)
               (derive :a :b)
               (derive :c :d))))
    (is (= (util/merge-hierarchies (-> (make-hierarchy) (derive :a :b))
                                   (-> (make-hierarchy) (derive :b :c)))
           (-> (make-hierarchy)
               (derive :a :b)
               (derive :b :c))))
    (is (= (util/merge-hierarchies (-> (make-hierarchy) (derive :a :b))
                                   (-> (make-hierarchy) (derive :c :a)))
           (-> (make-hierarchy)
               (derive :a :b)
               (derive :c :a))))
    (is (= (util/merge-hierarchies (-> (make-hierarchy) (derive :a :b))
                                   (-> (make-hierarchy) (derive :c :b)))
           (-> (make-hierarchy)
               (derive :a :b)
               (derive :c :b))))
    (is (= (util/merge-hierarchies (-> (make-hierarchy) (derive :a :b))
                                   (-> (make-hierarchy) (derive :a :c)))
           (-> (make-hierarchy)
               (derive :a :b)
               (derive :a :c)))))

  (testing "Cyclic derivations"
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (util/merge-hierarchies
                  (-> (make-hierarchy) (derive :a :b))
                  (-> (make-hierarchy) (derive :b :a)))))
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (util/merge-hierarchies
                  (-> (make-hierarchy) (derive :a :b) (derive :b :c))
                  (-> (make-hierarchy) (derive :c :a)))))))

;; Property based tests

(defn- derive-ex-type
  [e]
  (condp re-find (str e)
    #"Cyclic derivation"       :cyclic-derive
    #"not= tag parent"         :tag-equal-to-parent
    #"already has.+ ancestor" :already-has-ancestor
    (throw e)))

#?(:clj
   (defmacro ^:private parse-ex
     [& body]
     `(try
        ~@body
        (catch ~(if (:ns &env)
                  'js/Error
                  'Throwable)
            e#
          (derive-ex-type e#)))))

(defn- valid-pairs?
  [x]
  (try
    (util/derive-pairs x) x
    (catch #?(:clj Throwable
              :cljs js/Error)
        e
      (derive-ex-type e)
      nil)))

(defn pair-gen
  []
  (let [el (gen/elements (gen/sample gen/keyword 50))
        n  100]
    (as-> el %
      (gen/vector-distinct % {:max-elements 5})
      (gen/one-of [el %])
      (gen/vector % 0 20)
      (gen/such-that valid-pairs? % n)
      (gen/such-that (comp even? count) % n)
      (gen/such-that not-empty % n))))

(defspec merge-hierarchies-equals-combined #?(:clj 10000 :cljs 1000)
  ;; Tests that the combined derived hierarchy is equal to the merge
  ;; of the two individual derived hierarchies. Note that while cyclic
  ;; derives are filtered out from each generated sequence, they can
  ;; still occur through the transitive relationships between the two
  ;; sequences. merge-hierarchies needs to handle these edge cases.
  (prop/for-all [x (pair-gen)
                 y (pair-gen)]
    (let [combined (concat x y)
          h1       (parse-ex (util/derive-pairs combined))
          h2       (parse-ex (util/merge-hierarchies
                              (util/derive-pairs x)
                              (util/derive-pairs y)))]
      (or (= h1 h2)
          ;; The semantics of this test are confused by the fact that
          ;; derived hierarchies are fundamentally unordered, whereas
          ;; a sequence of derives is not.`clojure.core/derive` is not
          ;; commutative: it will throw an error if the child already
          ;; has the given parent as an ancestors. However, it is
          ;; always possible to re-order such a sequence of derives
          ;; into a valid one, assuming that there are also no
          ;; cyclical dependencies (these do not depend on derive
          ;; order). Until we implement the algorithm to re-order
          ;; derives into a valid sequence, we drop the subset of
          ;; tests where a derive-pairs expressions produces
          ;; an :already-has-ancestor error.
          (= h1 :already-has-ancestor)
          (= h2 :already-has-ancestor)))))

(defspec merge-hierarchy-commutative #?(:clj 10000 :cljs 1000)
  (prop/for-all [x (pair-gen)
                 y (pair-gen)]
    (let [x (util/derive-pairs x)
          y (util/derive-pairs y)]
      (= (parse-ex (util/merge-hierarchies x y))
         (parse-ex (util/merge-hierarchies y x))))))

(defspec merge-hierarchy-associative #?(:clj 10000 :cljs 1000)
  (prop/for-all [x (pair-gen)
                 y (pair-gen)
                 z (pair-gen)]
    (let [x (util/derive-pairs x)
          y (util/derive-pairs y)
          z (util/derive-pairs z)]
      (= (parse-ex (util/merge-hierarchies x (util/merge-hierarchies y z)))
         (parse-ex (util/merge-hierarchies (util/merge-hierarchies x y) z))))))

