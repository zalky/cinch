(ns cinch.spec-test
  (:require [cinch.spec :as s*]
            [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest testing is]]))

(s/def ::k-or-s
  (s/or :keyword keyword? :string string?))

(s/def ::k-or-s-cat
   (s/cat :keyword keyword? :string string?))

(s/def ::k-or-s-cat-conform-to
  (s*/conform-to
    ::k-or-s-cat
    (constantly :always)))

(deftest conform-to-test
  (testing "Basic conforming"
    (is (= (-> ::k-or-s
               (s*/conform-to first)
               (s/conform :x))
           :keyword))
    (is (= (-> ::k-or-s-cat
               (s*/conform-to (juxt :string :keyword))
               (s/conform [:x "y"]))
           ["y" :x]))

    (testing "Transparent errors"
      (is (= (s/explain-data ::k-or-s-cat-conform-to [:x 'y])
             {::s/problems [{:path [:string]
                             :pred `string?
                             :val  'y
                             :via  [::k-or-s-cat-conform-to
                                    ::k-or-s-cat]
                             :in   [1]}]
              ::s/spec     ::k-or-s-cat-conform-to
              ::s/value    [:x 'y]})))))

(s/def ::k-or-s-cat-non-conformer
  (s*/non-conformer ::k-or-s-cat))

(deftest non-conformer-test
  (testing "Basic non conforming"
    (is (= (-> ::k-or-s
               (s*/non-conformer)
               (s/conform :x))
           :x))
    (is (= (-> ::k-or-s-cat
               (s*/non-conformer)
               (s/conform [:x "y"]))
           [:x "y"])))

  (testing "Transparent errors"
    (is (= (s/explain-data ::k-or-s-cat-non-conformer [:x 'y])
           {::s/problems [{:path [:string]
                           :pred `string?
                           :val  'y
                           :via  [::k-or-s-cat-non-conformer
                                  ::k-or-s-cat]
                           :in   [1]}]
            ::s/spec     ::k-or-s-cat-non-conformer
            ::s/value    [:x 'y]}))))

(s/def ::ref-any
  (s*/any-cardinality ::s*/ref))

(def A (random-uuid))
(def B (random-uuid))
(def C (random-uuid))

(deftest cardinality-test
  (testing "ref conform basic"
    (is (= (s/conform ::ref-any nil)
           ::s/invalid))
    (is (= (s/conform ::ref-any [])
           '()))
    (is (= (s/conform ::ref-any [nil])
           ::s/invalid))
    (is (= (s/conform ::ref-any :bad-ref)
           ::s/invalid))
    (is (= (s/conform ::ref-any [:bad-ident A])
           ::s/invalid))
    (is (= (s/conform ::ref-any [:system/uuid A])
           [:system/uuid A])))

  (testing "ref conform from"
    (testing "cardinality one"
      (testing "uuid"
        (is (= (s/conform ::ref-any A)
               ::s/invalid)))

      (testing "entity"
        (is (= (s/conform ::ref-any {:system/uuid A
                                     :kr/type     :entity})
               ::s/invalid))))

    (testing "cardinality many"
      (testing "ref"
        (is (= (s/conform ::ref-any [[:system/uuid A]
                                     [:system/uuid B]])
               [[:system/uuid A]
                [:system/uuid B]])))

      (testing "uuid"
        (is (= (s/conform ::ref-any [A B])
               ::s/invalid)))

      (testing "entity"
        (is (= (s/conform ::ref-any [{:system/uuid A
                                      :kr/type     :entity}
                                     {:system/uuid B
                                      :kr/type     :entity}])
               ::s/invalid)))

      (testing "with heterogenous"
        (is (= (s/conform ::ref-any [A
                                     [:system/uuid B]
                                     {:system/uuid C
                                      :kr/type     :entity}])
               ::s/invalid))))))

(s/def ::ref-coerce-many
  (s*/any-cardinality ::s*/ref :coerce-many true))

(deftest coerce-many-cardinality-test
  (testing "ref coerce many"
    (is (= (s/conform ::ref-coerce-many nil)
           ::s/invalid))
    (is (= (s/conform ::ref-coerce-many [])
           '()))
    (is (= (s/conform ::ref-coerce-many [nil])
           ::s/invalid))
    (is (= (s/conform ::ref-coerce-many :bad-ref)
           ::s/invalid))
    (is (= (s/conform ::ref-coerce-many [:bad-ident A])
           ::s/invalid))
    (is (= (s/conform ::ref-coerce-many A)
           ::s/invalid))
    (is (= (s/conform ::ref-coerce-many {:system/uuid A})
           ::s/invalid))
    (is (= (s/conform ::ref-coerce-many [:system/uuid A])
           [[:system/uuid A]]))
    (is (= (s/conform ::ref-coerce-many [[:system/uuid A]
                                         [:system/uuid B]])
           [[:system/uuid A]
            [:system/uuid B]]))))
