(ns jsonschema.type-system.simplify-test
  (:use clojure.test
        roxxi.utils.collections
        jsonschema.type-system.types)
  (:require [jsonschema.type-system.merge-common :as m]
            [jsonschema.type-system.extract :refer [extract-type-simplifying]]
            [jsonschema.type-system.simplify :refer [type-simplifier
                                                     simplify-types]]))

;; # Generic Helpers for all tests
;;
;; This may look a lot like merge-test but the key thing here is
;; we're testing the type-simplifier in simplify.clj
;; As such, many of the tests are the same (if not similar)
;; but the expected outcomes are different.
;;
(def merger (type-simplifier))
(def extract-type extract-type-simplifying)

(defmacro merged-is
  ([x1 x2 result]
     `(is (= (m/type-merge merger ~x1 ~x2) ~result)))
  ([x1 x2 result comment]
     `(is (= (m/type-merge merger ~x1 ~x2) ~result) comment)))

;; # Canonical Examples of data types
;;
;; ## Canonical scalar expressions
;;
(def scalar-expressions
  [nil true "Hello" 6 "date(someday)" 3.14])

;; ### Generate scalar types from example expressions
;;
(defn generate-type-name=>type [scalar-exprs]
  (extract-map scalar-exprs
               :xform #(extract-type %)
               :key-extractor #(getType %)))

(def scalar-types
  {:null #jsonschema.type_system.types.Null{},
   :bool #jsonschema.type_system.types.Bool{},
   :str #jsonschema.type_system.types.Str{:min 5, :max 5},
   :int #jsonschema.type_system.types.Int{:min 6, :max 6},
   :date #jsonschema.type_system.types.Date{:format nil},
   :real #jsonschema.type_system.types.Real{:min 3.14, :max 3.14}})

;; ## Canonical Collection examples
;;
(def collection-expressions
  [{:name "empty" :coll []}
   {:name "single" :coll ["string" "string" "string" "string"]}
   {:name "mixed" :coll [nil true "Hello" 6 "date(someday)" 10.209]}
   {:name "nested" :coll [[1 2 3] [4 5 6 7] [8 9 2]]}
   {:name "mixed-nested" :coll [ ["string" "string" "string" "string"] [8 9 2] [nil nil] ]}
   {:name "nested-mixed" :coll [ [1 "str" nil] [1 "str" nil] [1 "str" nil] ]}
   {:name "mixed-nested-mixed" :coll [ [1 "str" nil] [1 "str"] ["date(date)" "str" nil] ]}
   {:name "empty-nested" :coll [ [ [] [] [] ] [ [] [] ] [ [] 5 [] ] ] }
   ])

(defn generate-name=>coll-type [coll-exprs]
  (extract-map coll-exprs
               :value-extractor #(extract-type (:coll %))
               :key-extractor #(keyword (:name %))))


(def coll-types
  {:empty #jsonschema.type_system.types.Collection{:coll-of :nothing},
   :single #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Str{:min 6, :max 6}},
   :mixed #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Null{} #jsonschema.type_system.types.Bool{} #jsonschema.type_system.types.Str{:min 5, :max 5} #jsonschema.type_system.types.Int{:min 6, :max 6} #jsonschema.type_system.types.Real{:min 10.209, :max 10.209} #jsonschema.type_system.types.Date{:format nil}}}},
   :nested #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Int{:min 1, :max 9}}},
   :mixed-nested #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Null{}} #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Int{:min 2, :max 9}} #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Str{:min 6, :max 6}}}}},
   :nested-mixed #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Null{} #jsonschema.type_system.types.Int{:min 1, :max 1} #jsonschema.type_system.types.Str{:min 3, :max 3}}}}},
   :mixed-nested-mixed #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Null{} #jsonschema.type_system.types.Int{:min 1, :max 1} #jsonschema.type_system.types.Str{:min 3, :max 3} #jsonschema.type_system.types.Date{:format nil}}}}},
   :empty-nested #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Int{:min 5, :max 5} #jsonschema.type_system.types.Collection{:coll-of :nothing}}}}}})

;; ## Canonical Document expressions
;; NOTE: the :name field is used to identify the document schema
;; and the document schema will include the :name field
(def document-expressions
  [{:name "scalars" :a "simple" :b nil :c 25 :d true :e "date(someday)" :f 10.4}
   {:name "flat-collection" :a ["item1" "item2" "item3"]}
   {:name "flat-mixed-collection" :a ["item1" 25 nil]}
   {:name "flat-nested-collection" :a [34 25 46 ["date(someday)" "date(someotherday)"]]}
   {:name "flat-nested-mixed-collection" :a [34 25 46 ["date(sos)" "date(so)" "hello"]]}
   {:name "subdoc" :a {:a-collection ["item1" "item2" "item3" "longer-item"]}}
   {:name "coll-subdoc" :a [ {:a ["item1" "item2" "item3"]}
                             {:a [1 2 3]}
                             {:a [4 5 6]}
                             {:a [7 8 nil]}
                             {:a [1 2 3]
                              :b "not-a-collection"}]}])

;; ### Generate document types from example expressions
(defn generate-name=>doc-type [doc-exprs]
  (extract-map doc-exprs
               :value-extractor #(extract-type %)
               :key-extractor #(keyword (:name %))))

(def empty-document-type #jsonschema.type_system.types.Document{:properties [], :map {}})

(def doc-types
  {:scalars #jsonschema.type_system.types.Document{:properties #{:a :name :c :b :f :d :e}, :map {:a #jsonschema.type_system.types.Str{:min 6, :max 6}, :name #jsonschema.type_system.types.Str{:min 7, :max 7}, :c #jsonschema.type_system.types.Int{:min 25, :max 25}, :b #jsonschema.type_system.types.Null{}, :f #jsonschema.type_system.types.Real{:min 10.4, :max 10.4}, :d #jsonschema.type_system.types.Bool{}, :e #jsonschema.type_system.types.Date{:format nil}}},
   :flat-collection #jsonschema.type_system.types.Document{:properties #{:a :name}, :map {:a #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Str{:min 5, :max 5}}, :name #jsonschema.type_system.types.Str{:min 15, :max 15}}},
   :flat-mixed-collection #jsonschema.type_system.types.Document{:properties #{:a :name}, :map {:a #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Null{} #jsonschema.type_system.types.Str{:min 5, :max 5} #jsonschema.type_system.types.Int{:min 25, :max 25}}}}, :name #jsonschema.type_system.types.Str{:min 21, :max 21}}},
   :flat-nested-collection #jsonschema.type_system.types.Document{:properties #{:a :name}, :map {:a #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Int{:min 25, :max 46} #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Date{:format nil}}}}}, :name #jsonschema.type_system.types.Str{:min 22, :max 22}}},
   :flat-nested-mixed-collection #jsonschema.type_system.types.Document{:properties #{:a :name}, :map {:a #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Int{:min 25, :max 46} #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Str{:min 5, :max 5} #jsonschema.type_system.types.Date{:format nil}}}}}}}, :name #jsonschema.type_system.types.Str{:min 28, :max 28}}},
   :subdoc #jsonschema.type_system.types.Document{:properties #{:a :name}, :map {:a #jsonschema.type_system.types.Document{:properties #{:a-collection}, :map {:a-collection #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Str{:min 5, :max 11}}}}, :name #jsonschema.type_system.types.Str{:min 6, :max 6}}},
   :coll-subdoc #jsonschema.type_system.types.Document{:properties #{:a :name}, :map {:a #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Document{:properties #{:a :b}, :map {:b #jsonschema.type_system.types.Str{:min 16, :max 16}, :a #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Null{} #jsonschema.type_system.types.Int{:min 1, :max 8} #jsonschema.type_system.types.Str{:min 5, :max 5}}}}}}}, :name #jsonschema.type_system.types.Str{:min 11, :max 11}}}})


;; # Scalar Merge Tests
;;
;; ## Scalar - Scalar
;;
;; ### Test Generation
;;
;; Generates scalar-scalar union map
(defn generate-scalar-scalar-unions [scalar-types]
  (persistent!
   (loop [outers (seq scalar-types)
          the-map (transient {})]
       (if (empty? outers)
         the-map
         (recur (rest outers)
                (loop [inners (seq scalar-types)
                       inner-map the-map]
                  (if (empty? inners)
                    inner-map
                    (recur (rest inners)
                           (let [[o-key o-val] (first outers)
                                 [i-key i-val]  (first inners)]
                             (if (= o-key i-key)
                               inner-map
                               (let [the-key (keyword (str (name o-key) "-" (name i-key)))
                                     the-val (make-union-with o-val i-val)]
                                 (assoc! inner-map the-key the-val))))))))))))

(def scalar-scalar-unions
  {:int-null #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Null{} #jsonschema.type_system.types.Int{:min 6, :max 6}}},
   :real-int #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Real{:min 3.14, :max 3.14} #jsonschema.type_system.types.Int{:min 6, :max 6}}},
   :null-int #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Null{} #jsonschema.type_system.types.Int{:min 6, :max 6}}},
   :date-bool #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Bool{} #jsonschema.type_system.types.Date{:format nil}}},
   :date-null #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Null{} #jsonschema.type_system.types.Date{:format nil}}},
   :date-real #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Real{:min 3.14, :max 3.14} #jsonschema.type_system.types.Date{:format nil}}},
   :date-str #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Str{:min 5, :max 5} #jsonschema.type_system.types.Date{:format nil}}},
   :bool-int #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Bool{} #jsonschema.type_system.types.Int{:min 6, :max 6}}},
   :bool-null #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Bool{} #jsonschema.type_system.types.Null{}}},
   :null-date #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Null{} #jsonschema.type_system.types.Date{:format nil}}},
   :int-bool #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Bool{} #jsonschema.type_system.types.Int{:min 6, :max 6}}},
   :int-real #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Real{:min 3.14, :max 3.14} #jsonschema.type_system.types.Int{:min 6, :max 6}}},
   :bool-date #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Bool{} #jsonschema.type_system.types.Date{:format nil}}},
   :real-null #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Null{} #jsonschema.type_system.types.Real{:min 3.14, :max 3.14}}},
   :null-str #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Null{} #jsonschema.type_system.types.Str{:min 5, :max 5}}},
   :real-str #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Real{:min 3.14, :max 3.14} #jsonschema.type_system.types.Str{:min 5, :max 5}}},
   :str-bool #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Bool{} #jsonschema.type_system.types.Str{:min 5, :max 5}}},
   :date-int #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Int{:min 6, :max 6} #jsonschema.type_system.types.Date{:format nil}}},
   :str-real #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Real{:min 3.14, :max 3.14} #jsonschema.type_system.types.Str{:min 5, :max 5}}},
   :str-int #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Str{:min 5, :max 5} #jsonschema.type_system.types.Int{:min 6, :max 6}}},
   :real-bool #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Bool{} #jsonschema.type_system.types.Real{:min 3.14, :max 3.14}}},
   :int-str #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Int{:min 6, :max 6} #jsonschema.type_system.types.Str{:min 5, :max 5}}},
   :bool-real #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Bool{} #jsonschema.type_system.types.Real{:min 3.14, :max 3.14}}},
   :int-date #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Int{:min 6, :max 6} #jsonschema.type_system.types.Date{:format nil}}},
   :str-date #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Str{:min 5, :max 5} #jsonschema.type_system.types.Date{:format nil}}},
   :bool-str #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Bool{} #jsonschema.type_system.types.Str{:min 5, :max 5}}},
   :str-null #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Null{} #jsonschema.type_system.types.Str{:min 5, :max 5}}},
   :null-bool #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Null{} #jsonschema.type_system.types.Bool{}}},
   :real-date #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Real{:min 3.14, :max 3.14} #jsonschema.type_system.types.Date{:format nil}}},
   :null-real #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Null{} #jsonschema.type_system.types.Real{:min 3.14, :max 3.14}}}})

;; ### The actual scalar-scalar union tests
;;
;; Using the maps above for validation
;; this creates the references to the test values and the supposed result
;; so we don't have to type it all out by hand.
;;
(defmacro is-scalar-scalar-merge
  [lhs rhs]
  `(merged-is (scalar-types ~lhs) (scalar-types ~rhs)
              ~(if (= lhs rhs)
                 `(scalar-types ~rhs)
                 `(scalar-scalar-unions ~(keyword (str (name lhs) "-" (name rhs)))))))

;; This creates the cross product of scalar merging test. Currently, we have 6 kinds-
;; so this generates 36 tests
(defmacro gen-scalar-scalar-test [scalars]
  `(do
     ~@(loop [outers scalars
              ls '()]
         (if (empty? outers)
           ls
           (recur (rest outers)
                  (loop [inners scalars
                         ils ls]
                    (if (empty? inners)
                      ils
                      (recur (rest inners)
                             (cons
                              `(is-scalar-scalar-merge ~(first outers) ~(first inners))
                              ils)))))))))

(deftest scalar-tests
  (testing "Scalar - Scalar"
    (testing "Tests if the following conditions are true:
            1. If we attempt to merge two scalar types that are the same,
               the result is an instance of either type.
            2. If we attempt to merge to a scalar with _non-equally typed_
               scalar then the result is a union of the two types.
            Regenerate these tests with
            (gen-scalar-scalar-test [:real :int :date :str :bool :null])."
      (do
        (is-scalar-scalar-merge :real :real)
        (is-scalar-scalar-merge :real :int)
        (is-scalar-scalar-merge :real :date)
        (is-scalar-scalar-merge :real :str)
        (is-scalar-scalar-merge :real :bool)
        (is-scalar-scalar-merge :real :null)
        (is-scalar-scalar-merge :int :real)
        (is-scalar-scalar-merge :int :int)
        (is-scalar-scalar-merge :int :date)
        (is-scalar-scalar-merge :int :str)
        (is-scalar-scalar-merge :int :bool)
        (is-scalar-scalar-merge :int :null)
        (is-scalar-scalar-merge :date :real)
        (is-scalar-scalar-merge :date :int)
        (is-scalar-scalar-merge :date :date)
        (is-scalar-scalar-merge :date :str)
        (is-scalar-scalar-merge :date :bool)
        (is-scalar-scalar-merge :date :null)
        (is-scalar-scalar-merge :str :real)
        (is-scalar-scalar-merge :str :int)
        (is-scalar-scalar-merge :str :date)
        (is-scalar-scalar-merge :str :str)
        (is-scalar-scalar-merge :str :bool)
        (is-scalar-scalar-merge :str :null)
        (is-scalar-scalar-merge :bool :real)
        (is-scalar-scalar-merge :bool :int)
        (is-scalar-scalar-merge :bool :date)
        (is-scalar-scalar-merge :bool :str)
        (is-scalar-scalar-merge :bool :bool)
        (is-scalar-scalar-merge :bool :null)
        (is-scalar-scalar-merge :null :real)
        (is-scalar-scalar-merge :null :int)
        (is-scalar-scalar-merge :null :date)
        (is-scalar-scalar-merge :null :str)
        (is-scalar-scalar-merge :null :bool)
        (is-scalar-scalar-merge :null :null))))


  (testing "Scalar - Collection / Document"
    (testing "Tests if we attempt to merge to a scalar with a document,
            or collection, then the result is a union of the two types."
      (merged-is (scalar-types :bool) (doc-types :scalars)
                 #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Bool{} #jsonschema.type_system.types.Document{:properties #{:a :name :c :b :f :d :e}, :map {:a #jsonschema.type_system.types.Str{:min 6, :max 6}, :name #jsonschema.type_system.types.Str{:min 7, :max 7}, :c #jsonschema.type_system.types.Int{:min 25, :max 25}, :b #jsonschema.type_system.types.Null{}, :f #jsonschema.type_system.types.Real{:min 10.4, :max 10.4}, :d #jsonschema.type_system.types.Bool{}, :e #jsonschema.type_system.types.Date{:format nil}}}}})
      (merged-is (scalar-types :bool) (coll-types :single)
                 #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Bool{} #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Str{:min 6, :max 6}}}})))

  (testing "Scalar - Union"
   (testing "If we attempt to merge a scalar with a union type,
             then we add the scalar to the union type if it is not already present,
             then return the union."
     (merged-is (scalar-types :null) (scalar-scalar-unions :bool-str)
                #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Null{} #jsonschema.type_system.types.Bool{} #jsonschema.type_system.types.Str{:min 5, :max 5}}})
     (merged-is (scalar-types :null) (scalar-scalar-unions :null-str)
                #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Str{:min 5, :max 5} #jsonschema.type_system.types.Null{}}})
     (merged-is (scalar-scalar-unions :null-str) (scalar-types :null)
                #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Str{:min 5, :max 5} #jsonschema.type_system.types.Null{}}}))))


;; # Document Tests

;; ## Document - Scalar / Collection
(deftest doc-scalar-coll-merging-tests
  (testing "Merging a document with any kind of scalar, or collection
            type results in a union of the two types."
    (merged-is (doc-types :scalars) (scalar-types :bool)
               #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Bool{} #jsonschema.type_system.types.Document{:properties #{:a :name :c :b :f :d :e}, :map {:a #jsonschema.type_system.types.Str{:min 6, :max 6}, :name #jsonschema.type_system.types.Str{:min 7, :max 7}, :c #jsonschema.type_system.types.Int{:min 25, :max 25}, :b #jsonschema.type_system.types.Null{}, :f #jsonschema.type_system.types.Real{:min 10.4, :max 10.4}, :d #jsonschema.type_system.types.Bool{}, :e #jsonschema.type_system.types.Date{:format nil}}}}})
    (merged-is (doc-types :flat-mixed-collection) (coll-types :mixed)
               #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Null{} #jsonschema.type_system.types.Bool{} #jsonschema.type_system.types.Str{:min 5, :max 5} #jsonschema.type_system.types.Int{:min 6, :max 6} #jsonschema.type_system.types.Real{:min 10.209, :max 10.209} #jsonschema.type_system.types.Date{:format nil}}}} #jsonschema.type_system.types.Document{:properties #{:a :name}, :map {:a #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Null{} #jsonschema.type_system.types.Str{:min 5, :max 5} #jsonschema.type_system.types.Int{:min 25, :max 25}}}}, :name #jsonschema.type_system.types.Str{:min 21, :max 21}}}}})))

;; ## Document - Incongruent Merging

(def merged-one-two-type
  #jsonschema.type_system.types.Document{:properties #{:a :b}, :map {:b #jsonschema.type_system.types.Str{:min 16, :max 16}, :a #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Null{} #jsonschema.type_system.types.Int{:min 1, :max 8}}}}}})

(deftest doc-incongruent-merging-tests
  (testing "Merging a document with any kind of **incongruent** document type
            results in a single document joining any common
            fields with a union (if appropriate)."
    (merged-is (extract-type {:a [7 8 nil]})
               (extract-type {:a [1 2 3] :b "not-a-collection"})
               merged-one-two-type)))

;; ## Document - Congruent Merging

(def congruent1
  (extract-type {:a "12"
                 :b "same type"
                 :c ["one" 2 "fire" true]
                 :d {:a "ten"
                     :b "same type"
                     :c ["hi" "how are you" "bye"]
                     :d {:inside "insider"}}
                 :e [1 2 3]}))

(def congruent2
  (extract-type {:a 12
                 :b "this"
                 :c [1 2 3]
                 :d {:a 10
                     :b "that"
                     :c ["hi" "how are you" "bye"]
                     :d {:inside "insider"}}
                 :e nil}))

(deftest doc-congruent-merging-tests
  (testing "When merging two documents that are **congruent**,
            create a new type where all of their property types are merged."

    (testing "This blocks tests if things will actually be merged"
      (merged-is congruent1 congruent2
                 #jsonschema.type_system.types.Document{:properties #{:a :c :b :d :e}, :map {:a #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Str{:min 2, :max 2} #jsonschema.type_system.types.Int{:min 12, :max 12}}}, :c #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Bool{} #jsonschema.type_system.types.Int{:min 1, :max 3} #jsonschema.type_system.types.Str{:min 3, :max 4}}}}, :b #jsonschema.type_system.types.Str{:min 4, :max 9}, :d #jsonschema.type_system.types.Document{:properties #{:a :c :b :d}, :map {:a #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Str{:min 3, :max 3} #jsonschema.type_system.types.Int{:min 10, :max 10}}}, :c #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Str{:min 2, :max 11}}, :b #jsonschema.type_system.types.Str{:min 4, :max 9}, :d #jsonschema.type_system.types.Document{:properties #{:inside}, :map {:inside #jsonschema.type_system.types.Str{:min 7, :max 7}}}}}, :e #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Null{} #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Int{:min 1, :max 3}}}}}}))

    (testing "This blocks tests if two things are congruent and have all the same types we'll get something that's equivalent to the original type of either document"
      (is (= (m/type-merge merger congruent1 congruent1) congruent1)))))


;; ## Document - Union

(def a-doc-type (extract-type {:a 1 :b "b"}))

(def union-with-no-doc
  (make-union-with (make-int 1) (make-str "a")))

(def union-with-incongruent-docs
  (make-union-with (make-int 1)
                   (extract-type {:a 1})
                   (extract-type {:a 1 :b "b" :c nil})))

(def union-with-congruent-doc
  (make-union-with (make-int 1)
                   (extract-type {:a "a" :b 1})))

(def union-with-equal-doc
  (make-union-with (make-int 1)
                   (extract-type {:a 1 :b "b"})))


(deftest doc-union-merging-tests
  (testing "When merging a document with a union,
            and the union contains no document types,
            then add this document type to the union"
    (merged-is a-doc-type union-with-no-doc
               #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Document{:properties #{:a :b}, :map {:a #jsonschema.type_system.types.Int{:min 1, :max 1}, :b #jsonschema.type_system.types.Str{:min 1, :max 1}}}
                                                               #jsonschema.type_system.types.Int{:min 1, :max 1}
                                                               #jsonschema.type_system.types.Str{:min 1, :max 1}}}))

  (testing "When merging a document with a union,
            every document in the union should get merged together"
    (merged-is a-doc-type union-with-incongruent-docs
               #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Int{:min 1, :max 1}
                                                               #jsonschema.type_system.types.Document{:properties #{:a :c :b}, :map {:a #jsonschema.type_system.types.Int{:min 1, :max 1}, :c #jsonschema.type_system.types.Null{}, :b #jsonschema.type_system.types.Str{:min 1, :max 1}}}}}))

  (testing "When merging a document with a union,
            and there exists exactly one document in the union with which this document is congruent with,
            then this document should be merged with the congruent document union"
    (merged-is a-doc-type union-with-congruent-doc
               #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Document{:properties #{:a :b}, :map {:a #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Int{:min 1, :max 1} #jsonschema.type_system.types.Str{:min 1, :max 1}}}, :b #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Str{:min 1, :max 1} #jsonschema.type_system.types.Int{:min 1, :max 1}}}}}
                                                               #jsonschema.type_system.types.Int{:min 1, :max 1}}}))

  (testing "When merging a document with a union,
            and union contains a document type that is equivalent to this one,
            then this is a no-op"
    (merged-is a-doc-type union-with-equal-doc
               union-with-equal-doc)))

;; # Collection Tests
;; # Collection - Scalar / Document / Collection / Union

(def coll-of-str (extract-type ["5" "6"]))
(def coll-of-num (extract-type [5 6]))
(def coll-of-null (extract-type [nil nil]))
(def union-of-coll-of-str-null
  (make-union-with coll-of-str coll-of-null))

;; (deftest collection-tests
;;   (testing "Collections follow all the rules of merging
;;             scalar types when merging with scalars,
;;             documents, collections, unions types."
;;     (testing "Scalars"
;;       (merged-is coll-of-str (scalar-types :null)
;;                  #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :null}
;;                                                            #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :string}}}}))
;;     (testing "Documents"
;;       (merged-is coll-of-str a-doc-type
;;                  #jsonschema.type_system.types.Union{:union-of
;;                                                #{#jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :string}}
;;                                                  #jsonschema.type_system.types.Document{:properties [:a :b],
;;                                                                                   :map {:a #jsonschema.type_system.types.Scalar{:type :int},
;;                                                                                         :b #jsonschema.type_system.types.Scalar{:type :string}}}}}))
;;     (testing "Collections"
;;       (testing "Identity"
;;         (merged-is coll-of-str coll-of-str
;;                    #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :string}}))
;;       (merged-is coll-of-num coll-of-str
;;                  #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :string}}
;;                                                            #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :int}}}}))
;;     (testing "Unions"
;;       (merged-is coll-of-str union-of-coll-of-str-null
;;                  #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :string}}
;;                                                            #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :null}}}})
;;       (merged-is coll-of-num union-of-coll-of-str-null
;;                  #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :string}}
;;                                                            #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :null}}
;;                                                            #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :int}}}}))))



;; # Union Tests

(deftest union-tests
  (testing "Identity: Unions always contain a unique set of types.
            (i.e. _if( u1 contains t1 and t1.equals(t2) )_ then
            _merge(u1, t2).equals(u1)_ is true"
    (let [some-types (map extract-type ["hello"  "hello" 5
                                        "hi" 5  [4 5 6]
                                        [7 8 9] "hi" [7 8 9]
                                        42 {:a "crazy"} {:a "hello"}])
          reversed-types (reverse some-types)
          merged-type (apply simplify-types some-types)
          reverse-merged-type (apply simplify-types reversed-types)]
      (is (= merged-type reverse-merged-type))
      (is (= merged-type
             #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Int{:min 5, :max 42}
                                                             #jsonschema.type_system.types.Str{:min 2, :max 5}
                                                             #jsonschema.type_system.types.Document{:properties #{:a}, :map {:a #jsonschema.type_system.types.Str{:min 5, :max 5}}}
                                                             #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Int{:min 4, :max 9}}}}))))

  (testing "Union - Union"
    (testing "If we attempt to merge two union types,
              the result is a union type containing a set
              containing the elements of
              1. The set of scalars amongst the two
              2. The set of collections amongst the two
              3. A single document that represents all
                 possible key=>value-type pairs possible"
      (let
          [some-types (map extract-type ["hello" 5 nil 10.5
                                         {:a "crazy"}
                                         {:a "hello"}
                                         {:a "hello"
                                          :b 5}
                                         {:a "hello"
                                          :b true}
                                         {:a 10.10
                                          :c 10}])
           other-types (map extract-type ["hello" 5 nil
                                         {:a "crazy"}
                                         {:a "hello"}
                                         {:a nil
                                          :b nil}
                                         {:a nil
                                          :b nil}
                                         {:x "x"
                                          :y "y"}])
           union1 (apply simplify-types some-types)
           union2 (apply simplify-types other-types)]
        (merged-is union1 union2
                   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Null{} #jsonschema.type_system.types.Int{:min 5, :max 5} #jsonschema.type_system.types.Str{:min 5, :max 5} #jsonschema.type_system.types.Real{:min 10.5, :max 10.5} #jsonschema.type_system.types.Document{:properties #{:y :x :a :c :b}, :map {:c #jsonschema.type_system.types.Int{:min 10, :max 10}, :x #jsonschema.type_system.types.Str{:min 1, :max 1}, :y #jsonschema.type_system.types.Str{:min 1, :max 1}, :b #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Bool{} #jsonschema.type_system.types.Null{} #jsonschema.type_system.types.Int{:min 5, :max 5}}}, :a #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Null{} #jsonschema.type_system.types.Str{:min 5, :max 5} #jsonschema.type_system.types.Real{:min 10.1, :max 10.1}}}}}}}))))

  (testing "Union - Document"
    (testing "Empty union with a document"
      (let [empty-union (make-union #{})]
        (merged-is empty-union a-doc-type
                   a-doc-type)))))
