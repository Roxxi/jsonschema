(ns jsonschema.type-system.merge-test
  (:use clojure.test
        jsonschema.utils
        jsonschema.type-system.types
        jsonschema.type-system.extract
        jsonschema.type-system.merge))

;; # Important note
;; The code commented out at various points below is code used to generate data for unit tests.
;; Code in This commented out region is used to generate the data for validation
;; By running these commands in the repl, you can generate and regenerate the unit tests.
;; This does require that you manually verify that the generated data is correct
;; but it simplifies updating all the code by hand.


;; # Generic Helpers for all tests

(def merger (type-merger))
(def extractor (clojure-type-extractor))

(defmacro merged-is
  ([x1 x2 result]
     `(is (= (type-merge merger ~x1 ~x2) ~result)))
  ([x1 x2 result comment]
     `(is (= (type-merge merger ~x1 ~x2) ~result) comment)))

;; # Canonical Examples of data types
;;
;; ## Canonical scalar expressions

(def scalar-expressions
  [nil true "Hello" 6 "date(someday)" "id(something)"])

;; ### Generate scalar types from example expressions

(defn generate-type-name=>type [scalar-exprs]
  (extract-map scalar-exprs
               :xform #(extract extractor %)
               :key-extractor #(getType %)))

;; ## Canonical Collection examples

(def collection-expressions
  [{:name "empty" :coll []}
   {:name "single" :coll ["string" "string" "string" "string"]}
   {:name "mixed" :coll [nil true "Hello" 6 "date(someday)" "id(something)"]}
   {:name "nested" :coll [[1 2 3] [4 5 6 7] [8 9 2]]}
   {:name "mixed-nested" :coll [ ["string" "string" "string" "string"] [8 9 2] [nil nil] ]}
   {:name "nested-mixed" :coll [ [1 "str" nil] [1 "str" nil] [1 "str" nil] ]}
   {:name "mixed-nested-mixed" :coll [ [1 "str" nil] [1 "str"] ["id(id)" "str" nil] ]}
   {:name "empty-nested" :coll [ [ [] [] [] ] [ [] [] ] [ [] 5 [] ] ] }
   ])

(defn generate-name=>coll-type [coll-exprs]
  (extract-map coll-exprs
               :value-extractor #(extract extractor (:coll %))               
               :key-extractor #(keyword (:name %))))


(def coll-types
  {:empty #jsonschema.type_system.types.Collection{:coll-of :nothing},
   :single #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :string}},
   :mixed #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :null} #jsonschema.type_system.types.Scalar{:type :bool} #jsonschema.type_system.types.Scalar{:type :id} #jsonschema.type_system.types.Scalar{:type :number} #jsonschema.type_system.types.Scalar{:type :string} #jsonschema.type_system.types.Scalar{:type :date}}}},
   :nested #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :number}}},
   :mixed-nested #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :string}} #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :null}} #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :number}}}}},
   :nested-mixed #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :null} #jsonschema.type_system.types.Scalar{:type :number} #jsonschema.type_system.types.Scalar{:type :string}}}}},
   :mixed-nested-mixed #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :null} #jsonschema.type_system.types.Scalar{:type :id} #jsonschema.type_system.types.Scalar{:type :string}}}} #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :null} #jsonschema.type_system.types.Scalar{:type :number} #jsonschema.type_system.types.Scalar{:type :string}}}} #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :number} #jsonschema.type_system.types.Scalar{:type :string}}}}}}},
   :empty-nested #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Collection{:coll-of :nothing} #jsonschema.type_system.types.Scalar{:type :number}}}} #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Collection{:coll-of :nothing}}}}}})

;; ## Canonical Document expressions
;; NOTE: the :name field is used to identify the document schema
;; and the document schema will include the :name field
(def document-expressions
  [{:name "scalars" :a "simple" :b nil :c 25 :d true :e "date(someday)" :f "id(something)"}
   {:name "flat-collection" :a ["item1" "item2" "item3"]}
   {:name "flat-mixed-collection" :a ["item1" 25 nil]}
   {:name "flat-nested-collection" :a [34 25 46 ["date(someday)" "date(someotherday)"]]}
   {:name "flat-nested-mixed-collection" :a [34 25 46 ["date(sos)" "date(so)" "hello"]]}
   {:name "subdoc" :a {:a-collection ["item1" "item2" "item3"]}}
   {:name "coll-subdoc" :a [ {:a ["item1" "item2" "item3"]}
                             {:a [1 2 3]}
                             {:a [4 5 6]}
                             {:a [7 8 nil]}
                             {:a [1 2 3]
                              :b "not-a-collection"}]}
   {:name "less-scalars" :a "simple" :b nil :c 25 :d true}])

;; ### Generate document types from example expressions
(defn generate-name=>doc-type [doc-exprs]
  (extract-map doc-exprs
               :value-extractor #(extract extractor %)
               :key-extractor #(keyword (:name %))))

(def empty-document-type #jsonschema.type_system.types.Document{:properties [], :map {}})

(def doc-types
  {:scalars
   #jsonschema.type_system.types.Document{:properties [:a :name :c :b :f :d :e],
                                    :map {:a #jsonschema.type_system.types.Scalar{:type :string},
                                          :name #jsonschema.type_system.types.Scalar{:type :string},
                                          :c #jsonschema.type_system.types.Scalar{:type :number},
                                          :b #jsonschema.type_system.types.Scalar{:type :null},
                                          :f #jsonschema.type_system.types.Scalar{:type :id},
                                          :d #jsonschema.type_system.types.Scalar{:type :bool},
                                          :e #jsonschema.type_system.types.Scalar{:type :date}}},
   :flat-collection
   #jsonschema.type_system.types.Document{:properties [:a :name],
                                    :map {:a #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :string}},
                                          :name #jsonschema.type_system.types.Scalar{:type :string}}},
   :flat-mixed-collection
   #jsonschema.type_system.types.Document{:properties [:a :name],
                                    :map {:a #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :null}
                                                                                                                                   #jsonschema.type_system.types.Scalar{:type :number}
                                                                                                                                   #jsonschema.type_system.types.Scalar{:type :string}}}},
                                          :name #jsonschema.type_system.types.Scalar{:type :string}}},
   :flat-nested-collection
   #jsonschema.type_system.types.Document{:properties [:a :name],
                                    :map {:a #jsonschema.type_system.types.Collection{:coll-of
                                                                                #jsonschema.type_system.types.Union{:union-of
                                                                                                              #{#jsonschema.type_system.types.Collection{:coll-of
                                                                                                                                                   #jsonschema.type_system.types.Scalar{:type :date}}
                                                                                                                #jsonschema.type_system.types.Scalar{:type :number}}}},
                                          :name #jsonschema.type_system.types.Scalar{:type :string}}},
   :flat-nested-mixed-collection
   #jsonschema.type_system.types.Document{:properties [:a :name],
                                    :map {:a #jsonschema.type_system.types.Collection{:coll-of
                                                                                #jsonschema.type_system.types.Union{:union-of
                                                                                                              #{#jsonschema.type_system.types.Scalar{:type :number}
                                                                                                                #jsonschema.type_system.types.Collection{:coll-of
                                                                                                                                                   #jsonschema.type_system.types.Union{:union-of
                                                                                                                                                                                 #{#jsonschema.type_system.types.Scalar{:type :string}
                                                                                                                                                                                   #jsonschema.type_system.types.Scalar{:type :date}}}}}}},
                                          :name #jsonschema.type_system.types.Scalar{:type :string}}},
   :subdoc
   #jsonschema.type_system.types.Document{:properties [:a :name],
                                    :map {:a #jsonschema.type_system.types.Document{:properties [:a-collection],
                                                                              :map {:a-collection
                                                                                    #jsonschema.type_system.types.Collection{:coll-of
                                                                                                                       #jsonschema.type_system.types.Scalar{:type :string}}}},
                                          :name #jsonschema.type_system.types.Scalar{:type :string}}},
   :coll-subdoc
   #jsonschema.type_system.types.Document{:properties [:a :name],
                                    :map {:a #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Document{:properties [:a :b],
                                                                                                                                                                    :map {:a #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :number}},
                                                                                                                                                                          :b #jsonschema.type_system.types.Scalar{:type :string}}} #jsonschema.type_system.types.Document{:properties [:a],
                                                                                                                                                                                                                                                              :map {:a #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :string}}}} #jsonschema.type_system.types.Document{:properties [:a],
                                                                                                                                                                                                                                                                                                                                                                                                     :map {:a #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :number}}}} #jsonschema.type_system.types.Document{:properties [:a],
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            :map {:a #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :null} #jsonschema.type_system.types.Scalar{:type :number}}}}}}}}},
                                          :name #jsonschema.type_system.types.Scalar{:type :string}}},
   :less-scalars
   #jsonschema.type_system.types.Document{:properties [:a :name :c :b :d],
                                    :map {:a #jsonschema.type_system.types.Scalar{:type :string},
                                          :name #jsonschema.type_system.types.Scalar{:type :string},
                                          :c #jsonschema.type_system.types.Scalar{:type :number},
                                          :b #jsonschema.type_system.types.Scalar{:type :null},
                                          :d #jsonschema.type_system.types.Scalar{:type :bool}}}})


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


(def scalar-types
  {:null #jsonschema.type_system.types.Scalar{:type :null},
   :bool #jsonschema.type_system.types.Scalar{:type :bool},
   :string #jsonschema.type_system.types.Scalar{:type :string},
   :date #jsonschema.type_system.types.Scalar{:type :date},
   :number #jsonschema.type_system.types.Scalar{:type :number},
   :id #jsonschema.type_system.types.Scalar{:type :id}})



(def scalar-scalar-unions
  {:null-string
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :null}
                                             #jsonschema.type_system.types.Scalar{:type :string}}},
   :date-bool 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :bool}
                                             #jsonschema.type_system.types.Scalar{:type :date}}},
   :string-null 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :null}
                                             #jsonschema.type_system.types.Scalar{:type :string}}},
   :number-string 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :number}
                                             #jsonschema.type_system.types.Scalar{:type :string}}},
   :id-bool 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :bool}
                                             #jsonschema.type_system.types.Scalar{:type :id}}},
   :date-null 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :null}
                                             #jsonschema.type_system.types.Scalar{:type :date}}},
   :date-string 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :string}
                                             #jsonschema.type_system.types.Scalar{:type :date}}},
   :id-null 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :null}
                                             #jsonschema.type_system.types.Scalar{:type :id}}},
   :bool-id 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :bool}
                                             #jsonschema.type_system.types.Scalar{:type :id}}},
   :bool-number 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :bool}
                                             #jsonschema.type_system.types.Scalar{:type :number}}},
   :number-null 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :null}
                                             #jsonschema.type_system.types.Scalar{:type :number}}},
   :bool-null 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :null}
                                             #jsonschema.type_system.types.Scalar{:type :bool}}},
   :null-date 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :null}
                                             #jsonschema.type_system.types.Scalar{:type :date}}},
   :bool-date 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :bool}
                                             #jsonschema.type_system.types.Scalar{:type :date}}},
   :number-date 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :number}
                                             #jsonschema.type_system.types.Scalar{:type :date}}},
   :string-date 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :string}
                                             #jsonschema.type_system.types.Scalar{:type :date}}},
   :string-id 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :id}
                                             #jsonschema.type_system.types.Scalar{:type :string}}},
   :bool-string 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :bool}
                                             #jsonschema.type_system.types.Scalar{:type :string}}},
   :number-bool 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :bool}
                                             #jsonschema.type_system.types.Scalar{:type :number}}},
   :id-string 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :id}
                                             #jsonschema.type_system.types.Scalar{:type :string}}},
   :id-number 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :id}
                                             #jsonschema.type_system.types.Scalar{:type :number}}},
   :string-number 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :number}
                                             #jsonschema.type_system.types.Scalar{:type :string}}},
   :number-id 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :id}
                                             #jsonschema.type_system.types.Scalar{:type :number}}},
   :null-id 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :null}
                                             #jsonschema.type_system.types.Scalar{:type :id}}},
   :id-date 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :id}
                                             #jsonschema.type_system.types.Scalar{:type :date}}},
   :date-number 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :number}
                                             #jsonschema.type_system.types.Scalar{:type :date}}},
   :null-number 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :null}
                                             #jsonschema.type_system.types.Scalar{:type :number}}},
   :string-bool 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :bool}
                                             #jsonschema.type_system.types.Scalar{:type :string}}},
   :date-id 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :id}
                                             #jsonschema.type_system.types.Scalar{:type :date}}},
   :null-bool 
   #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :null}
                                             #jsonschema.type_system.types.Scalar{:type :bool}}}})

;; ### The actual scalar-scalar union tests
;;
;; Using the maps above for validation
;; this creates the references to the test values and the supposed result
;; so we don't have to type it all out by hand.
;;
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
               scalar then the result is a union of the two types."
      (gen-scalar-scalar-test [:null :bool :number :date :id :string])))


  (testing "Scalar - Collection / Document"
    (testing "Tests if we attempt to merge to a scalar with a document,
            or collection, then the result is a union of the two types."
      (merged-is (scalar-types :bool) (doc-types :scalars)
                 #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :bool} #jsonschema.type_system.types.Document{:properties [:a :name :c :b :f :d :e], :map {:a #jsonschema.type_system.types.Scalar{:type :string}, :name #jsonschema.type_system.types.Scalar{:type :string}, :c #jsonschema.type_system.types.Scalar{:type :number}, :b #jsonschema.type_system.types.Scalar{:type :null}, :f #jsonschema.type_system.types.Scalar{:type :id}, :d #jsonschema.type_system.types.Scalar{:type :bool}, :e #jsonschema.type_system.types.Scalar{:type :date}}}}})
      (merged-is (scalar-types :bool) (coll-types :single)
                 #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :string}} #jsonschema.type_system.types.Scalar{:type :bool}}})))

  (testing "Scalar - Union"
   (testing "If we attempt to merge a scalar with a union type,
             then we add the scalar to the union type if it is not already present,
             then return the union."
     (merged-is (scalar-types :null) (scalar-scalar-unions :bool-string)
                #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :null} #jsonschema.type_system.types.Scalar{:type :bool} #jsonschema.type_system.types.Scalar{:type :string}}})
     (merged-is (scalar-types :null) (scalar-scalar-unions :null-string)
                #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :null} #jsonschema.type_system.types.Scalar{:type :string}}})
     (merged-is (scalar-scalar-unions :null-string) (scalar-types :null)
                #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :null} #jsonschema.type_system.types.Scalar{:type :string}}}))))

  
;; # Document Tests

;; ## Document - Scalar / Collection
(deftest doc-scalar-coll-merging-tests
  (testing "Merging a document with any kind of scalar, or collection
            type results in a union of the two types."
    (merged-is (doc-types :scalars) (scalar-types :bool) 
               #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :bool} #jsonschema.type_system.types.Document{:properties [:a :name :c :b :f :d :e], :map {:a #jsonschema.type_system.types.Scalar{:type :string}, :name #jsonschema.type_system.types.Scalar{:type :string}, :c #jsonschema.type_system.types.Scalar{:type :number}, :b #jsonschema.type_system.types.Scalar{:type :null}, :f #jsonschema.type_system.types.Scalar{:type :id}, :d #jsonschema.type_system.types.Scalar{:type :bool}, :e #jsonschema.type_system.types.Scalar{:type :date}}}}})
    (merged-is (doc-types :flat-mixed-collection) (coll-types :mixed)
               #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Document{:properties [:a :name], :map {:a #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :null} #jsonschema.type_system.types.Scalar{:type :number} #jsonschema.type_system.types.Scalar{:type :string}}}}, :name #jsonschema.type_system.types.Scalar{:type :string}}} #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :null} #jsonschema.type_system.types.Scalar{:type :bool} #jsonschema.type_system.types.Scalar{:type :id} #jsonschema.type_system.types.Scalar{:type :number} #jsonschema.type_system.types.Scalar{:type :string} #jsonschema.type_system.types.Scalar{:type :date}}}}}})))

;; ## Document - Incongurent Merging

(def merged-one-two-type
  #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Document{:properties [:a :b], :map {:a #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :number}}, :b #jsonschema.type_system.types.Scalar{:type :string}}} #jsonschema.type_system.types.Document{:properties [:a], :map {:a #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :null} #jsonschema.type_system.types.Scalar{:type :number}}}}}}}})

(deftest doc-incongruent-merging-tests
  (testing "Merging a document with any kind of **incongruent** document type
            results in a union of the two types."
      (merged-is (extract-type {:a [7 8 nil]})
                 (extract-type {:a [1 2 3]
                                :b "not-a-collection"})
                 merged-one-two-type)))

;; ## Document - Congruent Merging

(def congruent1
  (extract-type {:a "12"
                 :b "same type" 
                 :c ["one"2 "fire" true]
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
    (testing "This block tests if things will actually be merged"
      (merged-is congruent1 congruent2                 
                 #jsonschema.type_system.types.Document{:properties [:a :c :b :d :e], :map {:a #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :number} #jsonschema.type_system.types.Scalar{:type :string}}}, :c #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :bool} #jsonschema.type_system.types.Scalar{:type :number} #jsonschema.type_system.types.Scalar{:type :string}}}} #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :number}}}}, :b #jsonschema.type_system.types.Scalar{:type :string}, :d #jsonschema.type_system.types.Document{:properties [:a :c :b :d], :map {:a #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :number} #jsonschema.type_system.types.Scalar{:type :string}}}, :c #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :string}}, :b #jsonschema.type_system.types.Scalar{:type :string}, :d #jsonschema.type_system.types.Document{:properties [:inside], :map {:inside #jsonschema.type_system.types.Scalar{:type :string}}}}}, :e #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :null} #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :number}}}}}}))
    (testing "This block tests if two things are congruent and have all the same types we'll get something that's equivalent to the original type of either document"
      (is (= (type-merge merger congruent1 congruent1) congruent1)))))


;; ## Document - Union
(def a-doc-type (extract-type {:a 5 :b "Hello"}))
(def union-with-no-doc
  #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :number}
                                            #jsonschema.type_system.types.Scalar{:type :string}}})
(def union-with-incongruent-docs
  #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :number}
                                            #jsonschema.type_system.types.Document{:properties [:a],
                                                                             :map {:a #jsonschema.type_system.types.Scalar{:type :number}}}
                                            #jsonschema.type_system.types.Document{:properties [:a :c :b],
                                                                             :map {:a #jsonschema.type_system.types.Scalar{:type :number},
                                                                                   :c #jsonschema.type_system.types.Scalar{:type :null},
                                                                                   :b #jsonschema.type_system.types.Scalar{:type :string}}}}})
(def union-with-congruent-doc
  #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :number}
                                            #jsonschema.type_system.types.Document{:properties [:a],
                                                                             :map {:a #jsonschema.type_system.types.Scalar{:type :string},
                                                                                   :b #jsonschema.type_system.types.Scalar{:type :number}}}}})
(def union-with-equal-doc
  #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :number}
                                            #jsonschema.type_system.types.Document{:properties [:a :b],
                                                                             :map {:a #jsonschema.type_system.types.Scalar{:type :number},
                                                                                   :b #jsonschema.type_system.types.Scalar{:type :string}}}}})


(deftest doc-union-merging-tests
  (testing "When merging a document with a union,
            and the union contains no document types,
            then add this document type to the union"
    (merged-is a-doc-type union-with-no-doc
               #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :number}
                                                         #jsonschema.type_system.types.Document{:properties [:a :b],
                                                                                          :map {:a #jsonschema.type_system.types.Scalar{:type :number},
                                                                                                :b #jsonschema.type_system.types.Scalar{:type :string}}}
                                                         #jsonschema.type_system.types.Scalar{:type :string}}}))

  (testing "When merging a document with a union,
            and every document type in the union is incongruent with this document,
            then this document should be added to the union"
    (merged-is a-doc-type union-with-incongruent-docs
               #jsonschema.type_system.types.Union{:union-of
                                             #{#jsonschema.type_system.types.Document{:properties [:a],
                                                                                :map {:a #jsonschema.type_system.types.Scalar{:type :number}}}
                                               #jsonschema.type_system.types.Scalar{:type :number}
                                               #jsonschema.type_system.types.Document{:properties [:a :c :b],
                                                                                :map {:a #jsonschema.type_system.types.Scalar{:type :number},
                                                                                      :c #jsonschema.type_system.types.Scalar{:type :null},
                                                                                      :b #jsonschema.type_system.types.Scalar{:type :string}}}
                                               #jsonschema.type_system.types.Document{:properties [:a :b],
                                                                                :map {:a #jsonschema.type_system.types.Scalar{:type :number},
                                                                                      :b #jsonschema.type_system.types.Scalar{:type :string}}}}}))

  (testing "When merging a document with a union,
            and there exists exactly one document in the union with which this document is congruent with,
            then this document should be merged with the congruent document union"
    (merged-is a-doc-type union-with-congruent-doc
               #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Document{:properties [:a],
                                                                                          :map {:a #jsonschema.type_system.types.Scalar{:type :string},
                                                                                                :b #jsonschema.type_system.types.Scalar{:type :number}}}
                                                         #jsonschema.type_system.types.Scalar{:type :number}
                                                         #jsonschema.type_system.types.Document{:properties [:a :b],
                                                                                          :map {:a #jsonschema.type_system.types.Scalar{:type :number},
                                                                                                :b #jsonschema.type_system.types.Scalar{:type :string}}}}}))    
      
  (testing "When merging a document with a union,
            and union contains a document type that is equivalent to this one,
            then this is a no-op"
    (merged-is a-doc-type union-with-equal-doc
               #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :number}
                                                         #jsonschema.type_system.types.Document{:properties [:a :b],
                                                                                          :map {:a #jsonschema.type_system.types.Scalar{:type :number},
                                                                                                :b #jsonschema.type_system.types.Scalar{:type :string}}}}})))

;; # Collection Tests
;; # Collection - Scalar / Document / Collection / Union

(def coll-of-str (extract-type ["5" "6"]))
(def coll-of-num (extract-type [5 6]))
(def coll-of-null (extract-type [nil nil]))
(def union-of-coll-of-str-null
  (make-union-with coll-of-str coll-of-null))

(deftest collection-tests
  (testing "Collections follow all the rules of merging
            scalar types when merging with scalars,
            documents, collections, unions types."
    (testing "Scalars"
      (merged-is coll-of-str (scalar-types :null)
                 #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :null}
                                                           #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :string}}}}))
    (testing "Documents"
      (merged-is coll-of-str a-doc-type
                 #jsonschema.type_system.types.Union{:union-of
                                               #{#jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :string}}
                                                 #jsonschema.type_system.types.Document{:properties [:a :b],
                                                                                  :map {:a #jsonschema.type_system.types.Scalar{:type :number},
                                                                                        :b #jsonschema.type_system.types.Scalar{:type :string}}}}}))
    (testing "Collections"
      (testing "Identity"
        (merged-is coll-of-str coll-of-str
                   #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :string}}))
      (merged-is coll-of-num coll-of-str
                 #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :string}}
                                                           #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :number}}}}))
    (testing "Unions"
      (merged-is coll-of-str union-of-coll-of-str-null
                 #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :string}}
                                                           #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :null}}}})
      (merged-is coll-of-num union-of-coll-of-str-null
                 #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :string}}
                                                           #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :null}}
                                                           #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :number}}}}))))



;; Union Merging


(deftest union-tests
  (testing "Identity: Unions always contain a unique set of types.
            (i.e. _if( u1 contains t1 and t1.equals(t2) )_ then
            _merge(u1, t2).equals(u1)_ is true"
    (let [some-types (map extract-type ["hello"  "hello" 5
                                        "hi" 5  [4 5 6]
                                        [7 8 9] "hi" [7 8 9]
                                        42 {:a "crazy"} {:a "hello"}])
          reversed-types (reverse some-types)
          merged-type (apply merge-types some-types)
          reverse-merged-type (apply merge-types reversed-types)]
      (is (and (= merged-type reverse-merged-type)
               (= merged-type          
                  #jsonschema.type_system.types.Union{:union-of
                                                #{#jsonschema.type_system.types.Document{:properties [:a], :map {:a #jsonschema.type_system.types.Scalar{:type :string}}}
                                                  #jsonschema.type_system.types.Scalar{:type :number}
                                                  #jsonschema.type_system.types.Scalar{:type :string}
                                                  #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Scalar{:type :number}}}})))))
  (testing "Union - Union"
    (testing "If we attempt to merge two union types,
              the result is a union type containing a set
              containing the elements of
              1. The set of scalars amongst the two
              2. The set of collections amongst the two
              3. The set of documents as a result of
                 merging any congruent documents"
      (let
          [some-types (map extract-type ["hello" 5 nil
                                         {:a "crazy"}
                                         {:a "hello"}
                                         {:a "hello"
                                          :b 5}
                                         {:a "hello"
                                          :b true}
                                         {:a 10
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
           union1 (apply merge-types some-types)
           union2 (apply merge-types other-types)]
        (merged-is union1 union2
                   #jsonschema.type_system.types.Union{:union-of
                                                 #{#jsonschema.type_system.types.Scalar{:type :null}
                                                   #jsonschema.type_system.types.Document{:properties [:a],
                                                                                    :map {:a #jsonschema.type_system.types.Scalar{:type :string}}}
                                                   #jsonschema.type_system.types.Scalar{:type :number}
                                                   #jsonschema.type_system.types.Scalar{:type :string}
                                                   #jsonschema.type_system.types.Document{:properties [:y :x],
                                                                                    :map {:y #jsonschema.type_system.types.Scalar{:type :string},
                                                                                          :x #jsonschema.type_system.types.Scalar{:type :string}}}
                                                   #jsonschema.type_system.types.Document{:properties [:a :c],
                                                                                    :map {:a #jsonschema.type_system.types.Scalar{:type :number},
                                                                                          :c #jsonschema.type_system.types.Scalar{:type :number}}}
                                                   #jsonschema.type_system.types.Document{:properties [:a :b],
                                                                                    :map {:a #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :null}
                                                                                                                                       #jsonschema.type_system.types.Scalar{:type :string}}},
                                                                                          :b #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Scalar{:type :null}
                                                                                                                                       #jsonschema.type_system.types.Scalar{:type :bool}
                                                                                                                                       #jsonschema.type_system.types.Scalar{:type :number}}}}}}})))))

                            




    
               


