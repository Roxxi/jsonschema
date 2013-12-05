(ns jsonschema.type-system.extract-test
  (:use clojure.test
        clojure.pprint
        jsonschema.type-system.types
        cheshire.core)
  (:require [jsonschema.type-system.merge :refer [merge-reducer]]
            [jsonschema.type-system.simplify :refer [simplify-reducer]]
            [jsonschema.type-system.extract
             :refer
             [extract-type-merging
              extract-type-simplifying
              clojure-predicator
              clojure-type-extractor
              extract
              special?
              null?
              bool?
              int?
              real?
              str?
              date?
              document?
              collection?]]
            [roxxi.utils.common :refer [def-]]))

(def- date-format-pattern "yyyy-MM-dd")

(deftest special-predicate-tests
  (testing "No special predicates as of now"))

(def predy (clojure-predicator (vector date-format-pattern)))

(defmacro p-is
  ([pred val]
     `(is (~pred predy ~val)))
  ([pred val comment]
     `(is (~pred predy ~val) ~comment)))

(defmacro p-is-not
  ([pred val]
     `(is (not (~pred predy ~val))))
  ([pred val comment]
     `(is (not (~pred predy ~val)) ~comment)))

(deftest clojure-predicator-test
  (testing "Testing the ClojureTypePredicator"
    (p-is null? nil)
    (p-is-not null? 5)
    (p-is-not special? "I'm not special")
    (p-is-not str? "2013-01-01" "It's special, it shouldn't be a string.")
    (p-is str? "hello!")
    (p-is str? "date(")
    (p-is str? "id(")
    (p-is int? 5)
    (p-is-not real? 5)
    (p-is real? 5.0)
    (p-is-not int? 5.0)
    (p-is real? 1.123456789012345678901234567890)
    (p-is-not int? "5")
    (p-is-not int? nil)
    (p-is-not real? "5")
    (p-is-not real? "5.0")
    (p-is-not real? nil)
    (p-is bool? true)
    (p-is bool? false)
    (p-is-not bool? "true")
    (p-is-not bool? "false")
    (p-is-not bool? "nil")
    (p-is-not bool? "other string")
    (p-is-not bool? nil)
    (p-is-not bool? 0)
    (p-is-not bool? 1.0)
    (p-is-not bool? [])
    (p-is-not bool? {})
    (p-is-not bool? {:a 1})
    (p-is date? "2013-10-10")
    (p-is date? "2-10-10")
    (p-is-not date? "2013--10")
    (p-is-not date? "")
    (p-is-not date? true)
    (p-is-not date? false)
    (p-is-not date? nil)
    (p-is-not date? "true")
    (p-is-not date? "false")
    (p-is-not date? "nil")
    (p-is-not date? "other string")
    (p-is-not date? 0)
    (p-is-not date? 1.0)
    (p-is-not date? [])
    (p-is-not date? {})
    (p-is-not date? {:a 1})
    (p-is-not document? [])
    (p-is-not document? [5 :a])
    (p-is-not document? [:a :a])
    (p-is-not document? #{:a :b})
    (p-is document? {})
    (p-is document? {:a :a})
    (p-is document? {:a 5, :b 6})
    (p-is document? {:a 5, :b 6 :c []})
    (p-is document? {:a 5, :b 6 :c ["a" "b" "c"]})
    (p-is document? {:a 5, :b 6 :c {:something :else :that :is :good :enough}})
    (p-is document? {:name "scalars"
                     :a "simple" :b nil :c 25 :d true :e "2013-01-01" :f "id(something)"})
    (p-is document? {:name "flat-collection" :a ["item1" "item2" "item3"]})
    (p-is document? {:name "flat-mixed-collection" :a ["item1" 25 nil]})
    (p-is document? {:name "flat-nested-collection"
                     :a [34 25 46 ["2013-01-01" "2013-01-02"]]})
    (p-is document? {:name "flat-nested-mixed-collection"
                     :a [34 25 46 ["2013-01-01" "2013-01-02" "hello"]]})
    (p-is document? {:name "subdoc" :a {:a-collection ["item1" "item2" "item3"]}})
    (p-is document? {:name "coll-subdoc"
                     :a [ {:a ["item1" "item2" "item3"]}
                          {:a [1 2 3]}
                          {:a [4 5 6]}
                          {:a [7 8 nil]}
                          {:a [1 2 3]
                           :b "not-a-collection"}]})
    (p-is document? {:name "less-scalars" :a "simple" :b nil :c 25 :d true})
    (p-is-not collection? nil)
    (p-is-not collection? 5)
    (p-is-not collection? "5")
    (p-is-not collection? "a string")
    (p-is-not collection? 5.0)
    (p-is-not collection? {})
    (p-is-not collection? {:a :map :that :is :not :empty})
    (p-is-not collection? {[] :a :b []})
    (p-is-not collection? {[] []})
    (p-is collection? [])
    (p-is collection? [nil true "Hello" 6 "2013-01-01" "id(something)"])
    (p-is collection? [1 2 3])
    (p-is collection? ["string" "string" "string" "string"])
    (p-is collection? [[] []])
    (p-is collection? [ [ [] [] [] ] [ [] [] ] [ [] 5 [] ] ])
    (p-is collection? [ [1 "str" nil] [1 "str"] ["id(id)" "str" nil] ])
    (p-is collection? [ [1 "str" nil] [1 "str" nil] [1 "str" nil] ])
    (p-is collection? [ ["string" "string" "string" "string"] [8 9 2] [nil nil] ])
    (p-is collection? [[1 2 3] [4 5 6 7] [8 9 2]])
    (p-is collection? ["a" 1 nil])
    (p-is collection? [{:a 1}])))


(defmacro t-is
  ([extract-fn x val]
     `(is (= (~extract-fn ~x) ~val)))
  ([extract-fn x val comment]
     `(is (= (~extract-fn ~x) ~val) ~comment)))

(defn- merge-extract-fn [val]
  (let [extractor (clojure-type-extractor merge-reducer [date-format-pattern])]
    (extract extractor val)))

(defmacro merged-t-is [x val]
  `(t-is merge-extract-fn ~x ~val))

(defn- simplify-extract-fn [val]
  (let [extractor (clojure-type-extractor simplify-reducer [date-format-pattern])]
    (extract extractor val)))

(defmacro simplified-t-is [x val]
  `(t-is simplify-extract-fn ~x ~val))

(def complex-doc
  {"a" 5,
   "b" [1 2 "a" 12 "b"],
   "c" {"a" 5, "b" [1 2 "a" 12 "b"], "c" "2013-01-01", "d" [1 2 3]},
   "d" [{"a" 5, "b" [1 2 "a" 12 "b"], "c" "2013-01-01", "d" [1 2 3]} 10 "a"]})

(def complex-doc-keys
  {:a 5,
   :c {:a 5, :c "2013-01-01", :b [1 2 "a" 12 "b"], :d [1 2 3]},
   :b [1 2 "a" 12 "b"],
   :d [{:a 5, :c "2013-01-01", :b [1 2 "a" 12 "b"], :d [1 2 3]} 10 "a"]})

(def complex-doc-type
  #jsonschema.type_system.types.Document{
                                         :properties #{"a" "b" "c" "d"},
                                         :map {"a" #jsonschema.type_system.types.Int{:min 5, :max 5},
                                               "b" #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Str{:min 1, :max 1} #jsonschema.type_system.types.Int{:min 1, :max 12}}}},
                                               "c" #jsonschema.type_system.types.Document{:properties #{"a" "b" "c" "d"}, :map {"a" #jsonschema.type_system.types.Int{:min 5, :max 5}, "b" #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Str{:min 1, :max 1} #jsonschema.type_system.types.Int{:min 1, :max 12}}}}, "c" #jsonschema.type_system.types.Date{:formats #{"yyyy-MM-dd"}}, "d" #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Int{:min 1, :max 3}}}},
                                               "d" #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Document{:properties #{"a" "b" "c" "d"}, :map {"a" #jsonschema.type_system.types.Int{:min 5, :max 5}, "b" #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Str{:min 1, :max 1} #jsonschema.type_system.types.Int{:min 1, :max 12}}}}, "c" #jsonschema.type_system.types.Date{:formats #{"yyyy-MM-dd"}}, "d" #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Int{:min 1, :max 3}}}} #jsonschema.type_system.types.Str{:min 1, :max 1} #jsonschema.type_system.types.Int{:min 10, :max 10}}}}}})

(def complex-doc-keys-type
  #jsonschema.type_system.types.Document{
                                         :properties #{:a :c :b :d},
                                         :map {:a #jsonschema.type_system.types.Int{:min 5, :max 5},
                                               :c #jsonschema.type_system.types.Document{:properties #{:a :c :b :d}, :map {:a #jsonschema.type_system.types.Int{:min 5, :max 5}, :c #jsonschema.type_system.types.Date{:formats #{"yyyy-MM-dd"}}, :b #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Str{:min 1, :max 1} #jsonschema.type_system.types.Int{:min 1, :max 12}}}}, :d #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Int{:min 1, :max 3}}}},
                                               :b #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Str{:min 1, :max 1} #jsonschema.type_system.types.Int{:min 1, :max 12}}}},
                                               :d #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Str{:min 1, :max 1} #jsonschema.type_system.types.Document{:properties #{:a :c :b :d}, :map {:a #jsonschema.type_system.types.Int{:min 5, :max 5}, :c #jsonschema.type_system.types.Date{:formats #{"yyyy-MM-dd"}}, :b #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Str{:min 1, :max 1} #jsonschema.type_system.types.Int{:min 1, :max 12}}}}, :d #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Int{:min 1, :max 3}}}} #jsonschema.type_system.types.Int{:min 10, :max 10}}}}}})

;; TODO add tests here for all the type tests above
(deftest clojure-extractor-test
  (testing "Testing the MergingClojureTypeExtractor"
    (merged-t-is nil (make-null))
    (merged-t-is 5 (make-int 5 5))
    (merged-t-is 5.0 (make-real 5.0 5.0))
    (merged-t-is "hello" (make-str 5 5))
    (merged-t-is "1989-10-24" (make-date (vector date-format-pattern)))
    (merged-t-is false (make-bool))
    (merged-t-is true (make-bool))
    (is (thrown? RuntimeException (extract-type-merging :hello))
        "It shouldn't know what to do with a keyword")
    (merged-t-is [1 2 3] #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Int{:min 1, :max 3}})
    (merged-t-is [ [] [] ] #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Collection{:coll-of :nothing}})
    (merged-t-is [1 2 "a"] #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Int{:min 1, :max 2} #jsonschema.type_system.types.Str{:min 1, :max 1}}}})
    (merged-t-is [1 2 "a" 12 "bb"] #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Str{:min 1, :max 2} #jsonschema.type_system.types.Int{:min 1, :max 12}}}})
    (merged-t-is {:a 1 :b 2 :c true :d nil :e 1.0 :f "string"} #jsonschema.type_system.types.Document{:properties #{:a :c :b :f :d :e}, :map {:a #jsonschema.type_system.types.Int{:min 1, :max 1}, :c #jsonschema.type_system.types.Bool{}, :b #jsonschema.type_system.types.Int{:min 2, :max 2}, :f #jsonschema.type_system.types.Str{:min 6, :max 6}, :d #jsonschema.type_system.types.Null{}, :e #jsonschema.type_system.types.Real{:min 1.0, :max 1.0}}})
    (merged-t-is complex-doc complex-doc-type)
    (merged-t-is complex-doc-keys complex-doc-keys-type)))

(deftest types-4-tweets
  (testing
      "Generating types for tweets in tweets.js
       This isn't really a test. It's for documentation"
    (let [result
          (let [parsed-js (parse-string (slurp "test/jsonschema/type_system/tweets.js"))]
            (map #(extract-type-merging %) parsed-js))]
      (is (coll? result)))))

;; # Tests from Matt

(deftest merge-vs-simplify
  (testing "Testing the differences between merge and simplify with"
    (testing "incongruent documents"
      (merged-t-is [{:a 1} {:a 10 :b 2}]
                   #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Document{:properties #{:a}, :map {:a #jsonschema.type_system.types.Int{:min 1, :max 1}}} #jsonschema.type_system.types.Document{:properties #{:a :b}, :map {:a #jsonschema.type_system.types.Int{:min 10, :max 10}, :b #jsonschema.type_system.types.Int{:min 2, :max 2}}}}}})
      (simplified-t-is [{:a 1} {:a 10 :b 2}]
                       #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Document{:properties #{:a :b}, :map {:b #jsonschema.type_system.types.Int{:min 2, :max 2}, :a #jsonschema.type_system.types.Int{:min 1, :max 10}}}}))

    (testing "empty collections"
      (merged-t-is [ [] [1] ]
                   #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Collection{:coll-of :nothing} #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Int{:min 1, :max 1}}}}})
      (simplified-t-is [ [] [1] ]
                       #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Int{:min 1, :max 1}}}))

    (testing "non-compatible nested collections"
      (testing "test 1"
        (merged-t-is [ [1] ["a"] [2 "asdf"] [24] ["aa"] ]
                     #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Int{:min 1, :max 24}} #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Str{:min 1, :max 2}} #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Int{:min 2, :max 2} #jsonschema.type_system.types.Str{:min 4, :max 4}}}}}}})
        (simplified-t-is [ [1] ["a"] [2 "asdf"] [24] ["aa"] ]
                         #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Str{:min 1, :max 4} #jsonschema.type_system.types.Int{:min 1, :max 24}}}}}))

      (testing "test 2"
        (merged-t-is [[1 "a"] [2 true]]
                     #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Bool{} #jsonschema.type_system.types.Int{:min 2, :max 2}}}} #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Int{:min 1, :max 1} #jsonschema.type_system.types.Str{:min 1, :max 1}}}}}}})
        (simplified-t-is [[1 "a"] [2 true]]
                         #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Bool{} #jsonschema.type_system.types.Int{:min 1, :max 2} #jsonschema.type_system.types.Str{:min 1, :max 1}}}}}))

      (testing "test 3"
        (merged-t-is [[1 true "a"] [2 false]]
                     #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Bool{} #jsonschema.type_system.types.Int{:min 2, :max 2}}}} #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Bool{} #jsonschema.type_system.types.Int{:min 1, :max 1} #jsonschema.type_system.types.Str{:min 1, :max 1}}}}}}})
        (simplified-t-is [[1 true "a"] [2 false]]
                         #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Bool{} #jsonschema.type_system.types.Int{:min 1, :max 2} #jsonschema.type_system.types.Str{:min 1, :max 1}}}}}))

      (testing "test 4"
        (merged-t-is [[1] ["asdf"]]
                     #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Str{:min 4, :max 4}} #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Int{:min 1, :max 1}}}}})
        (simplified-t-is [[1] ["asdf"]]
                         #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Int{:min 1, :max 1} #jsonschema.type_system.types.Str{:min 4, :max 4}}}}})))))


(deftest merge-and-simplify-behave-the-same
  (testing "Testing that merging and simplifying behave the same with"
    (testing "collections and scalars"
      (is (= (extract-type-merging [ [1 2 3] 4])
             (extract-type-simplifying [ [1 2 3] 4])))
      (is (= (extract-type-merging [ [1 2 3] 4])
             #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Int{:min 4, :max 4} #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Int{:min 1, :max 3}}}}})))

    (testing "congruent dictionaries"
      (is (= (extract-type-merging [{:a 1} {:a "str"}])
             (extract-type-simplifying [{:a 1} {:a "str"}])))
      (is (= (extract-type-merging [{:a 1} {:a "str"}])
             #jsonschema.type_system.types.Collection{:coll-of #jsonschema.type_system.types.Document{:properties #{:a}, :map {:a #jsonschema.type_system.types.Union{:union-of #{#jsonschema.type_system.types.Int{:min 1, :max 1} #jsonschema.type_system.types.Str{:min 3, :max 3}}}}}})))))
