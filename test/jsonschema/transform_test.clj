(ns jsonschema.transform-test
  (:use clojure.test
        roxxi.utils.print
        jsonschema.transform        
        jsonschema.type-system.extract)
  (:require [jsonschema.type-system.simplify :as s]
            [jsonschema.type-system.merge :as m]))

(deftest schema-translatability []
  (testing "Ensuring the following schemas are NOT translatable to a
database table"
    (testing "Scalars and collections"
      (are [valid? structure] (= valid? (type-translatable-to-table-schema?
                                         (extract-type structure)))
           false 5
           false "hello"
           false 5.5
           false nil
           false []
           false {}
           false [1 2 3]
           false [1 "hello" 5.5 nil]))
    (testing "Documents and unions as the result of merging"
      (are [valid? type] (= valid? (type-translatable-to-table-schema? type))
           false (extract-type {:p1 "hello" :nested {:n1 "nested"}})
           false (extract-type {:p1 "hello" :coll [nil 5 "hello"]})
           false (s/simplify-types
                  ;; union on both parameters, where the latter union 
                  ;; contains two doc types
                  (extract-type {:p1 10 :nested {:n1 5}})
                  (extract-type {:p1 "hello" :nested {:n1 "nested"}}))
           false (m/merge-types
                  ;; union on both parameters, where the latter
                  ;; union contains two doc types
                  (extract-type {:p1 10 :nested {:n1 5}})
                  (extract-type {:p1 "hello" :nested {:n1 "nested"}}))
           false (m/merge-types
                  ;; union containing a document and a string
                  (extract-type {:p1 10 :p2 {:n1 5 :n2 3}})
                  (extract-type {:p1 "hello" :p2 "bye"}))
           false (m/merge-types
                  (extract-type {:p1 10 :nested {:n1 5 :n2 3}})
                  (extract-type {:p1 "hello" :nested {:n1 "nested"}})))))
  (testing "The following schemas should be translatable to a DB table"
    (are [valid? type] (= valid? (type-translatable-to-table-schema? type))
         true (extract-type {:col1 "hello"})
         true (extract-type {:col1 "hello" :col2 10 :col3 5.5 :col4 nil})
         true (s/simplify-types
               (extract-type {:col1 10 :col2 "10.5"})
               (extract-type {:col1 "hello" :col2 12.8}))
         true (s/simplify-types
               (extract-type {:col1 10 :col2 "10.5" :col3 nil :col4 18})
               (extract-type {:col1 "hello" :col2 12.8})))))

(deftest tablification-tests []
  (testing "Making sure the following schemas can be converted to
a schema suitable for a Database table"
    ;; ex 1
    (is (= (db-tablify-type (extract-type {:col1 "hello"}))
           #jsonschema.type_system.types.Document{:properties [:col1],
                                                  :map {:col1 #jsonschema.type_system.types.Scalar{:type :string}}}))
    ;; ex2
    (is (= (db-tablify-type (extract-type {:col1 "hello" :col2 10 :col3 5.5 :col4 nil}))
           #jsonschema.type_system.types.Document{:properties [:col4 :col1 :col3 :col2],
                                                  :map {:col4 #jsonschema.type_system.types.Scalar{:type :null},
                                                        :col1 #jsonschema.type_system.types.Scalar{:type :string},
                                                        :col3 #jsonschema.type_system.types.Scalar{:type :real},
                                                        :col2 #jsonschema.type_system.types.Scalar{:type :int}}}))
    ;; ex3
    (is (= (db-tablify-type
            (s/simplify-types
             (extract-type {:col1 10 :col2 "10.5"})
             (extract-type {:col1 "hello" :col2 12.8})))
           #jsonschema.type_system.types.Document{:properties [:col1 :col2],
                                                  :map {:col1 #jsonschema.type_system.types.Scalar{:type :string},
                                                        :col2 #jsonschema.type_system.types.Scalar{:type :string}}}
           ))
    ;; ex4
    (is (= (db-tablify-type
            (s/simplify-types
             (extract-type {:col1 10 :col2 "10.5" :col3 nil :col4 18})
             (extract-type {:col1 "hello" :col2 12.8})))
           #jsonschema.type_system.types.Document{:properties [:col4 :col1 :col3 :col2],
                                                  :map {:col4 #jsonschema.type_system.types.Scalar{:type :int},
                                                        :col1 #jsonschema.type_system.types.Scalar{:type :string},
                                                        :col3 #jsonschema.type_system.types.Scalar{:type :null},
                                                        :col2 #jsonschema.type_system.types.Scalar{:type :string}}}
           ))
    ;; ex5
    (is (= (db-tablify-type
            (s/simplify-types
             (extract-type {:col1 nil  :col2 nil  :col3 nil  :col4 nil  :col5 nil})
             (extract-type {:col1 true :col2 true :col3 true :col4 true})
             (extract-type {:col1 1    :col2 1    :col3 1})
             (extract-type {:col1 5.5  :col2 5.5 })
             (extract-type {:col1 "hi"})))
           #jsonschema.type_system.types.Document{:properties [:col4 :col5 :col1 :col3 :col2],
                                                  :map {:col1 #jsonschema.type_system.types.Scalar{:type :string},
                                                        :col2 #jsonschema.type_system.types.Scalar{:type :real},
                                                        :col3 #jsonschema.type_system.types.Scalar{:type :int},
                                                        :col4 #jsonschema.type_system.types.Scalar{:type :bool},
                                                        :col5 #jsonschema.type_system.types.Scalar{:type :null}}}))))
         




                  


