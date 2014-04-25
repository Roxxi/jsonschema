(ns jsonschema.simple-types-test
  (:use clojure.test
        roxxi.utils.print
        jsonschema.transform)
  (:require [jsonschema.type-system.types :as types]
            [jsonschema.type-system.db-types.simple :as simple-types]
            [jsonschema.type-system.db-types.translator :as dt]))

(def simple-tt (simple-types/make-simple-type-translator))

(deftest simple-int->json-type-test []
  (let [json-type (dt/col-type->json-type simple-tt "int")]
    (is (= (type json-type) jsonschema.type_system.types.Int))
    (is (= (types/getMin json-type) simple-types/SIGNED_64_INT_MIN))
    (is (= (types/getMax json-type) simple-types/SIGNED_64_INT_MAX))))

(deftest simple-int->json-type-test []
  (let [json-type (dt/col-type->json-type simple-tt "str")]
    (is (= (type json-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin json-type) 0))
    (is (= (types/getMax json-type) 80))))

(deftest simple-bool->json-type-test []
  (let [json-type (dt/col-type->json-type simple-tt "bool")]
    (is (= (type json-type) jsonschema.type_system.types.Bool))))

(deftest simple-real->json-type-test []
  (let [json-type (dt/col-type->json-type simple-tt "real")]
    (is (= (type json-type) jsonschema.type_system.types.Real))))

(deftest simple-real->json-type-test []
  (let [json-type (dt/col-type->json-type simple-tt "date(yyyy-mm-dd)")]
    (is (= (type json-type) jsonschema.type_system.types.Date))
    (is (= (types/getFormats json-type) #{"yyyy-mm-dd"}))))
