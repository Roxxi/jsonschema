(ns jsonschema.vertica-types-test
  (:use clojure.test
        roxxi.utils.print
        jsonschema.transform)
  (:require [jsonschema.type-system.types :as types]
            [jsonschema.type-system.db-types.vertica :as vrt-types]))

;;;;;;;;;;;;;;;; INTEGER TYPES ;;;;;;;;;;;;;;;;;;;;;;;;

;; INTEGER
;; INT
;; BIGINT
;; INT8
;; SMALLINT
;; TINYINT
(deftest vrt-tinyint->json-type-test []
  (let [int-type (vrt-types/col-type->json-type "tinyint")]
    (is (= (type int-type) jsonschema.type_system.types.Int))
    (is (= (types/getMin int-type) vrt-types/SIGNED_64_INT_MIN))
    (is (= (types/getMax int-type) vrt-types/SIGNED_64_INT_MAX))))

(deftest vrt-smallint->json-type-test []
  (let [int-type (vrt-types/col-type->json-type "smallint")]
    (is (= (type int-type) jsonschema.type_system.types.Int))
    (is (= (types/getMin int-type) vrt-types/SIGNED_64_INT_MIN))
    (is (= (types/getMax int-type) vrt-types/SIGNED_64_INT_MAX))))

(deftest vrt-int8->json-type-test []
  (let [int-type (vrt-types/col-type->json-type "int8")]
    (is (= (type int-type) jsonschema.type_system.types.Int))
    (is (= (types/getMin int-type) vrt-types/SIGNED_64_INT_MIN))
    (is (= (types/getMax int-type) vrt-types/SIGNED_64_INT_MAX))))

(deftest vrt-int->json-type-test []
  (let [int-type (vrt-types/col-type->json-type "int")]
    (is (= (type int-type) jsonschema.type_system.types.Int))
    (is (= (types/getMin int-type) vrt-types/SIGNED_64_INT_MIN))
    (is (= (types/getMax int-type) vrt-types/SIGNED_64_INT_MAX))))

(deftest vrt-integer->json-type-test []
  (let [int-type (vrt-types/col-type->json-type "integer")]
    (is (= (type int-type) jsonschema.type_system.types.Int))
    (is (= (types/getMin int-type) vrt-types/SIGNED_64_INT_MIN))
    (is (= (types/getMax int-type) vrt-types/SIGNED_64_INT_MAX))))

;;;;;;;;;;;;;;;; BOOLEAN TYPES ;;;;;;;;;;;;;;;;;;;;;;;;
;; BOOLEAN

(deftest vrt-bool->json-type-test []
  (let [int-type (vrt-types/col-type->json-type "boolean")]
    (is (= (type int-type) jsonschema.type_system.types.Bool))))

;;;;;;;;;;;;;;;; BINARY TYPES ;;;;;;;;;;;;;;;;;;;;;;;;

;; BINARY
(deftest vrt-bin->json-type-test []
  (let [bin-type (vrt-types/col-type->json-type "binary")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) vrt-types/DEFAULT_BINARY_LENGTH))
    (is (= (types/getMax bin-type) vrt-types/DEFAULT_BINARY_LENGTH))))

(deftest vrt-bin1k->json-type-test []
  (let [bin-type (vrt-types/col-type->json-type "binary(1000)")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) 1000))
    (is (= (types/getMax bin-type) 1000))))

(deftest vrt-binmax->json-type-test []
  (let [bin-type (vrt-types/col-type->json-type "binary(999999)")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) vrt-types/MAX_BINARY_LENGTH))
    (is (= (types/getMax bin-type) vrt-types/MAX_BINARY_LENGTH))))

;; VARBINARY
(deftest vrt-varbin->json-type-test []
  (let [bin-type (vrt-types/col-type->json-type "varbinary")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) vrt-types/DEFAULT_VARBINARY_LENGTH))
    (is (= (types/getMax bin-type) vrt-types/DEFAULT_VARBINARY_LENGTH))))

(deftest vrt-varbin1k->json-type-test []
  (let [bin-type (vrt-types/col-type->json-type "varbinary(1000)")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) 1000))
    (is (= (types/getMax bin-type) 1000))))

(deftest vrt-varbinmax->json-type-test []
  (let [bin-type (vrt-types/col-type->json-type "varbinary(999999)")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) vrt-types/MAX_BINARY_LENGTH))
    (is (= (types/getMax bin-type) vrt-types/MAX_BINARY_LENGTH))))

;; BYTEA
;; RAW
;; (from Vertica 6 "BYTEA and RAW are synonyms for VARBINARY.")
(deftest vrt-bytea->json-type-test []
  (let [bin-type (vrt-types/col-type->json-type "bytea")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) vrt-types/DEFAULT_VARBINARY_LENGTH))
    (is (= (types/getMax bin-type) vrt-types/DEFAULT_VARBINARY_LENGTH))))

(deftest vrt-bytea1k->json-type-test []
  (let [bin-type (vrt-types/col-type->json-type "bytea(1000)")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) 1000))
    (is (= (types/getMax bin-type) 1000))))

(deftest vrt-byteamax->json-type-test []
  (let [bin-type (vrt-types/col-type->json-type "bytea(999999)")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) vrt-types/MAX_BINARY_LENGTH))
    (is (= (types/getMax bin-type) vrt-types/MAX_BINARY_LENGTH))))

(deftest vrt-raw->json-type-test []
  (let [bin-type (vrt-types/col-type->json-type "raw")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) vrt-types/DEFAULT_VARBINARY_LENGTH))
    (is (= (types/getMax bin-type) vrt-types/DEFAULT_VARBINARY_LENGTH))))

(deftest vrt-raw1k->json-type-test []
  (let [bin-type (vrt-types/col-type->json-type "raw(1000)")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) 1000))
    (is (= (types/getMax bin-type) 1000))))

(deftest vrt-rawmax->json-type-test []
  (let [bin-type (vrt-types/col-type->json-type "raw(999999)")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) vrt-types/MAX_BINARY_LENGTH))
    (is (= (types/getMax bin-type) vrt-types/MAX_BINARY_LENGTH))))
