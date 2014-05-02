(ns jsonschema.vertica-types-test
  (:use clojure.test
        roxxi.utils.print
        jsonschema.transform)
  (:require [jsonschema.type-system.types :as types]
            [jsonschema.type-system.db-types.vertica :as vrt-types]
            [jsonschema.type-system.db-types.translator :as dt]))

(def vertica-tt (vrt-types/make-vertica-type-translator))

;;;;;;;;;;;;;;;; INTEGER TYPES ;;;;;;;;;;;;;;;;;;;;;;;;

;; INTEGER
;; INT
;; BIGINT
;; INT8
;; SMALLINT
;; TINYINT
(deftest vrt-tinyint->json-type-test []
  (let [int-type (dt/col-type->json-type vertica-tt "tinyint")]
    (is (= (type int-type) jsonschema.type_system.types.Int))
    (is (= (types/getMin int-type) vrt-types/SIGNED_64_INT_MIN))
    (is (= (types/getMax int-type) vrt-types/SIGNED_64_INT_MAX))))

(deftest vrt-smallint->json-type-test []
  (let [int-type (dt/col-type->json-type vertica-tt "smallint")]
    (is (= (type int-type) jsonschema.type_system.types.Int))
    (is (= (types/getMin int-type) vrt-types/SIGNED_64_INT_MIN))
    (is (= (types/getMax int-type) vrt-types/SIGNED_64_INT_MAX))))

(deftest vrt-int8->json-type-test []
  (let [int-type (dt/col-type->json-type vertica-tt "int8")]
    (is (= (type int-type) jsonschema.type_system.types.Int))
    (is (= (types/getMin int-type) vrt-types/SIGNED_64_INT_MIN))
    (is (= (types/getMax int-type) vrt-types/SIGNED_64_INT_MAX))))

(deftest vrt-int->json-type-test []
  (let [int-type (dt/col-type->json-type vertica-tt "int")]
    (is (= (type int-type) jsonschema.type_system.types.Int))
    (is (= (types/getMin int-type) vrt-types/SIGNED_64_INT_MIN))
    (is (= (types/getMax int-type) vrt-types/SIGNED_64_INT_MAX))))

(deftest vrt-integer->json-type-test []
  (let [int-type (dt/col-type->json-type vertica-tt "integer")]
    (is (= (type int-type) jsonschema.type_system.types.Int))
    (is (= (types/getMin int-type) vrt-types/SIGNED_64_INT_MIN))
    (is (= (types/getMax int-type) vrt-types/SIGNED_64_INT_MAX))))

;;;;;;;;;;;;;;;; BOOLEAN TYPES ;;;;;;;;;;;;;;;;;;;;;;;;
;; BOOLEAN

(deftest vrt-bool->json-type-test []
  (let [int-type (dt/col-type->json-type vertica-tt "boolean")]
    (is (= (type int-type) jsonschema.type_system.types.Bool))))

;;;;;;;;;;;;;;;; BINARY TYPES ;;;;;;;;;;;;;;;;;;;;;;;;

;; BINARY
(deftest vrt-bin->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "binary")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) vrt-types/DEFAULT_BINARY_LENGTH))
    (is (= (types/getMax bin-type) vrt-types/DEFAULT_BINARY_LENGTH))))

(deftest vrt-bin1k->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "binary(1000)")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) 1000))
    (is (= (types/getMax bin-type) 1000))))

(deftest vrt-binmax->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "binary(999999)")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) vrt-types/MAX_BINARY_LENGTH))
    (is (= (types/getMax bin-type) vrt-types/MAX_BINARY_LENGTH))))

;; VARBINARY
(deftest vrt-varbin->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "varbinary")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) vrt-types/DEFAULT_VARBINARY_LENGTH))
    (is (= (types/getMax bin-type) vrt-types/DEFAULT_VARBINARY_LENGTH))))

(deftest vrt-varbin1k->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "varbinary(1000)")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) 1000))
    (is (= (types/getMax bin-type) 1000))))

(deftest vrt-varbinmax->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "varbinary(999999)")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) vrt-types/MAX_BINARY_LENGTH))
    (is (= (types/getMax bin-type) vrt-types/MAX_BINARY_LENGTH))))

;; BYTEA
;; RAW
;; (from Vertica 6 "BYTEA and RAW are synonyms for VARBINARY.")
(deftest vrt-bytea->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "bytea")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) vrt-types/DEFAULT_VARBINARY_LENGTH))
    (is (= (types/getMax bin-type) vrt-types/DEFAULT_VARBINARY_LENGTH))))

(deftest vrt-bytea1k->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "bytea(1000)")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) 1000))
    (is (= (types/getMax bin-type) 1000))))

(deftest vrt-byteamax->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "bytea(999999)")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) vrt-types/MAX_BINARY_LENGTH))
    (is (= (types/getMax bin-type) vrt-types/MAX_BINARY_LENGTH))))

(deftest vrt-raw->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "raw")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) vrt-types/DEFAULT_VARBINARY_LENGTH))
    (is (= (types/getMax bin-type) vrt-types/DEFAULT_VARBINARY_LENGTH))))

(deftest vrt-raw1k->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "raw(1000)")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) 1000))
    (is (= (types/getMax bin-type) 1000))))

(deftest vrt-rawmax->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "raw(999999)")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) vrt-types/MAX_BINARY_LENGTH))
    (is (= (types/getMax bin-type) vrt-types/MAX_BINARY_LENGTH))))

;;;;;;;;;;;;;;;; STRING TYPES ;;;;;;;;;;;;;;;;;;;;;;;;

;; CHARACTER
(deftest vrt-character->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "character")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) vrt-types/DEFAULT_CHAR_LENGTH))
    (is (= (types/getMax bin-type) vrt-types/DEFAULT_CHAR_LENGTH))))

(deftest vrt-character1k->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "character(1000)")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) 1000))
    (is (= (types/getMax bin-type) 1000))))

(deftest vrt-charactermax->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "character(999999)")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) vrt-types/MAX_CHAR_LENGTH))
    (is (= (types/getMax bin-type) vrt-types/MAX_CHAR_LENGTH))))

;; CHAR
(deftest vrt-char->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "char")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) vrt-types/DEFAULT_CHAR_LENGTH))
    (is (= (types/getMax bin-type) vrt-types/DEFAULT_CHAR_LENGTH))))

(deftest vrt-char1k->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "char(1000)")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) 1000))
    (is (= (types/getMax bin-type) 1000))))

(deftest vrt-charmax->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "char(999999)")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) vrt-types/MAX_CHAR_LENGTH))
    (is (= (types/getMax bin-type) vrt-types/MAX_CHAR_LENGTH))))

;; VARCHAR
(deftest vrt-varchar->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "varchar")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) vrt-types/DEFAULT_VARCHAR_LENGTH))
    (is (= (types/getMax bin-type) vrt-types/DEFAULT_VARCHAR_LENGTH))))

(deftest vrt-varchar1k->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "varchar(1000)")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) 1000))
    (is (= (types/getMax bin-type) 1000))))

(deftest vrt-varcharmax->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "varchar(999999)")]
    (is (= (type bin-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin bin-type) vrt-types/MAX_CHAR_LENGTH))
    (is (= (types/getMax bin-type) vrt-types/MAX_CHAR_LENGTH))))

;; DATE
(deftest vrt-date->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "date")]
    (is (= (type bin-type) jsonschema.type_system.types.Date))
    (is (= (types/getFormats bin-type) #{"yyyy-MM-dd"}))))

(deftest vrt-datetime->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "datetime")]
    (is (= (type bin-type) jsonschema.type_system.types.Date))
    (is (= (types/getFormats bin-type) #{"yyyy-MM-dd HH:mm:ss" "yyyy-MM-dd HH:mm:ssZ"}))))

(deftest vrt-smalldatetime->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "smalldatetime")]
    (is (= (type bin-type) jsonschema.type_system.types.Date))
    (is (= (types/getFormats bin-type) #{"yyyy-MM-dd HH:mm:ss" "yyyy-MM-dd HH:mm:ssZ"}))))

(deftest vrt-timestamp->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "timestamp")]
    (is (= (type bin-type) jsonschema.type_system.types.Date))
    (is (= (types/getFormats bin-type) #{"yyyy-MM-dd HH:mm:ss" "yyyy-MM-dd HH:mm:ssZ"}))))

;; INTERVAL maps easier to a Real than to a Date
(deftest vrt-interval->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "interval")]
    (is (= (type bin-type) jsonschema.type_system.types.Real))
    (is (= (types/getMin bin-type) vrt-types/MIN_NUMERIC_VALUE))
    (is (= (types/getMax bin-type) vrt-types/MAX_NUMERIC_VALUE))))

;;;;;;;;;;;;;;;; REAL TYPES ;;;;;;;;;;;;;;;;;;;;;;;;

;; NUMERICs
(deftest vrt-interval->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "interval")]
    (is (= (type bin-type) jsonschema.type_system.types.Real))
    (is (= (types/getMin bin-type) vrt-types/MIN_NUMERIC_VALUE))
    (is (= (types/getMax bin-type) vrt-types/MAX_NUMERIC_VALUE))))

(deftest vrt-decimal->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "decimal")]
    (is (= (type bin-type) jsonschema.type_system.types.Real))
    (is (= (types/getMin bin-type) vrt-types/MIN_NUMERIC_VALUE))
    (is (= (types/getMax bin-type) vrt-types/MAX_NUMERIC_VALUE))))

(deftest vrt-number->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "number")]
    (is (= (type bin-type) jsonschema.type_system.types.Real))
    (is (= (types/getMin bin-type) vrt-types/MIN_NUMERIC_VALUE))
    (is (= (types/getMax bin-type) vrt-types/MAX_NUMERIC_VALUE))))

(deftest vrt-money->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "money")]
    (is (= (type bin-type) jsonschema.type_system.types.Real))
    (is (= (types/getMin bin-type) vrt-types/MIN_NUMERIC_VALUE))
    (is (= (types/getMax bin-type) vrt-types/MAX_NUMERIC_VALUE))))

;; DOUBLEs
(deftest vrt-double->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "double")]
    (is (= (type bin-type) jsonschema.type_system.types.Real))
    (is (= (types/getMin bin-type) vrt-types/MIN_IEEE_754_FLOAT))
    (is (= (types/getMax bin-type) vrt-types/MAX_IEEE_754_FLOAT))))

(deftest vrt-float->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "float")]
    (is (= (type bin-type) jsonschema.type_system.types.Real))
    (is (= (types/getMin bin-type) vrt-types/MIN_IEEE_754_FLOAT))
    (is (= (types/getMax bin-type) vrt-types/MAX_IEEE_754_FLOAT))))

(deftest vrt-float8->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "float8")]
    (is (= (type bin-type) jsonschema.type_system.types.Real))
    (is (= (types/getMin bin-type) vrt-types/MIN_IEEE_754_FLOAT))
    (is (= (types/getMax bin-type) vrt-types/MAX_IEEE_754_FLOAT))))

(deftest vrt-real->json-type-test []
  (let [bin-type (dt/col-type->json-type vertica-tt "real")]
    (is (= (type bin-type) jsonschema.type_system.types.Real))
    (is (= (types/getMin bin-type) vrt-types/MIN_IEEE_754_FLOAT))
    (is (= (types/getMax bin-type) vrt-types/MAX_IEEE_754_FLOAT))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REVERSE MAPPING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest schema-int->vrt-int []
  (let [schema-int (types/make-int 1024)
        vrt-int (dt/json-type->col-type vertica-tt schema-int)]
    (is (= vrt-int "int"))))

(deftest schema-int->vrt-bigint []
  (let [schema-int (types/make-int 9223372036854775807)
        vrt-int (dt/json-type->col-type vertica-tt schema-int)]
    (is (= vrt-int "int"))))

(deftest schema-int->vrt-bigint []
  (let [schema-int (types/make-int 9223372036854775808)]
    (is (thrown? clojure.lang.ExceptionInfo
                 (dt/json-type->col-type vertica-tt schema-int)))))

(deftest schema-varchar->vrt-str []
  (let [schema-str (types/make-str 1024 1024)
        vrt-str (dt/json-type->col-type vertica-tt schema-str)]
    (is (= vrt-str "varchar(1024)"))))

(deftest schema-big-varchar->vrt-str []
  (let [schema-str (types/make-str 70000 70000)
        vrt-str (dt/json-type->col-type vertica-tt schema-str)]
    (is (= vrt-str "varchar(65000)"))))

(deftest schema-bool->vrt->bool []
  (let [schema-bool (types/make-bool)
        vrt-bool (dt/json-type->col-type vertica-tt schema-bool)]
    (is (= vrt-bool "bool"))))

(deftest schema-real->vrt-real []
  (let [schema-real (types/make-real 1024 1024)
        vrt-real (dt/json-type->col-type vertica-tt schema-real)]
    (is (= vrt-real "decimal"))))
