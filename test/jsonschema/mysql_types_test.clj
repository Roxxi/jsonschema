(ns jsonschema.mysql-types-test
  (:use clojure.test
        roxxi.utils.print
        jsonschema.transform)
  (:require [jsonschema.type-system.types :as types]
            [jsonschema.type-system.db-types.mysql :as mysql-db-types]
            [jsonschema.type-system.db-types.translator :as dt]))

;; (deftest mysql-col-def-str->col-parts-test []
;;   (= (mysql-db-types/mysql-col-def-str->col-map "int(10) unsigned") {:json-type :int :mysql-type-kw :int_unsigned :col-type "int"}))

(def mysql-tt (mysql-db-types/make-mysql-type-translator))

;; INTEGER TYPES
(deftest mysql-tinyint->json-type-test []
  (let [int-type (dt/col-type->json-type mysql-tt "tinyint(1)")]
    (is (= (type int-type) jsonschema.type_system.types.Int))
    (is (= (types/getMin int-type) -128))
    (is (= (types/getMax int-type) 127))))

(deftest mysql-utinyint->json-type-test []
  (let [int-type (dt/col-type->json-type mysql-tt "tinyint(1) unsigned")]
    (is (= (type int-type) jsonschema.type_system.types.Int))
    (is (= (types/getMin int-type) 0))
    (is (= (types/getMax int-type) 255))))

(deftest mysql-smallint->json-type-test []
  (let [int-type (dt/col-type->json-type mysql-tt "smallint(2)")]
    (is (= (type int-type) jsonschema.type_system.types.Int))
    (is (= (types/getMin int-type) -32768))
    (is (= (types/getMax int-type) 32767))))

(deftest mysql-usmallint->json-type-test []
  (let [int-type (dt/col-type->json-type mysql-tt "smallint(2) unsigned")]
    (is (= (type int-type) jsonschema.type_system.types.Int))
    (is (= (types/getMin int-type) 0))
    (is (= (types/getMax int-type) 65535))))

(deftest mysql-mediumint->json-type-test []
  (let [int-type (dt/col-type->json-type mysql-tt "mediumint(3)")]
    (is (= (type int-type) jsonschema.type_system.types.Int))
    (is (= (types/getMin int-type) -8388608))
    (is (= (types/getMax int-type) 8388607))))

(deftest mysql-umediumint->json-type-test []
  (let [int-type (dt/col-type->json-type mysql-tt "mediumint(3) unsigned")]
    (is (= (type int-type) jsonschema.type_system.types.Int))
    (is (= (types/getMin int-type) 0))
    (is (= (types/getMax int-type) 16777215))))

(deftest mysql-int->json-type-test []
  (let [int-type (dt/col-type->json-type mysql-tt "int(10)")]
    (is (= (type int-type) jsonschema.type_system.types.Int))
    (is (= (types/getMin int-type) -2147483648))
    (is (= (types/getMax int-type) 2147483647))))

(deftest mysql-uint->json-type-test []
  (let [int-type (dt/col-type->json-type mysql-tt "int(11) unsigned")]
    (is (= (type int-type) jsonschema.type_system.types.Int))
    (is (= (types/getMin int-type) 0))
    (is (= (types/getMax int-type) 4294967295))))

(deftest mysql-bigint->json-type-test []
  (let [int-type (dt/col-type->json-type mysql-tt "bigint(20)")]
    (is (= (type int-type) jsonschema.type_system.types.Int))
    (is (= (types/getMin int-type) -9223372036854775808))
    (is (= (types/getMax int-type) 9223372036854775807))))

(deftest mysql-ubigint->json-type-test []
  (let [int-type (dt/col-type->json-type mysql-tt "bigint(21) unsigned")]
    (is (= (type int-type) jsonschema.type_system.types.Int))
    (is (= (types/getMin int-type) 0))
    (is (= (types/getMax int-type) 18446744073709551615))))

;; DECIMAL & NUMERIC
(deftest mysql-dec30->json-type-test []
  (let [real-type (dt/col-type->json-type mysql-tt "decimal(3,0)")]
    (is (= (type real-type) jsonschema.type_system.types.Real))
    (is (= (types/getMin real-type) mysql-db-types/DECIMAL_MIN))
    (is (= (types/getMax real-type) mysql-db-types/DECIMAL_MAX))))

(deftest mysql-dec45->json-type-test []
  (let [real-type (dt/col-type->json-type mysql-tt "decimal")]
    (is (= (type real-type) jsonschema.type_system.types.Real))
    (is (= (types/getMin real-type) mysql-db-types/DECIMAL_MIN))
    (is (= (types/getMax real-type) mysql-db-types/DECIMAL_MAX))))

(deftest mysql-num24->json-type-test []
  (let [real-type (dt/col-type->json-type mysql-tt "numeric(2,4)")]
    (is (= (type real-type) jsonschema.type_system.types.Real))
    (is (= (types/getMin real-type) mysql-db-types/DECIMAL_MIN))
    (is (= (types/getMax real-type) mysql-db-types/DECIMAL_MAX))))

(deftest mysql-num24->json-type-test []
  (let [real-type (dt/col-type->json-type mysql-tt "numeric")]
    (is (= (type real-type) jsonschema.type_system.types.Real))
    (is (= (types/getMin real-type) mysql-db-types/DECIMAL_MIN))
    (is (= (types/getMax real-type) mysql-db-types/DECIMAL_MAX))))

;; FLOAT & DOUBLE
(deftest mysql-float->json-type-test []
  (let [real-type (dt/col-type->json-type mysql-tt "float")]
    (is (= (type real-type) jsonschema.type_system.types.Real))
    (is (= (types/getMin real-type) mysql-db-types/DECIMAL_MIN))
    (is (= (types/getMax real-type) mysql-db-types/DECIMAL_MAX))))

(deftest mysql-ufloat->json-type-test []
  (let [real-type (dt/col-type->json-type mysql-tt "float unsigned")]
    (is (= (type real-type) jsonschema.type_system.types.Real))
    (is (= (types/getMin real-type) mysql-db-types/DECIMAL_MIN))
    (is (= (types/getMax real-type) mysql-db-types/DECIMAL_MAX))))

(deftest mysql-double->json-type-test []
  (let [real-type (dt/col-type->json-type mysql-tt "double")]
    (is (= (type real-type) jsonschema.type_system.types.Real))
    (is (= (types/getMin real-type) mysql-db-types/DECIMAL_MIN))
    (is (= (types/getMax real-type) mysql-db-types/DECIMAL_MAX))))

(deftest mysql-udouble->json-type-test []
  (let [real-type (dt/col-type->json-type mysql-tt "double unsigned")]
    (is (= (type real-type) jsonschema.type_system.types.Real))
    (is (= (types/getMin real-type) mysql-db-types/DECIMAL_MIN))
    (is (= (types/getMax real-type) mysql-db-types/DECIMAL_MAX))))

;; CHAR & VARCHAR
(deftest mysql-char->json-type-test []
  (let [str-type (dt/col-type->json-type mysql-tt "char(10)")]
    (is (= (type str-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin str-type) 0))
    (is (= (types/getMax str-type) 10))))

(deftest mysql-varchar->json-type-test []
  (let [str-type (dt/col-type->json-type mysql-tt "varchar(255)")]
    (is (= (type str-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin str-type) 0))
    (is (= (types/getMax str-type) 255))))

(deftest mysql-blob->json-type-test []
  (let [str-type (dt/col-type->json-type mysql-tt "blob")]
    (is (= (type str-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin str-type) 0))
    (is (= (types/getMax str-type) 65535))))

(deftest mysql-text->json-type-test []
  (let [str-type (dt/col-type->json-type mysql-tt "text")]
    (is (= (type str-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin str-type) 0))
    (is (= (types/getMax str-type) 65535))))

(deftest mysql-enum->json-type-test []
  (let [str-type (dt/col-type->json-type mysql-tt "enum")]
    (is (= (type str-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin str-type) 0))
    (is (= (types/getMax str-type) 65535))))

(deftest mysql-set->json-type-test []
  (let [str-type (dt/col-type->json-type mysql-tt "set")]
    (is (= (type str-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin str-type) 0))
    (is (= (types/getMax str-type) 65535))))

;; DATE TYPES
(deftest mysql-date->json-type-test []
  (let [date-type (dt/col-type->json-type mysql-tt "date")]
    (is (= (type date-type) jsonschema.type_system.types.Date))
    (is (= (types/getFormats date-type)) "yyyy-MM-dd")))

(deftest mysql-datetime->json-type-test []
  (let [date-type (dt/col-type->json-type mysql-tt "datetime")]
    (is (= (type date-type) jsonschema.type_system.types.Date))
    (is (= (types/getFormats date-type)) "yyyy-MM-dd HH:mm:ss")))

(deftest mysql-timestamp->json-type-test []
  (let [date-type (dt/col-type->json-type mysql-tt "timestamp")]
    (is (= (type date-type) jsonschema.type_system.types.Date))
    (is (= (types/getFormats date-type)) "yyyy-MM-dd HH:mm:ss")))

;; BOOL
(deftest mysql-bool->json-type-test []
  (let [bool-type (dt/col-type->json-type mysql-tt "bool")]
    (is (= (type bool-type) jsonschema.type_system.types.Bool))))

(deftest mysql-boolean->json-type-test []
  (let [bool-type (dt/col-type->json-type mysql-tt "boolean")]
    (is (= (type bool-type) jsonschema.type_system.types.Bool))))

;; BIT
(deftest mysql-bit->json-type-test []
  (let [int-type (dt/col-type->json-type mysql-tt "bit")]
    (is (= (type int-type) jsonschema.type_system.types.Int))
    (is (= (types/getMin int-type) 1))
    (is (= (types/getMax int-type) 64))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REVERSE MAPPING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest schema-int->mysql-int []
  (let [schema-int (types/make-int 1024)
        mysql-int (dt/json-type->col-type mysql-tt schema-int)]
    (is (= mysql-int "int"))))

(deftest schema-int->mysql-bigint []
  (let [schema-int (types/make-int 2147483648)
        mysql-int (dt/json-type->col-type mysql-tt schema-int)]
    (is (= mysql-int "bigint"))))

(deftest schema-int->mysql-str []
  (let [schema-str (types/make-str 1024 1024)
        mysql-str (dt/json-type->col-type mysql-tt schema-str)]
    (is (= mysql-str "varchar(1024)"))))

(deftest schema-bool->mysql->bool []
  (let [schema-bool (types/make-bool)
        mysql-bool (dt/json-type->col-type mysql-tt schema-bool)]
    (is (= mysql-bool "bool"))))

(deftest schema-real->mysql-real []
  (let [schema-real (types/make-real 1024 1024)
        mysql-real (dt/json-type->col-type mysql-tt schema-real)]
    (is (= mysql-real "decimal"))))
