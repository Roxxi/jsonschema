(ns jsonschema.sql-server-types-test
  (:use clojure.test
        roxxi.utils.print
        jsonschema.transform)
  (:require [clojure.math.numeric-tower :as math]
            [jsonschema.type-system.types :as types]
            [jsonschema.type-system.db-types.sql-server :as sqls-types]))

(def sql-server-tt (sql-types/make-sql-server-type-translator))

;;;;;;;;;;;;;;;; INTEGER TYPES ;;;;;;;;;;;;;;;;;;;;;;;;

;; (see: http://msdn.microsoft.com/en-us/library/ms187745.aspx)

;; INT
;; BIGINT
;; SMALLINT
;; TINYINT

(deftest sqls-tinyint->json-type-test []
  (let [int-type (sqls-types/col-type->json-type "tinyint")]
    (is (= (type int-type) jsonschema.type_system.types.Int))
    (is (= (types/getMin int-type) 0))
    (is (= (types/getMax int-type) 255))))

(deftest sqls-smallint->json-type-test []
  (let [int-type (sqls-types/col-type->json-type "smallint")]
    (is (= (type int-type) jsonschema.type_system.types.Int))
    (is (= (types/getMin int-type) (math/expt -2 15)))
    (is (= (types/getMax int-type) (- (math/expt 2 15) 1)))))

(deftest sqls-int->json-type-test []
  (let [int-type (sqls-types/col-type->json-type "int")]
    (is (= (type int-type) jsonschema.type_system.types.Int))
    (is (= (types/getMin int-type) (math/expt -2 31)))
    (is (= (types/getMax int-type) (- (math/expt 2 31) 1)))))

(deftest sqls-bigint->json-type-test []
  (let [int-type (sqls-types/col-type->json-type "bigint")]
    (is (= (type int-type) jsonschema.type_system.types.Int))
    (is (= (types/getMin int-type) (math/expt -2 63)))
    (is (= (types/getMax int-type) (- (math/expt 2 63) 1)))))

;;;;;;;;;;;;;;;; NUMERICS ;;;;;;;;;;;;;;;;;;;;;;;;

;; (see: http://msdn.microsoft.com/en-us/library/ms173773.aspx)
;; FLOAT
;; REAL

(deftest sqls-float->json-type-test []
  (let [num-type (sqls-types/col-type->json-type "float")]
    (is (= (type num-type) jsonschema.type_system.types.Real))
    (is (= (types/getMin num-type) -1.79E308))
    (is (= (types/getMax num-type) 1.79E308))))

(deftest sqls-real->json-type-test []
  (let [num-type (sqls-types/col-type->json-type "real")]
    (is (= (type num-type) jsonschema.type_system.types.Real))
    (is (= (types/getMin num-type) -3.40E38))
    (is (= (types/getMax num-type) 3.40E38))))

(deftest sqls-decimal->json-type-test []
  (let [num-type (sqls-types/col-type->json-type "decimal")]
    (is (= (type num-type) jsonschema.type_system.types.Real))
    (is (= (types/getMin num-type) (+ (math/expt -10 38) 1)))
    (is (= (types/getMax num-type) (- (math/expt 10 38) 1)))))

(deftest sqls-money->json-type-test []
  (let [num-type (sqls-types/col-type->json-type "money")]
    (is (= (type num-type) jsonschema.type_system.types.Real))
    (is (= (types/getMin num-type) -922337203685477.5808))
    (is (= (types/getMax num-type) 922337203685477.5807))))

(deftest sqls-smallmoney->json-type-test []
  (let [num-type (sqls-types/col-type->json-type "smallmoney")]
    (is (= (type num-type) jsonschema.type_system.types.Real))
    (is (= (types/getMin num-type) -214748.3648))
    (is (= (types/getMax num-type) 214748.3647))))

;;;;;;;;;;;;;;;;;;;;;;;; DATES ;;;;;;;;;;;;;;;;;;;;;;;;

;; Date & Time (see: http://msdn.microsoft.com/en-us/library/ff848733.aspx)
;; DATE
;; DATETIME
;; DATETIME2
;; DATETIMEOFFSET
;; SMALLDATETIME

(deftest sqls-date->json-type-test []
  (let [date-type (sqls-types/col-type->json-type "date")]
    (is (= (type date-type) jsonschema.type_system.types.Date))
    (is (= (types/getFormats date-type) #{"YYYY-MM-DD"}))))

(deftest sqls-datetime->json-type-test []
  (let [date-type (sqls-types/col-type->json-type "datetime")]
    (is (= (type date-type) jsonschema.type_system.types.Date))
    (is (= (types/getFormats date-type) #{"yyyy-MM-dd HH:mm:ss"}))))

(deftest sqls-datetime2->json-type-test []
  (let [date-type (sqls-types/col-type->json-type "datetime2")]
    (is (= (type date-type) jsonschema.type_system.types.Date))
    (is (= (types/getFormats date-type) #{"yyyy-MM-dd HH:mm:ss.SSSSSSS"}))))

(deftest sqls-datetimeoffset->json-type-test []
  (let [date-type (sqls-types/col-type->json-type "datetimeoffset")]
    (is (= (type date-type) jsonschema.type_system.types.Date))
    (is (= (types/getFormats date-type) #{"yyyy-MM-dd HH:mm:ss.SSSSSSSXXX"}))))

(deftest sqls-smalldatetime->json-type-test []
  (let [date-type (sqls-types/col-type->json-type "smalldatetime")]
    (is (= (type date-type) jsonschema.type_system.types.Date))
    (is (= (types/getFormats date-type) #{"yyyy-MM-dd HH:mm:00"}))))

(deftest sqls-time->json-type-test []
  (let [date-type (sqls-types/col-type->json-type "time")]
    (is (= (type date-type) jsonschema.type_system.types.Date))
    (is (= (types/getFormats date-type) #{"HH:mm:ss.SSSSSSS"}))))

;;;;;;;;;;;;;;;; STRING & BINARY TYPES ;;;;;;;;;;;;;;;;;;;;;;;;
;; (see: http://msdn.microsoft.com/en-us/library/ff848814.aspx)

(def MAX_30_BIT_VALUE (- (math/expt 2 31) 1))
(def MAX_29_BIT_VALUE (- (math/expt 2 30) 1))

;; CHAR
(deftest sqls-char->json-type-test []
  (let [json-type (sqls-types/col-type->json-type "char")]
    (is (= (type json-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin json-type) 1))
    (is (= (types/getMax json-type) 8000))))

(deftest sqls-char10->json-type-test []
  (let [json-type (sqls-types/col-type->json-type "char(10)")]
    (is (= (type json-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin json-type) 1))
    (is (= (types/getMax json-type) 10))))

(deftest sqls-char9999->json-type-test []
  (let [json-type (sqls-types/col-type->json-type "char(9999)")]
    (is (= (type json-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin json-type) 1))
    (is (= (types/getMax json-type) 8000))))

;; VARCHAR
(deftest sqls-varchar->json-type-test []
  (let [json-type (sqls-types/col-type->json-type "varchar")]
    (is (= (type json-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin json-type) 1))
    (is (= (types/getMax json-type) 8000))))

(deftest sqls-varchar10->json-type-test []
  (let [json-type (sqls-types/col-type->json-type "varchar(10)")]
    (is (= (type json-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin json-type) 1))
    (is (= (types/getMax json-type) 10))))

(deftest sqls-varchar9999->json-type-test []
  (let [json-type (sqls-types/col-type->json-type "varchar(9999)")]
    (is (= (type json-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin json-type) 1))
    (is (= (types/getMax json-type) 8000))))


(deftest sqls-varchar9999->json-type-test []
  (let [json-type (sqls-types/col-type->json-type "varchar(max)")]
    (is (= (type json-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin json-type) 1))
    (is (= (types/getMax json-type) (- (math/expt 2 31) 1)))))

;;NCHAR

(deftest sqls-nchar->json-type-test []
  (let [json-type (sqls-types/col-type->json-type "nchar")]
    (is (= (type json-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin json-type) 1))
    (is (= (types/getMax json-type) 8000))))

(deftest sqls-nchar10->json-type-test []
  (let [json-type (sqls-types/col-type->json-type "nchar(10)")]
    (is (= (type json-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin json-type) 1))
    (is (= (types/getMax json-type) 10))))

(deftest sqls-nchar9999->json-type-test []
  (let [json-type (sqls-types/col-type->json-type "nchar(9999)")]
    (is (= (type json-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin json-type) 1))
    (is (= (types/getMax json-type) 8000))))

(deftest sqls-ncharmax->json-type-test []
  (let [json-type (sqls-types/col-type->json-type "nchar(max)")]
    (is (= (type json-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin json-type) 1))
    (is (= (types/getMax json-type) MAX_30_BIT_VALUE))))

;; NVARCHAR

(deftest sqls-nvarchar->json-type-test []
  (let [json-type (sqls-types/col-type->json-type "nvarchar")]
    (is (= (type json-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin json-type) 1))
    (is (= (types/getMax json-type) 8000))))

(deftest sqls-nvarchar10->json-type-test []
  (let [json-type (sqls-types/col-type->json-type "nvarchar(10)")]
    (is (= (type json-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin json-type) 1))
    (is (= (types/getMax json-type) 10))))

(deftest sqls-nvarchar9999->json-type-test []
  (let [json-type (sqls-types/col-type->json-type "nvarchar(9999)")]
    (is (= (type json-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin json-type) 1))
    (is (= (types/getMax json-type) 8000))))

(deftest sqls-nvarcharmax->json-type-test []
  (let [json-type (sqls-types/col-type->json-type "nvarchar(max)")]
    (is (= (type json-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin json-type) 1))
    (is (= (types/getMax json-type) MAX_30_BIT_VALUE))))

;;BINARY

(deftest sqls-binary->json-type-test []
  (let [json-type (sqls-types/col-type->json-type "binary")]
    (is (= (type json-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin json-type) 1))
    (is (= (types/getMax json-type) 8000))))

(deftest sqls-binary10->json-type-test []
  (let [json-type (sqls-types/col-type->json-type "binary(10)")]
    (is (= (type json-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin json-type) 1))
    (is (= (types/getMax json-type) 10))))

(deftest sqls-binary9999->json-type-test []
  (let [json-type (sqls-types/col-type->json-type "binary(9999)")]
    (is (= (type json-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin json-type) 1))
    (is (= (types/getMax json-type) 8000))))

;; VARBINARY

(deftest sqls-varbinary->json-type-test []
  (let [json-type (sqls-types/col-type->json-type "varbinary")]
    (is (= (type json-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin json-type) 1))
    (is (= (types/getMax json-type) 8000))))

(deftest sqls-varbinary10->json-type-test []
  (let [json-type (sqls-types/col-type->json-type "varbinary(10)")]
    (is (= (type json-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin json-type) 1))
    (is (= (types/getMax json-type) 10))))

(deftest sqls-varbinary9999->json-type-test []
  (let [json-type (sqls-types/col-type->json-type "varbinary(9999)")]
    (is (= (type json-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin json-type) 1))
    (is (= (types/getMax json-type) 8000))))

(deftest sqls-varbinarymax->json-type-test []
  (let [json-type (sqls-types/col-type->json-type "varbinary(max)")]
    (is (= (type json-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin json-type) 1))
    (is (= (types/getMax json-type) MAX_30_BIT_VALUE))))

;; TEXT
(deftest sqls-text->json-type-test []
  (let [json-type (sqls-types/col-type->json-type "text")]
    (is (= (type json-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin json-type) 1))
    (is (= (types/getMax json-type) MAX_30_BIT_VALUE))))

(deftest sqls-image->json-type-test []
  (let [json-type (sqls-types/col-type->json-type "image")]
    (is (= (type json-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin json-type) 1))
    (is (= (types/getMax json-type) MAX_30_BIT_VALUE))))

(deftest sqls-ntext->json-type-test []
  (let [json-type (sqls-types/col-type->json-type "ntext")]
    (is (= (type json-type) jsonschema.type_system.types.Str))
    (is (= (types/getMin json-type) 1))
    (is (= (types/getMax json-type) MAX_29_BIT_VALUE))))

;;;;;;;;;;;;;;;; OTHER TYPES ;;;;;;;;;;;;;;;;;;;;;;;;
;; (see: http://msdn.microsoft.com/en-us/library/ms187745.aspx)

;; ROWVERSION

(deftest sqls-rowversion->json-type-test []
  (let [json-type (sqls-types/col-type->json-type "rowversion")]
    (is (= (type json-type) jsonschema.type_system.types.Int))))

;; TIMESTAMP

(deftest sqls-timestamp->json-type-test []
  (let [json-type (sqls-types/col-type->json-type "timestamp")]
    (is (= (type json-type) jsonschema.type_system.types.Int))))
