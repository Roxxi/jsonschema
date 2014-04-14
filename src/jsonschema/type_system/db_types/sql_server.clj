(ns jsonschema.type-system.db-types.sql-server
  "Type translation from database column types to JSON schema types"
  {:author "Shawn Shah"
   :date "2/10/2014"}
  (:require [clojure.string :as string]
            [clojure.math.numeric-tower :as math]
            [roxxi.utils.print :refer [print-expr]]
            [jsonschema.type-system.types :as json-types]
            [jsonschema.type-system.db-types.common :as db-common]
            [jsonschema.type-system.db-types.translator :as dt]
            [slingshot.slingshot :as slingshot]))

;; SQL Server (T-SQL) Type Conversions

;; type mapping
(def col-type->json-type-kw
  {
   "tinyint" :int
   "smallint" :int
   "int" :int
   "bigint" :int
   "rowversion" :int
   "timestamp" :int
   "real" :real
   "float" :real
   "decimal" :real
   "numeric" :real
   "money" :real
   "smallmoney" :real
   "date" :date
   "datetime" :date
   "datetime2" :date
   "smalldatetime" :date
   "datetimeoffset" :date
   "time" :date
   "char" :str
   "varchar" :str
   "nchar" :str
   "nvarchar" :str
   "binary" :str
   "varbinary" :str
   "text" :str
   "ntext" :str
   "image" :str
   "uniqueidentifier" :str
   "xml" :str
 })

(defmulti col-map->json-type
  "Multimethod. Takes a map with column attributes
like {:json-type :int :mysql-type-kw :int_unsigned :col-length 10}"
  (fn [col-map]
    (:json-type col-map)))

;;;;;;;;;;;;;;;; INTEGER TYPES ;;;;;;;;;;;;;;;;;;;;;;;;

;; SQL-Server Integer Type definitions
;; (see: http://msdn.microsoft.com/en-us/library/ms187745.aspx)

;; INT
;; BIGINT
;; SMALLINT
;; TINYINT

;; ROWVERSION/TIMESTAMP
;; (see: http://msdn.microsoft.com/en-us/library/ms182776.aspx)

(def int-synonym->int-type-kw
  {
   :rowversion :int
   :timestamp :int
   })

(defn- translate-int-type [col-type-kw]
  "If binary type is a synonym, translate to canonical type.
Otherwise, pass type argument through."
  (if (contains? int-synonym->int-type-kw col-type-kw)
    (get int-synonym->int-type-kw col-type-kw)
    col-type-kw))

(def int-type-kw->min-max
  {
   :tinyint {:min 0 :max 255}
   :smallint {:min (math/expt -2 15) :max (- (math/expt 2 15) 1)}
   :int {:min (math/expt -2 31) :max (- (math/expt 2 31) 1)}
   :bigint {:min (math/expt -2 63) :max (- (math/expt 2 63) 1)}
   })

(defmethod col-map->json-type :int [col-map]
  (let [translated-col-type (translate-int-type (:col-type-kw col-map))
        min-max (translated-col-type int-type-kw->min-max)]
    (json-types/make-int (:min min-max) (:max min-max))))

;;;;;;;;;;;;;;;; STRING & BINARY TYPES ;;;;;;;;;;;;;;;;;;;;;;;;

;; (see: http://msdn.microsoft.com/en-us/library/ms176089.aspx)
;; CHARACTER
;; CHAR
;; VARCHAR
;; NCHAR
;; NVARCHAR
;; BINARY
;; VARBINARY

;; IMAGE (deprecated)
;; NTEXT (deprecated)
;; TEXT (deprecated)

;; UNIQUEIDENTIFIER
;; (see: http://msdn.microsoft.com/en-us/library/ms187942.aspx)

;; XML
;; (see: http://msdn.microsoft.com/en-us/library/ms187339.aspx)

(def str-synonym->str-type-kw
  {
   :character :varchar
   :char :varchar
   :nchar :varchar
   :nvarchar :varchar
   :binary :varchar
   :varbinary :varchar
   :uniqueidentifier :varchar
   :image :text
   :xml :text
   })

(def str-type-kw->default-min-max-length
  {
   :varchar {:default 1 :min 1 :max 8000}
   :text (zipmap [:default :min :max] (repeat 3 (- (math/expt 2 31) 1)))
   :ntext (zipmap [:default :min :max] (repeat 3 (- (math/expt 2 30) 1)))
   })

;; Multimethods for string column parameter handling.

(defmulti str-col-map->default-min-max
  (fn [x]
    (:col-type-kw x)))

;; Column parameters, if present, are hints to modify the
;; default limits of the column. For example, the n parameter
;; to varchar(n) will extend the default maximum length to n.
;; n can also be an arbitrary string like "max", so a global
;; assumption about the type of column parameters cannot be made.
;; As such, a dispatch method may exist for each type to do
;; special handling of the column parameters and modify
;; the column limits. Otherwise, the default column limits will pass through.

(defmethod str-col-map->default-min-max :varchar [col-map]
  "Special handling for varchar length parameter"
  (let [default-min-max (str-type-kw->default-min-max-length
                         (:col-type-kw col-map))
        col-param-or-nil (db-common/str-or-nil->int-or-nil (:col-length col-map))]
    (condp = col-param-or-nil
      (symbol "max") (assoc default-min-max :max (- (math/expt 2 31) 1))
       nil default-min-max
       (assoc default-min-max :max (db-common/coalesce-with-limit
                                    col-param-or-nil
                                    (:default default-min-max)
                                    (:max default-min-max))))))

(defmethod str-col-map->default-min-max :default [col-map]
  "Special handling for varchar length parameter"
  (let [default-min-max (str-type-kw->default-min-max-length
                         (:col-type-kw col-map))]
    default-min-max))

(defmethod str-col-map->default-min-max :default [col-map]
  "Default behavior, treat column length as integer, apply to column max"
  (let [default-min-max (str-type-kw->default-min-max-length
                         (:col-type-kw col-map))
        n-col-param (db-common/str-or-nil->int-or-nil (:col-length col-map))
        coalesced-max (db-common/coalesce-with-limit
                       n-col-param
                       (:default default-min-max)
                       (:max default-min-max))]
    {:min 1 :max coalesced-max}))

;; End multimethods for string parameter handling ;;

(defn- translate-str-type [col-type-kw]
  "If string type is a synonym, translate to canonical type.
Otherwise, pass binary type through."
  (if (contains? str-synonym->str-type-kw col-type-kw)
    (get str-synonym->str-type-kw col-type-kw)
    col-type-kw))

(defmethod col-map->json-type :str [col-map]
  (let [str-type-kw (translate-str-type (:col-type-kw col-map))
        translated-col-map (assoc col-map :col-type-kw str-type-kw)
        col-limits (str-col-map->default-min-max translated-col-map)]
    (json-types/make-str (:min col-limits) (:max col-limits))))

;;;;;;;;;;;;;;;; DATE TYPES ;;;;;;;;;;;;;;;;;;;;;;;;

;; Date & Time (http://msdn.microsoft.com/en-us/library/ff848733.aspx)
;; DATE
;; DATETIME
;; DATETIME2
;; DATETIMEOFFSET
;; SMALLDATETIME

;; Note: TIME has a slightly different date format (number of microseconds shown)
;; based on number of bytes requested
;; TIME

(def date-synonym->date-type-kw {})

(def date-type-kw->date-format-patterns
  {
   :date ["YYYY-MM-DD"]
   :datetime ["yyyy-MM-dd HH:mm:ss"]
   :datetime2 ["yyyy-MM-dd HH:mm:ss.SSSSSSS"]
   :datetimeoffset ["yyyy-MM-dd HH:mm:ss.SSSSSSSXXX"]
   :smalldatetime ["yyyy-MM-dd HH:mm:00"]
   :time ["HH:mm:ss.SSSSSSS"]
   })

;; TODO: move all these translation behavior into
;; a more type-agnostic function
(defn- translate-date-type [col-type-kw]
  "If date type is a synonym, translate to canonical type.
Otherwise, pass date type through."
  (if (contains? date-synonym->date-type-kw col-type-kw)
    (get date-synonym->date-type-kw col-type-kw)
    col-type-kw))

(defmethod col-map->json-type :date [col-map]
  (let [date-type-kw (translate-date-type  (:col-type-kw col-map))
        date-format-patterns (date-type-kw date-type-kw->date-format-patterns)]
    (json-types/make-date date-format-patterns)))

;;;;;;;;;;;;;;;; REAL TYPES ;;;;;;;;;;;;;;;;;;;;;;;;

;; Approximates
;; (see: http://msdn.microsoft.com/en-us/library/ms173773.aspx)
;; FLOAT
;; REAL

;; Decimal & Numeric
;; (see: http://msdn.microsoft.com/en-us/library/ms187746.aspx)
;; DECIMAL
;; NUMERIC

;; Money & Smallmoney
;; (see: http://msdn.microsoft.com/en-us/library/ms179882.aspx)
;; MONEY
;; SMALLMONEY

(def real-type-kw->min-max
  {
   :float {:min  -1.79E308 :max 1.79E308}
   :real {:min -3.40E38 :max  3.40E38}
   :decimal {:min (+ (math/expt -10 38) 1) :max (- (math/expt 10 38) 1)}
   :money {:min -922337203685477.5808 :max 922337203685477.5807}
   :smallmoney {:min -214748.3648 :max 214748.3647}
   })

;; TODO clean this up to avoid repeated typing of the same kws
;; maybe something like:
;;    :double [:float :float8 :real]
;;    :numeric [:decimal :number :money :interval]
(def real-synonym->real-type-kw
  {
   :numeric :decimal
   })

;; TODO: refactor duplicated logic with other translate-*-type functions
(defn- translate-real-type [col-type-kw]
  "If real type is a synonym, translate to canonical type.
Otherwise, pass real type through."
  (if (contains? real-synonym->real-type-kw col-type-kw)
    (get real-synonym->real-type-kw col-type-kw)
    col-type-kw))

(defmethod col-map->json-type :real [col-map]
  (let [real-type-kw (translate-real-type (:col-type-kw col-map))
        min-max (real-type-kw real-type-kw->min-max)]
    (json-types/make-real (:min min-max) (:max min-max))))

;;;;;;;;;;;;;;;;;;;;;;;; UNIMPLEMENTED "OTHER" TYPES ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CURSOR
;; HIERARCHYID
;; SQLVARIANT
;; GEOGRAPHY
;; GEOMETRY

;
;; (defn col-type->json-type [^String col-def-str]
;;   "Transform a mysql type string (i.e. 'int(10) unsigned') into a JSONSchema type"
;;   (let [col-map (db-common/col-def-str->col-map col-def-str col-type->json-type-kw)]
;;     (col-map->json-type (print-expr col-map))))

(deftype SqlServerTypeTranslator []
    dt/DBTypeTranslator
  (col-type->json-type [_ col-def-str]
    "Transform a mysql type string (i.e. 'int(10) unsigned') into a JSONSchema type"
    (let [col-map (db-common/col-def-str->col-map col-def-str col-type->json-type-kw)]
      (col-map->json-type col-map))))

(defn make-sql-server-type-translator []
  (SqlServerTypeTranslator. ))
