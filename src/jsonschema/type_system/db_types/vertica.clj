(ns jsonschema.type-system.db-types.vertica
  "Type translation from database column types to JSON schema types"
  {:author "Shawn Shah"
   :date "2/3/2014"}
  (:require [clojure.string :as string]
            [clojure.math.numeric-tower :as math]
            [roxxi.utils.print :refer [print-expr]]
            [jsonschema.type-system.types :as json-types]
            [jsonschema.type-system.db-types.common :as db-common]
            [jsonschema.type-system.db-types.translator :as dt]
            [slingshot.slingshot :as slingshot]))

;; Vertica 6 Type Conversions


;; TODO: clean this up to avoid duplicated typing
;; more like:
;;    :int ["tinyint" "smallint" ...]
;;    :binary ["binary" "varbinary" ...]
(def col-type->json-type-kw
  {
   "tinyint" :int
   "smallint" :int
   "int8" :int
   "int" :int
   "integer" :int
   "bigint" :int
   "boolean" :bool
   "binary" :binary
   "varbinary" :binary
   "bytea" :binary
   "raw" :binary
   "character" :str
   "char" :str
   "varchar" :str
   "date" :date
   "datetime" :date
   "timestamp" :date
   "timestamptz" :date
   "interval" :real
   "smalldatetime" :date
   "double" :real
   "float" :real
   "float8" :real
   "real" :real
   "number" :real
   "money" :real
   "decimal" :real
   "numeric" :real
 })

(defmulti col-map->json-type
  "Multimethod. Takes a map with column attributes
like {:json-type :int :mysql-type-kw :int_unsigned :col-length 10}"
  (fn [col-map]
    (:json-type col-map)))

;;;;;;;;;;;;;;;; INTEGER TYPES ;;;;;;;;;;;;;;;;;;;;;;;;

;; INTEGER
;; INT
;; BIGINT
;; INT8
;; SMALLINT
;; TINYINT
;; type mapping

;; "INT, INTEGER, INT8, SMALLINT, TINYINT, and BIGINT are all synonyms
;; for the same signed 64-bit integer data type"
(def SIGNED_64_INT_MAX (- (math/expt 2 63) 1))
(def SIGNED_64_INT_MIN (+ (* -1 (math/expt 2 63)) 1))

(defmethod col-map->json-type :int [col-map]
  (json-types/make-int SIGNED_64_INT_MIN SIGNED_64_INT_MAX))

;;;;;;;;;;;;;;;; BINARY TYPES ;;;;;;;;;;;;;;;;;;;;;;;;

;; BINARY
;; VARBINARY
;; BYTEA
;; RAW

;; TODO: Look into UTF-8 conversion issues (probably rare) here
;; since String deals with length, and BINARY deals
;; with bytes
(def DEFAULT_BINARY_LENGTH 1)
(def DEFAULT_VARBINARY_LENGTH 80)
(def MAX_BINARY_LENGTH 65000)

;; (from Vertica 6 "BYTEA and RAW are synonyms for VARBINARY.")
(def bin-synonym->bin-type-kw
  {
   :bytea :varbinary
   :raw :varbinary
   })

(defn- translate-bin-type [col-type-kw]
  "If binary type is a synonym, translate to canonical type.
Otherwise, pass binary type through."
  (if (contains? bin-synonym->bin-type-kw col-type-kw)
    (get bin-synonym->bin-type-kw col-type-kw)
    col-type-kw))

(defn- bin-type-length->max-length [col-type-kw col-length]
  "Determine correct column length given binary type and provided length"
  (cond
   (= col-type-kw :binary) (db-common/coalesce-with-limit
                            col-length DEFAULT_BINARY_LENGTH MAX_BINARY_LENGTH)
   (= col-type-kw :varbinary) (db-common/coalesce-with-limit
                               col-length DEFAULT_VARBINARY_LENGTH MAX_BINARY_LENGTH)
   :else nil))

(defmethod col-map->json-type :binary [col-map]
  (let [bin-type-kw (translate-bin-type (:col-type-kw col-map))
        n-col-length (db-common/str-or-nil->int-or-nil (:col-length col-map))
        bin-max-length (bin-type-length->max-length
                        bin-type-kw n-col-length)]
    (json-types/make-str bin-max-length bin-max-length)))

;;;;;;;;;;;;;;;; BOOLEAN TYPES ;;;;;;;;;;;;;;;;;;;;;;;;
;; BOOLEAN

(defmethod col-map->json-type :bool [col-map]
  (json-types/make-bool))

;;;;;;;;;;;;;;;; STRING TYPES ;;;;;;;;;;;;;;;;;;;;;;;;

;; CHARACTER
;; CHAR
;; VARCHAR
(def str-synonyms
  {
   :character :char
   })

(def DEFAULT_CHAR_LENGTH 1)
(def DEFAULT_VARCHAR_LENGTH 80)
(def MAX_CHAR_LENGTH 65000) ;; also VARCHAR MAX

(def str-type-kw->min-max
  {
   :char [DEFAULT_CHAR_LENGTH MAX_CHAR_LENGTH]
   :varchar [DEFAULT_VARCHAR_LENGTH MAX_CHAR_LENGTH]
   })

(defmethod col-map->json-type :str [col-map]
  (let [n-col-length (db-common/str-or-nil->int-or-nil (:col-length col-map))
        str-type-kw (db-common/translate-type
                     (:col-type-kw col-map) str-synonyms)
        [default-length max-length]
        (str-type-kw->min-max str-type-kw)
        json-str-length (db-common/coalesce-with-limit
                         n-col-length default-length max-length)]
    (json-types/make-str json-str-length json-str-length)))

;;;;;;;;;;;;;;;; DATE TYPES ;;;;;;;;;;;;;;;;;;;;;;;;

;; DATE
;; "'1999-01-08' Iso 8601; January 8 in any mode (recommended format)"
;; (see: https://my.vertica.com/docs/6.1.x/HTML/index.htm#9256.htm)

;; DATETIME (synonym of TIMESTAMP)
;; SMALLDATETIME (synonym of TIMESTAMP)
;; TIME
;; TIMESTAMP
;; INTERVAL
;; (we will alias INTERVAL to a Real type instead since it's
;; measuring a difference between two times)

(def date-synonyms
  {
   :datetime :timestamp
   :smalldatetime :timestamp
   :timestamptz :timestamp
   })

(def date-type-kw->date-format-patterns
  {
   :date ["yyyy-MM-dd"]
   :timestamp ["yyyy-MM-dd HH:mm:ss" "yyyy-MM-dd HH:mm:ssZ"]
   :time ["HH:mm:ss" "HH:mm:ssZ"]
   })

;; TODO: move all these translation behavior into
;; a more type-agnostic function
;; (defn- translate-date-type [col-type-kw]
;;   "If date type is a synonym, translate to canonical type.
;; Otherwise, pass date type through."
;;   (if (contains? date-synonym->date-type-kw col-type-kw)
;;     (get date-synonym->date-type-kw col-type-kw)
;;     col-type-kw))

(defmethod col-map->json-type :date [col-map]
  (let [date-type-kw (db-common/translate-type
                      (:col-type-kw col-map) date-synonyms)
        date-format-patterns (date-type-kw date-type-kw->date-format-patterns)]
    (json-types/make-date date-format-patterns)))

;;;;;;;;;;;;;;;; REAL TYPES ;;;;;;;;;;;;;;;;;;;;;;;;

;; "All of the DOUBLE PRECISION data types are synonyms for 64-bit IEEE 754 FLOAT."
;; (see: https://my.vertica.com/docs/6.1.x/HTML/index.htm#2587.htm)
;; DOUBLE PRECISION
;; FLOAT
;; FLOAT(n)
;; FLOAT8
;; REAL

;; "NUMERIC, DECIMAL, NUMBER, and MONEY are all synonyms that return NUMERIC types."
;; (see; https://my.vertica.com/docs/6.1.x/HTML/index.htm#12295.htm)
;; DECIMAL
;; NUMERIC
;; NUMBER
;; MONEY

;; TODO clean this up to avoid repeated typing of the same kws
;; maybe something like:
;;    :double [:float :float8 :real]
;;    :numeric [:decimal :number :money :interval]
(def real-synonyms
  {
   :float :double
   :float8 :double
   :real :double
   :decimal :numeric
   :number :numeric
   :money :numeric
   :interval :numeric
   :numeric :numeric
   })

;; ;; TODO: refactor duplicated logic with other translate-*-type functions
;; (defn- translate-real-type [col-type-kw]
;;   "If real type is a synonym, translate to canonical type.
;; Otherwise, pass real type through."
;;   (if (contains? real-synonym->real-type-kw col-type-kw)
;;     (get real-synonym->real-type-kw col-type-kw)
;;     col-type-kw))

;; The JVM also uses 64-bit IEEE 754 standard for its Double implementation
;; Let's piggyback on that for now
;; TODO: move these into a common utility module
(def MAX_IEEE_754_FLOAT (. Double MIN_VALUE))
(def MIN_IEEE_754_FLOAT (. Double MAX_VALUE))

;; NUMERIC(p,s) - p is the precision (total number of digits) and s is the maximum scale
;; (number of decimal places).
;;
;; precision - "The total number of significant digits that the data type stores.
;; precision must be positive and <= 1024. If you assign a value that exceeds the precision value,
;; an error occurs."
;;
;; scale - "The maximum number of digits to the right of the decimal point that
;; the data type stores. scale must be non-negative and less than or equal to
;; precision. If you omit the scale parameter, the scale value is set to 0. If
;; you assign a value with more decimal digits than scale, the value is rounded
;; to scale digits."
(def MAX_NUMERIC_VALUE (read-string (string/join "" (repeat 1024 "9"))))
(def MIN_NUMERIC_VALUE (* -1 MAX_NUMERIC_VALUE))

(def real-type-kw->min-max
  {
   :double {:min MIN_IEEE_754_FLOAT :max MAX_IEEE_754_FLOAT}
   :numeric {:min MIN_NUMERIC_VALUE :max MAX_NUMERIC_VALUE}
   })

(defmethod col-map->json-type :real [col-map]
  (let [base-type-kw (db-common/translate-type
                      (:col-type-kw col-map) real-synonyms)
        min-max (base-type-kw real-type-kw->min-max)]
    (json-types/make-real (:min min-max) (:max min-max))))

;; dispatcher
;; (defn col-type->json-type [^String col-def-str]
;;   "Transform a mysql type string (i.e. 'int(10) unsigned') into a JSONSchema type"
;;   (let [col-map (db-common/col-def-str->col-map col-def-str col-type->json-type-kw)]
;;     (col-map->json-type col-map)))

;;(print-expr (db-common/translate-type :tinyint {:tinyint :int}))

;;;;;;;;;;;;;; BEGIN JSONSCHEMA->DB TYPE TRANSLATIONS ;;;;;;;;;;;;;;;;;;;;;;;

(defn- dispatch-json-type->col-type [json-type]
  (json-types/getType json-type))

(defmulti map-json-type->col-type
  #'dispatch-json-type->col-type)

;; the biggest numeric value we can store is a 64-bit int,
;; so a normal INT should work here
(defmethod map-json-type->col-type :int [json-type]
  (let [type-max (json-types/getMax json-type)]
  (if  (>= SIGNED_64_INT_MAX type-max)
    "int"
    (slingshot/throw+
     (format "integer range out of bounds (%s) for Vertica Int (max: %s)"
             type-max
             SIGNED_64_INT_MAX))
    )))

;; Seems like most MySQL string types have the same limits
;; Just return a VARCHAR.
(defmethod map-json-type->col-type :str [json-type]
  (let [str-length (min (json-types/getMax json-type) MAX_CHAR_LENGTH)]
    (format "varchar(%d)" str-length)))

(defmethod map-json-type->col-type :bool [json-type]
  "bool")

(defmethod map-json-type->col-type :real [json-type]
  "decimal")

(defmethod map-json-type->col-type :date [json-type]
  (if (= json-types/getType json-type :date)
    "date"
    "timestamp"))

;;;;;;;;;;;;;; END JSONSCHEMA->DB TYPE TRANSLATIONS ;;;;;;;;;;;;;;;;;;;;;;;

(deftype VerticaTypeTranslator []
  dt/DBTypeTranslator
  (col-type->json-type [_ col-def-str]
    "Transform a mysql type string (i.e. 'int(10) unsigned') into a JSONSchema type"
    (let [col-map (db-common/col-def-str->col-map col-def-str
                                                  col-type->json-type-kw)]
      (col-map->json-type col-map)))

  (json-type->col-type [_ json-type]
    "Transform a jsonschema type (i.e. jsonschema.type-system.types.Int{:min 0 :max 2147483647}
into a mysql type (i.e. 'bigint')"
    (map-json-type->col-type json-type)))

(defn make-vertica-type-translator []
  (VerticaTypeTranslator. ))

(def sample-vrt-table
  [
   "created_by" "int(8)"
   "created_date" "timestamp(8)"
   "is_deleted"  "int(8)"
   "modified_by" "int(8)"
   "modified_date" "timestamp(8)"
   "product_id" "int(8)"
   "product_to_sku_id" "int(8)"
   "sku_id" "int(8)"
   ])
