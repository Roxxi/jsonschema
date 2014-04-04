(ns jsonschema.type-system.db-types.mysql
  "Type translation from database column types to JSON schema types"
  {:author "Shawn Shah"
   :date "1/22/2014"}
  (:require [clojure.string :as string]
            [clojure.math.numeric-tower :as math]
            [roxxi.utils.print :refer [print-expr]]
            [jsonschema.type-system.types :as json-types]
            [jsonschema.type-system.db-types.common :as db-common]
            [jsonschema.type-system.db-types.translator :as dt]
            [slingshot.slingshot :as slingshot]))

;; MySQL 5.1 Type Conversions
;;;;;;;;;;;;;;;;;;;;;;; BEGIN DB->JSONSCHEMA TYPE TRANSLATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; type mapping
(def col-type->json-type-kw
  {
   "tinyint" :int
   "smallint" :int
   "mediumint" :int
   "int" :int
   "integer" :int
   "bigint" :int
   "bit" :int
   "decimal" :real
   "numeric" :real
   "float" :real
   "double" :real
   "char" :str
   "varchar" :str
   "blob" :str
   "text" :str
   "enum" :str
   "set" :str
   "boolean" :bool
   "bool" :bool
   "datetime" :date
   "date" :date
   "timestamp" :date
 })

(defmulti col-map->json-type
  "Multimethod. Takes a map with column attributes
like {:json-type :int :col-type-kw :int_unsigned :col-length 10}"
  (fn [col-map]
    (:json-type col-map)))

;; Numeric

;; INTEGER TYPES
;; BIT
;; TINYINT
;; SMALLINT
;; MEDIUMINT
;; INT/INTEGER
;; BIGINT
(def int-type->min-max
  {
   :bit {:min 1 :max 64}
   :tinyint {:min -128 :max 127}
   :tinyint_unsigned {:min 0 :max 255}
   :smallint {:min -32768 :max 32767}
   :smallint_unsigned {:min 0 :max 65535}
   :mediumint {:min -8388608 :max 8388607}
   :mediumint_unsigned {:min 0 :max 16777215}
   :int {:min -2147483648 :max 2147483647}
   :int_unsigned {:min 0 :max 4294967295}
   :bigint {:min -9223372036854775808 :max 9223372036854775807}
   :bigint_unsigned {:min 0 :max 18446744073709551615}
   })

(defmethod col-map->json-type :int [col-map]
  (let [min-max (int-type->min-max (:col-type-kw col-map))]
    (json-types/make-int (:min min-max) (:max min-max))))

;; DECIMAL
;; NUMERIC
;; FLOAT
;; DOUBLE

;; TODO: find more precise min/max values, but these are approximate bounds that should suffice
(def DECIMAL_MAX (math/expt 10 35))
(def DECIMAL_MIN (* -1 DECIMAL_MAX))

(defmethod col-map->json-type :real [col-map]
  (json-types/make-real DECIMAL_MIN DECIMAL_MAX))

;; String
;; Only some MySQL string types have implicit max length
(def str-type->max
  {
   :enum 65535
   :set 65535
   :blob 65535
   :text 65535
})

;; CHAR
;; VARCHAR
;; BLOB
;; TEXT

;; ENUM
;; Difficult to determine the actual max string length. The documentation only
;; mentions the limit to the number of max values. Feels like the limit of the
;; enum string length is actually the MySQL row limit of 65535

;; SET
;; Same for ENUM. Probably limited by MySQL row size limit of 65535

;; TODO: test case around thrown exception
(defn- str-type-length->max [mysql-str-type-kw str-length-str]
  "Pass n-length through if defined, otherwise check for implicit max length"
  (if (nil? str-length-str)
    (if (contains? str-type->max mysql-str-type-kw)
      (str-type->max mysql-str-type-kw)
      (slingshot/throw+ {:type mysql-str-type-kw :length str-length-str}))
    (Integer. str-length-str)))

(defmethod col-map->json-type :str [col-map]
  (let [n-length (str-type-length->max (:col-type-kw col-map) (:col-length col-map))]
    (json-types/make-str 0 n-length)))

;; Date
;;
;; DATE
;; TIME
;; DATETIME
;; TIMESTAMP
;; YEAR
(defn- col-type-kw->date-format-pattern [^String date-type-str]
  (let [date-fmt-str "yyyy-MM-dd"
        datetime-fmt-str "yyyy-MM-dd HH:mm:ss"]
    (condp = date-type-str
      :date "yyyy-MM-dd"
      :datetime datetime-fmt-str
      :timestamp datetime-fmt-str)))

(defmethod col-map->json-type :date [col-map]
  (let [date-fmt-pattern (col-type-kw->date-format-pattern
                          (:col-type-kw col-map))]
    (json-types/make-date date-fmt-pattern)))

;; BOOLEAN
;; BOOL
(defmethod col-map->json-type :bool [col-map]
  (json-types/make-bool))

;;;;;;;;;;;;;; END DB->JSONSCHEMA TYPE TRANSLATIONS ;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;; BEGIN JSONSCHEMA->DB TYPE TRANSLATIONS ;;;;;;;;;;;;;;;;;;;;;;;

(defn- dispatch-json-type->col-type [json-type]
  (json-types/getType json-type))

(defmulti map-json-type->col-type
  #'dispatch-json-type->col-type)

(defmethod map-json-type->col-type :int [json-type]
  (let [max-val (json-types/getMax json-type)
        int-max (-> int-type->min-max :int :max)]
    (cond
     (> max-val int-max) "bigint"
     :else "int")))

;; Seems like most MySQL string types have the same limits
;; Just return a VARCHAR.
(defmethod map-json-type->col-type :str [json-type]
  (let [str-length (json-types/getMax json-type)]
    (format "varchar(%d)" str-length)))

(defmethod map-json-type->col-type :bool [json-type]
  "bool")

(defmethod map-json-type->col-type :real [json-type]
  "decimal")

(defmethod map-json-type->col-type :date [json-type]
  "timestamp")

;;;;;;;;;;;;;; END JSONSCHEMA->DB TYPE TRANSLATIONS ;;;;;;;;;;;;;;;;;;;;;;;

(defn- col-def-str-and-type-kw->signed-type-kw [^String col-def-str col-type-kw]
  "Take a column type keyword and the column definition string. If column definition is of an unsigned type,
 return the unsigned version of the column type keyword.

For example:
:int and 'int unsigned' -> :int_unsigned
:tinyint and 'tinyint' -> :tinyint"
  (if (db-common/col-def-str-is-unsigned? col-def-str)
    (keyword (format "%s_unsigned" (name col-type-kw)))
    col-type-kw))

;;;;;;;;;;;; Translator Implementation ;;;;;;;;;;;;;

(deftype MysqlTypeTranslator []
  dt/DBTypeTranslator
  (col-type->json-type [_ col-def-str]
    "Transform a mysql column type string (i.e. 'int(10) unsigned')
into a JSONSchema type"
    (let [col-map (db-common/col-def-str->col-map
                   col-def-str col-type->json-type-kw)
          signed-type-kw (col-def-str-and-type-kw->signed-type-kw
                          col-def-str
                          (:col-type-kw col-map))
          col-map-with-signed-type-kw (assoc col-map :col-type-kw signed-type-kw)]
      (col-map->json-type col-map-with-signed-type-kw)))

  (json-type->col-type [_ json-type]
    "Transform a jsonschema type (i.e. jsonschema.type-system.types.Int{:min 0 :max 2147483647}
into a mysql type (i.e. 'bigint')"
    (map-json-type->col-type json-type)))

(defn make-mysql-type-translator []
  (MysqlTypeTranslator. ))
