(ns jsonschema.type-system.db-types.mysql
  "Type translation from database column types to JSON schema types"
  {:author "Shawn Shah"
   :date "1/22/2014"}
  (:require [clojure.string :as string]
            [clojure.math.numeric-tower :as math]
            [roxxi.utils.print :refer [print-expr]]
            [jsonschema.type-system.types :as json-types]
            [slingshot.slingshot :as slingshot]))

;; MySQL 5.1 Type Conversions

;; Some forward declarations
(declare col-def-str->col-map)
(declare col-def-str->type-str)
(declare col-def-str->length-str)
(declare col-def-str->col-type-length)
(declare col-def-str-is-unsigned?)

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
   "boolean" :bool
   "bool" :bool
   "datetime" :date
   "date" :date
   "timestamp" :date
 })

(defmulti col-map->json-type
  "Multimethod. Takes a map with column attributes
like {:json-type :int :mysql-type-kw :int_unsigned :col-length 10}"
  (fn [col-map]
    (:json-type col-map)))

;; Numeric

;; INTEGER TYPES
;; BIT
;; TINYINT
;; BOOL/BOOLEAN
;; SMALLINT
;; MEDIUMINT
;; INT/INTEGER
;; BIGINT
(def int-type->min-max
  {
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
  (let [min-max (int-type->min-max (:mysql-type-kw col-map))]
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
   :blob 65535
   :text 65535
})

;; CHAR
;; VARCHAR
;; BLOB
;; TEXT
;; ENUM
;; SET

;; TODO: test case around thrown exception
(defn- str-type-length->max [mysql-str-type-kw str-length-str]
  "Pass n-length through if defined, otherwise check for implicit max length"
  (if (nil? str-length-str)
    (if (contains? str-type->max mysql-str-type-kw)
      (str-type->max mysql-str-type-kw)
      (slingshot/throw+ {:type mysql-str-type-kw :length str-length-str}))
    (Integer. str-length-str)))

(defmethod col-map->json-type :str [col-map]
  (let [n-length (str-type-length->max (:mysql-type-kw col-map) (:col-length col-map))]
    (json-types/make-str 0 n-length)))

;; Date
;;
;; DATE
;; TIME
;; DATETIME
;; TIMESTAMP
;; YEAR
(defn- mysql-type-kw->date-format-pattern [^String date-type-str]
  (let [date-fmt-str "yyyy-MM-dd"
        datetime-fmt-str "yyyy-MM-dd HH:mm:ss"]
    (condp = date-type-str
      :date "yyyy-MM-dd"
      :datetime datetime-fmt-str
      :timestamp datetime-fmt-str)))

(defmethod col-map->json-type :date [col-map]
  (let [date-fmt-pattern (mysql-type-kw->date-format-pattern
                          (:mysql-type-kw col-map))]
    (json-types/make-date date-fmt-pattern)))

;; BOOLEAN
;; BOOL
(defmethod col-map->json-type :bool [col-map]
  (json-types/make-bool))

;; Common
(defn- col-def-str-is-unsigned? [^String col-def-str]
  (let [str-parts (string/split (string/lower-case col-def-str) #"[\s]+")]
    (and (= 2 (count str-parts)) (= (second str-parts) "unsigned"))))

(defn- col-def-str->type-str [col-def-str]
  (first (string/split col-def-str #"[^\w]+")))

(defn- col-def-str->length-str [col-def-str]
  (let [matches (re-find #"\(([0-9,]+)\)" col-def-str)]
    (if (> (count matches) 1) (second matches) nil)))

(defn- col-def-str->col-type-length [^String col-def-str]
  (let [col-type-str (col-def-str->type-str col-def-str)
        col-length-str (col-def-str->length-str col-def-str)]
    {:col-type col-type-str :col-length col-length-str}))

(defn- col-type-signed-str->mysql-type-kw [col-type-str col-signed-str]
  (if (nil? col-signed-str)
    (keyword col-type-str)
    (keyword (format "%s_%s" col-type-str col-signed-str))))

(defn- col-def-str->col-map [^String col-def-str]
  "Transform a column definition (i.e. 'int(11)') into map with column attributes"
  (let [col-signed-str (if (col-def-str-is-unsigned? col-def-str) "unsigned" nil)
        col-type-length (col-def-str->col-type-length col-def-str)
        col-type-kw (col-type-signed-str->mysql-type-kw
                     (:col-type col-type-length) col-signed-str)
        col-json-type (col-type->json-type-kw (:col-type col-type-length))]
     {:json-type col-json-type
      :mysql-type-kw col-type-kw
      :col-length (:col-length col-type-length)}))

(defn col-type->json-type [^String col-def-str]
  "Transform a mysql column type string (i.e. 'int(10) unsigned') into a JSONSchema type"
  (let [col-map (col-def-str->col-map col-def-str)]
    (col-map->json-type col-map)))
