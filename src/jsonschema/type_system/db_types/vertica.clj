(ns jsonschema.type-system.db-types.vertica
  "Type translation from database column types to JSON schema types"
  {:author "Shawn Shah"
   :date "2/3/2014"}
  (:require [clojure.string :as string]
            [clojure.math.numeric-tower :as math]
            [roxxi.utils.print :refer [print-expr]]
            [jsonschema.type-system.types :as json-types]
            [slingshot.slingshot :as slingshot]))

;; Vertica 6 Type Conversions
;; Some forward declarations
(declare col-def-str->col-map)
(declare col-def-str->type-str)
(declare col-def-str->length-str)
(declare col-def-str->col-type-length)
(declare col-def-str-is-unsigned?)
(declare min-or-default-num)
(declare str-or-nil->int-or-nil)

;; type mapping
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
   "interval" :real
   "smalldatetime" :date
   "double" :real
   "float" :real
   "float8" :real
   "real" :real
   "number" :real
   "money" :real
   "decimal" :real
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

(defn- translate-bin-type [vrt-type-kw]
  "If binary type is a synonym, translate to canonical type.
Otherwise, pass binary type through."
  (if (contains? bin-synonym->bin-type-kw vrt-type-kw)
    (get bin-synonym->bin-type-kw vrt-type-kw)
    vrt-type-kw))

(defn- bin-type-length->max-length [vrt-type-kw col-length]
  "Determine correct column length given binary type and provided length"
  (cond
   (= vrt-type-kw :binary) (min-or-default-num
                            col-length DEFAULT_BINARY_LENGTH MAX_BINARY_LENGTH)
   (= vrt-type-kw :varbinary) (min-or-default-num
                               col-length DEFAULT_VARBINARY_LENGTH MAX_BINARY_LENGTH)
   :else nil))

(defmethod col-map->json-type :binary [col-map]
  (let [bin-type-kw (translate-bin-type (:vrt-type-kw col-map))
        n-col-length (str-or-nil->int-or-nil (:col-length col-map))
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
(def str-synonym->str-type-kw
  {
   :character :char
   })

(defn- translate-str-type [vrt-type-kw]
  "If binary type is a synonym, translate to canonical type.
Otherwise, pass binary type through."
  (if (contains? str-synonym->str-type-kw vrt-type-kw)
    (get str-synonym->str-type-kw vrt-type-kw)
    vrt-type-kw))

(def DEFAULT_CHAR_LENGTH 1)
(def DEFAULT_VARCHAR_LENGTH 80)
(def MAX_CHAR_LENGTH 65000)
(def MAX_VARCHAR_LENGTH 65000)

(defn- str-type-kw->default-and-max-length [str-type-kw]
  (cond
   (= str-type-kw :char) [DEFAULT_CHAR_LENGTH MAX_CHAR_LENGTH]
   (= str-type-kw :varchar) [DEFAULT_VARCHAR_LENGTH MAX_VARCHAR_LENGTH]
   :else nil))

(defmethod col-map->json-type :str [col-map]
  (let [n-col-length (str-or-nil->int-or-nil (:col-length col-map))
        str-type-kw (translate-str-type (:vrt-type-kw col-map))
        [default-length max-length]
        (str-type-kw->default-and-max-length str-type-kw)
        json-str-length (min-or-default-num
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

(def date-synonym->date-type-kw
  {
   :datetime :timestamp
   :smalldatetime :timestamp
   })

(def date-type-kw->date-format-patterns
  {
   :date ["yyyy-MM-dd"]
   :timestamp ["yyyy-MM-dd HH:mm:ss" "yyyy-MM-dd HH:mm:ssZ"]
   :time ["HH:mm:ss" "HH:mm:ssZ"]
   })

;; TODO: move all these translation behavior into
;; a more type-agnostic function
(defn- translate-date-type [vrt-type-kw]
  "If date type is a synonym, translate to canonical type.
Otherwise, pass date type through."
  (if (contains? date-synonym->date-type-kw vrt-type-kw)
    (get date-synonym->date-type-kw vrt-type-kw)
    vrt-type-kw))

(defmethod col-map->json-type :date [col-map]
  (let [date-type-kw (translate-date-type  (:vrt-type-kw col-map))
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
(def real-synonym->real-type-kw
  {
   :float :double
   :float8 :double
   :real :double
   :decimal :numeric
   :number :numeric
   :money :numeric
   :interval :numeric
   })

;; TODO: refactor duplicated logic with other translate-*-type functions
(defn- translate-real-type [vrt-type-kw]
  "If real type is a synonym, translate to canonical type.
Otherwise, pass real type through."
  (if (contains? real-synonym->real-type-kw vrt-type-kw)
    (get real-synonym->real-type-kw vrt-type-kw)
    vrt-type-kw))

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
  (let [real-type-kw (translate-real-type (:vrt-type-kw col-map))
        min-max (real-type-kw real-type-kw->min-max)]
    (json-types/make-real (:min min-max) (:max min-max))))

;; Common functions
;; TODO: Refactor. Lots of duplication here with mysql.clj common functions
(defn- str-or-nil->int-or-nil [str-or-nil]
   (if (nil? str-or-nil) nil
       (Integer. str-or-nil)))

(defn- min-or-default-num [n-val n-default-val n-max-val]
  "If n-val is nil, return the n-default-val. If val is NOT nil,
return the minimum of n-val and n-max-val"
  (if (nil? n-val) n-default-val (min n-val n-max-val)))

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

(defn- col-type-signed-str->type-kw [col-type-str col-signed-str]
  (if (nil? col-signed-str)
    (keyword col-type-str)
    (keyword (format "%s_%s" col-type-str col-signed-str))))

(defn- col-def-str->col-map [^String col-def-str]
  "Transform a column definition (i.e. 'int(11)') into map with column attributes"
  (let [col-type-length (col-def-str->col-type-length col-def-str)
        col-json-type (col-type->json-type-kw (:col-type col-type-length))
        vrt-type-kw (keyword (col-def-str->type-str col-def-str))]
     {:json-type col-json-type
      :vrt-type-kw vrt-type-kw
      :col-length (:col-length col-type-length)}))

(defn col-type->json-type [^String col-def-str]
  "Transform a mysql type string (i.e. 'int(10) unsigned') into a JSONSchema type"
  (let [col-map (col-def-str->col-map col-def-str)]
    (col-map->json-type col-map)))
