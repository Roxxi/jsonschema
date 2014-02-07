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

;; type mapping
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

(defn- min-or-default-num [n-val n-default-val n-max-val]
  "If n-val is nil, return the n-default-val. If val is NOT nil,
return the minimum of n-val and n-max-val"
  (if (nil? n-val) n-default-val (min n-val n-max-val)))

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
        n-col-length (if (nil? (:col-length col-map)) nil
                         (Integer. (:col-length col-map)))
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
;; Only some MySQL string types have implicit max length

;;;;;;;;;;;;;;;; DATE TYPES ;;;;;;;;;;;;;;;;;;;;;;;;

;; DATE/TIME
;; DATE
;; DATETIME
;; SMALLDATETIME
;; TIME
;; TIME
;; TIMESTAMP
;; TIMESTAMP
;; INTERVAL
;; APPROXIMATE

;;;;;;;;;;;;;;;; REAL TYPES ;;;;;;;;;;;;;;;;;;;;;;;;

;; DOUBLE
;; FLOAT
;; FLOAT(n)
;; FLOAT8
;; REAL
;; EXACT
;; DECIMAL
;; NUMERIC
;; NUMBER
;; MONEY
(def DECIMAL_MAX (math/expt 10 35))
(def DECIMAL_MIN (* -1 DECIMAL_MAX))

(defmethod col-map->json-type :real [col-map]
  (json-types/make-real DECIMAL_MIN DECIMAL_MAX))

;; Common functions
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
  "Transform a mysql column type string (i.e. 'int(10) unsigned') into a JSONSchema type"
  (let [col-map (col-def-str->col-map col-def-str)]
    (col-map->json-type col-map)))
