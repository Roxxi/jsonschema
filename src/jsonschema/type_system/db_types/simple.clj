(ns jsonschema.type-system.db-types.simple
  "Type translation from database column types to JSON schema types"
  {:author "Shawn Shah"
   :date "1/22/2014"}
  (:require [clojure.string :as string]
            [clojure.math.numeric-tower :as math]
            [roxxi.utils.print :refer [print-expr]]
            [jsonschema.type-system.types :as types]
            [jsonschema.type-system.db-types.common :as db-common]
            [jsonschema.type-system.db-types.translator :as dt]
            [slingshot.slingshot :as slingshot]))

;; Useful values for type limits
(def SIGNED_64_INT_MAX (- (math/expt 2 63) 1))
(def SIGNED_64_INT_MIN (+ (* -1 (math/expt 2 63)) 1))

(def MAX_IEEE_754_FLOAT (. Double MIN_VALUE))
(def MIN_IEEE_754_FLOAT (. Double MAX_VALUE))

(def MAX_STR_LENGTH 65535)
(def DEFAULT_STR_LENGTH 80)

(def col-type->json-type-kw
  {
   "int" :int
   "varchar" :string
   "str" :string
   "date" :date
   "real" :real
   })

(defn date-col-length->date-fmt-set [col-length]
  (-> (string/replace col-length #"\s+" "")
      (string/split col-length #",")
      set))

(defn- str-col-length->str-length [str-col-length]
  (db-common/coalesce-with-limit str-col-length DEFAULT_STR_LENGTH MAX_STR_LENGTH))

;; TODO map strings to types with sensible lengths for Vertica
;; Since it is the most likely destination
(defn col-map->json-type [col-map]
  (let []
    (condp = (:col-type-kw (print-expr col-map))
      :int (types/make-int SIGNED_64_INT_MIN SIGNED_64_INT_MAX)
      :str (types/make-str 0 (str-col-length->str-length (:col-length col-map)))
      :date (types/make-date (date-col-length->date-fmt-set (:col-length col-map)))
      :real (types/make-real MIN_IEEE_754_FLOAT MAX_IEEE_754_FLOAT)
      :bool (types/make-bool)
            )))

(defn map-json-type->col-type [json-type]
  (condp = (types/getType json-type)
    :int "int"
    :str "string"
    :date "date"
    :real "real"
    :bool "bool"))

(deftype SimpleTypeTranslator []
  dt/DBTypeTranslator
  (col-type->json-type [_ col-def-str]
    "Transform a simple type string (i.e. 'int')
into a JSONSchema type"
    (let [col-map (db-common/col-def-str->col-map
                   col-def-str col-type->json-type-kw)]
      (col-map->json-type col-map)))

  (json-type->col-type [_ json-type]
    "Transform a jsonschema type (i.e. jsonschema.type-system.types.Int{:min 0 :max 2147483647}
into a simple type (i.g. 'int')"
    (map-json-type->col-type json-type)))

(defn make-simple-type-translator []
  (SimpleTypeTranslator. ))
