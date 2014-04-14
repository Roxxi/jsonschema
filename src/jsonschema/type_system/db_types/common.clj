(ns jsonschema.type-system.db-types.common
  "Common utilities for JSONSchema DB Types"
  {:author "Shawn Shah"
   :date "2/13/2014"}
  (:require [roxxi.utils.print :refer [print-expr]]
            [clojure.string :as string]
            [jsonschema.type-system.db-types.translator :as dt]
            [jsonschema.type-system.types :as types]))

(defn- numeric-wider-than-str? [num-type str-type]
  (let [num-length (types/getMax num-type)
        str-length (count (str (types/getMax str-type)))]
    (> num-length str-length)))

(defn- str-wider-than-numeric? [str-type num-type]
  (let [str-length (types/getMax str-type)
        num-length (count (str (types/getMax num-type)))]
    (> str-length num-length)))

(defmulti wider?
  (fn [l-type r-type]
    [(types/getType l-type) (types/getType r-type)]))

(defn- date->max-date-fmt-length [date-type]
  (apply max (map count (types/getFormats date-type))))

;; bool -> other types

(defmethod wider? [:bool :bool] [l-type r-type]
  false)

(defmethod wider? [:bool :int] [l-type r-type]
  false)

(defmethod wider? [:bool :real] [l-type r-type]
  false)

(defmethod wider? [:bool :date] [l-type r-type]
  false)

(defmethod wider? [:bool :str] [l-type r-type]
  false)

(defmethod wider? [:int :bool] [l-type r-type]
  true)

;; int -> other types

(defmethod wider? [:int :bool] [l-type r-type]
  true)

;; int -> int handled by default
;; int -> real handled by default
;; int -> date
(defmethod wider? [:int :date] [l-type r-type]
  false)

(defmethod wider? [:int :str] [l-type r-type]
  (numeric-wider-than-str? l-type r-type))

;; real -> other types

(defmethod wider? [:real :bool] [l-type r-type]
  true)

;; real -> int handled by default

(defmethod wider? [:real :str] [l-type r-type]
  (numeric-wider-than-str? l-type r-type))

(defmethod wider? [:real :date] [l-type r-type]
  true)

;; str -> other types

(defmethod wider? [:str :bool] [l-type r-type]
  (let [str-length (types/getMax l-type)]
    (> str-length 1)))

(defmethod wider? [:str :int] [l-type r-type]
  (str-wider-than-numeric? l-type r-type))

(defmethod wider? [:str :real] [l-type r-type]
  (str-wider-than-numeric? l-type r-type))

(defmethod wider? [:str :date] [l-type r-type]
  (let [str-length (types/getMax l-type)
        max-date-fmt-length (date->max-date-fmt-length r-type)]
    (> str-length max-date-fmt-length)))

;; date -> other types

(defmethod wider? [:date :bool] [l-type r-type]
  true)

(defmethod wider? [:date :int] [l-type r-type]
  true)

(defmethod wider? [:date :real] [l-type r-type]
  true)

(defmethod wider? [:date :str] [l-type r-type]
  (let [max-date-fmt-length (date->max-date-fmt-length r-type)
        str-length (types/getMax l-type)]
    (> max-date-fmt-length str-length)))

(defmethod wider? [:date :date] [l-type r-type]
  (let [l-max-date-fmt-length (date->max-date-fmt-length r-type)
        r-max-date-fmt-length (date->max-date-fmt-length r-type)]
    (> l-max-date-fmt-length r-max-date-fmt-length)))

;; default

(defmethod wider? :default [l-type r-type]
  (> (types/getMax l-type) (types/getMax r-type)))

(def eq-width-or-narrower? (complement wider?))

(defn str-or-nil->int-or-nil [str-or-nil]
   (if (nil? str-or-nil) nil
       (read-string str-or-nil)))

(defn coalesce-with-limit [n-val n-default-val n-max-val]
  "If n-val is nil, return the n-default-val. If val is NOT nil,
return the minimum of n-val and n-max-val"
  (if (nil? n-val) n-default-val (min n-val n-max-val)))

(defn translate-type [col-type-kw col-type-syn-map]
  "Translate a type keyword to the lower level type, if it is a synoynm. If not
a synonym, pass type keyword through.
For example:
    (type-kw-and-synonyms->base-type-kw :tinyint {:tinyint :int}) -> :int
    (type-kw-and-synonyms->base-type-kw :float {:tinyint :int}) -> :float"
  (if (contains? col-type-syn-map col-type-kw)
    (get col-type-syn-map col-type-kw)
    col-type-kw))

;; TODO: write a more generic unsigned checker that can find
;; it anywhere in the type definition
(defn col-def-str-is-unsigned? [^String col-def-str]
  (let [str-parts (string/split (string/lower-case col-def-str) #"[\s]+")]
    (and (= 2 (count str-parts)) (= (second str-parts) "unsigned"))))

(defn col-def-str->type-str [col-def-str]
  (first (string/split col-def-str #"[^\w]+")))

(defn col-def-str->length-str [col-def-str]
  (let [matches (re-find #"\(([^\)]+)\)" col-def-str)]
    (if (> (count matches) 1) (second matches) nil)))

(defn col-def-str->col-type-length [^String col-def-str]
  (let [col-type-str (col-def-str->type-str col-def-str)
        col-length-str (col-def-str->length-str col-def-str)]
    {:col-type col-type-str :col-length col-length-str}))

(defn col-def-str->col-map [^String col-def-str col-type->json-type-kw]
  "Transform a column definition (i.e. 'int(11)') into map with column attributes"
  (let [col-type-length (col-def-str->col-type-length col-def-str)
        col-json-type (col-type->json-type-kw (:col-type col-type-length))
        col-type-kw (keyword (col-def-str->type-str col-def-str))]
     {:json-type col-json-type
      :col-type-kw col-type-kw
      :col-length (:col-length col-type-length)}))
