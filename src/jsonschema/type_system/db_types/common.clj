(ns jsonschema.type-system.db-types.common
  "Common utilities for JSONSchema DB Types"
  {:author "Shawn Shah"
   :date "2/13/2014"}
  (:require [roxxi.utils.print :refer [print-expr]]
            [clojure.string :as string]
            [jsonschema.type-system.db-types.translator :as dt]
            [jsonschema.type-system.types :as types]))

(defmulti wider?
 (fn [l-type r-type]
    (print-expr [(types/getType l-type) (types/getType r-type)])
  ))

(defmethod wider? [:int :str] [l-type r-type]
  (let [l-length (count (str (types/getMax l-type)))
        r-length (types/getMax r-type)]
    (> l-length r-length)))

(defmethod wider? [:str :int] [l-type r-type]
  (let [l-length (types/getMax l-type)
        r-length (count (str (types/getMax r-type)))]
    (> l-length r-length)))

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
