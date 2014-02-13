(ns jsonschema.type-system.db-types.common
  "Common utilities for JSONSchema DB Types"
  {:author "Shawn Shah"
   :date "2/13/2014"}
  (:require [clojure.string :as string]))

;; Common functions
;; TODO: Refactor. Lots of duplication here with mysql.clj common functions
(defn str-or-nil->int-or-nil [str-or-nil]
   (if (nil? str-or-nil) nil
       (read-string str-or-nil)))

(defn coalesce-with-limit [n-val n-default-val n-max-val]
  "If n-val is nil, return the n-default-val. If val is NOT nil,
return the minimum of n-val and n-max-val"
  (if (nil? n-val) n-default-val (min n-val n-max-val)))

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
