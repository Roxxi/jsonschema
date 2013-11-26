(ns jsonschema.type-system.extract
  (:require [jsonschema.type-system.types :refer :all]
            [jsonschema.type-system.merge-common :refer [turn-into-a-collection]]
            [jsonschema.type-system.merge :refer [merge-reducer]]
            [jsonschema.type-system.simplify :refer [simplify-reducer]]
            [roxxi.utils.collections :refer :all]
            [roxxi.utils.print :refer [print-expr]]))

;; # General Concepts for deriving types from structures
(defprotocol TypePredicator
    "For some sort of representation of a document (e.g. JSON, Avro, BSON, XML,
AnalyticaUniverse) provides answers for the following predicates"
    (null? [this x] "true iff x is explicitly null")
    (bool? [this x] "true iff x is a boolean")
    (int? [this x] "true iff x is an integer")
    (real? [this x] "true iff x is a decimal")
    (str? [this x] "true iff x is a string")
    (document? [this x] "true iff x is a document")
    (collection? [this x] "true iff x is a collection")
    (special? [this x] "true iff x is a special type for a particular
representation (i.e. BSON dates or IDs, Avro Binary, etc. Effectively, they're
scalars, in that they cannot be containers of other types)"))

(defprotocol TypeExtractor
  "Extracts the type of a given object- specific to whatever datastructure it's
traversing. This may or may not take advantage of the TypePredicator above, but
conceptually, if we couldn't implement a predicator, we can't implement this."
  (extract [extractor x]
    "Returns a type for this object using the make-<type> functions above"))


;; # Special Datatypes
;; The whole notion here is we want to prove out we can do things
;; like handle mongo's date and id representations

(defn special-date? [x]
  (and (clojure.core/string? x)
       (.startsWith (clojure.string/lower-case x) "date(")
       (.endsWith x ")")))

(defn special-id? [x]
  false)

(defn make-special [x]
  (cond
   (special-date? x) (make-date nil)
   (special-id? x) nil))


;; # Implementation for Examples.

(deftype ClojureTypePredicator []
  TypePredicator
  (special? [this x] (or (special-date? x) (special-id? x)))
  (null? [this x] (nil? x))
  (bool? [this x] (or (true? x) (false? x)))
  (int? [this x] (and (number? x) (integer? x)))
  (real? [this x] (and (number? x) (not (integer? x))))
  (str? [this x] (and (string? x) (not (special? this x))))
  (document? [this x] (map? x))
  (collection? [this x] (or (vector? x) (list? x))))

(deftype ClojureTypeExtractor [pred type-reducer]
  TypeExtractor
  (extract [extractor x]
    (cond
     (special? pred x) (make-special x),
     (null? pred x) (make-null),
     (bool? pred x) (make-bool),
     (int? pred x) (make-int x),
     (real? pred x) (make-real x),
     (str? pred x) (make-str x),
     (document? pred x) (make-document
                         (project-map x :value-xform #(extract extractor %))),
     (collection? pred x) (turn-into-a-collection type-reducer
                                                  (map #(extract extractor %) x))
     :else (throw
            (RuntimeException.
             (str "Do not know how to extract a type from " x " of class " (class x)))))))

;; ## Factory methods.

(defn clojure-predicator []
  (ClojureTypePredicator.))

(defn merging-clojure-type-extractor []
  (ClojureTypeExtractor. (clojure-predicator) merge-reducer))

(defn simplifying-clojure-type-extractor []
  (ClojureTypeExtractor. (clojure-predicator) simplify-reducer))

;; # Convenience functions

(defn extract-type-merging [clojure-data-structure]
  (extract (merging-clojure-type-extractor) clojure-data-structure))

(defn extract-type-simplifying [clojure-data-structure]
  (extract (simplifying-clojure-type-extractor) clojure-data-structure))
