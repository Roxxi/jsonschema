(ns jsonschema.type-system.extract
  (:use roxxi.utils.collections
        jsonschema.type-system.types
        clojure.set))

;; # General Concepts for deriving types from structures
(defprotocol TypePredicator
    "For some sort of representaiton of a document (e.g. JSON, Avro, BSON, XML, AnalyticaUniverse)
     provides answers for the following predicates"
    (null? [this x] "true iff x is explicitly null")
    (bool? [this x] "true iff x is a boolean")
    (int? [this x] "true iff x is an integer")
    (real? [this x] "true iff x is a decimal")
    (str? [this x] "true iff x is a string")
    (document? [this x] "true iff x is a document")
    (collection? [this x] "true iff x is a collection")
    (special? [this x] "true iff x is a special type for a particular representation (i.e. BSON dates or IDs, Avro Binary, etc. Effectively, they're scalars, in that they cannot be containers of other types)"))

(defprotocol TypeExtractor
  "Extracts the type of a given boject- specific to whatever datastructure its traversing. This may or may not take advantage of the TypePredicator above, but conceptually, if we couldn't implement a predicator, we can't implement this"
  (extract [extractor x] "Returns a type for this object using the make-<type> functions above"))


;; # Special Datatypes
;; The whole notion here is we want to prove out we can do things
;; like handle mongo's date and id representations

(defn special-date? [x]
  (and (clojure.core/string? x)
       (.startsWith (clojure.string/lower-case x) "date(")
       (.endsWith x ")")))

(defn special-id? [x]
  (and (clojure.core/string? x)
       (.startsWith (clojure.string/lower-case x) "id(")
       (.endsWith x ")")))

(defn make-special [x]
  (cond
   (special-date? x) (make-scalar :date),
   (special-id? x) (make-scalar :id)))


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
  
(deftype ClojureTypeExtractor 
    [pred]
  TypeExtractor
  (extract [extractor x]
    (cond
     (special? pred x) (make-special x),
     (null? pred x) (make-scalar :null),
     (bool? pred x) (make-scalar :bool),
     (int? pred x) (make-scalar :int),
     (real? pred x) (make-scalar :real),
     (str? pred x) (make-scalar :string),
     (document? pred x)(make-document (project-map x :value-xform #(extract extractor %))),
     (collection? pred x) (make-collection (map #(extract extractor %) x))
     :else (throw
            (RuntimeException.
             (str "Do not know how to extract a type from " x " of class " (class x)))))))

;; ## Factory methods.

(defn clojure-predicator []
  (ClojureTypePredicator.))


(defn clojure-type-extractor []
  (ClojureTypeExtractor. (clojure-predicator)))

;; # Convenience functions

(defn extract-type [clojure-data-structure]
  (extract (clojure-type-extractor) clojure-data-structure))