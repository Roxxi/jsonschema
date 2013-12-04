(ns jsonschema.type-system.extract
  "Takes a Clojure object and gives you back a Type."
  {:author "Alex Bahouth, Matt Halverson"
   :date "12/1/2012"}
  (:require [jsonschema.type-system.types :refer :all]
            [jsonschema.type-system.merge-common :refer [turn-into-a-collection]]
            [jsonschema.type-system.merge :refer [merge-reducer]]
            [jsonschema.type-system.simplify :refer [simplify-reducer]]
            [roxxi.utils.collections :refer :all]
            [roxxi.utils.print :refer [print-expr]])
  (:import [java.text ParsePosition DateFormat]))

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
;; like handle mongo's date and id representations.

;; parseable? doesn't NEED to be in this module; ultimately, it may
;; live somewhere else. For now, though, I (Matt) just wanted to have
;; it checked in somewhere.
(defn parseable? [^DateFormat date-format string]
  "This function is a more-performant alternative to calling
      (try (.parse date-format string) (catch ParseException e ... ))
This does NOT throw/catch exceptions internally.

The implementation of this is based on the java 6 and java 7 source code --
the unary parse method calls the binary parse method, then inspects the state of
the ParsePosition. If its index is zero, then (and only then) is a
ParseException thrown. Here, if the index is zero, `false` is returned."
  (let [pos (ParsePosition. 0)
        result (.parse date-format (str string) pos)
        idx (.getIndex pos)]
    (not (zero? idx))))
;; Sample usage:
;;   (def sdf (java.text.SimpleDateFormat. "yyyy-MM-dd"))
;;   (parseable? sdf "2013-10-10")
;;   (parseable? sdf "2013--10")
;;   (parseable? "")
;;   (parseable? "2-10-10")

(defn special-date? [x]
  (and (clojure.core/string? x)
       (.startsWith (clojure.string/lower-case x) "date(")
       (.endsWith x ")")))

;; You may wonder, what is special-id? doing here if it always returns false?
;; The answer is, it's a stub during development to make sure that special data
;; types actually have a place where they belong, somewhere in the overall
;; type system.
(defn special-id? [x]
  false)

(defn make-special [x]
  (cond
   (special-date? x) (make-date nil)
   (special-id? x) nil))


;; # Implementation for examples

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
     (collection? pred x) (turn-into-a-collection
                           type-reducer
                           (map #(extract extractor %) x))
     :else (throw
            (RuntimeException.
             (str "Do not know how to extract a type from " x
                  " of class " (class x)))))))

;; # Factory functions

(defn clojure-predicator []
  (ClojureTypePredicator.))

(defn clojure-type-extractor [type-reducer]
  (ClojureTypeExtractor. (clojure-predicator) type-reducer))

;; # Convenience functions

(defn merging-clojure-type-extractor []
  (clojure-type-extractor merge-reducer))

(defn simplifying-clojure-type-extractor []
  (clojure-type-extractor simplify-reducer))

(defn extract-type-merging [clojure-data-structure]
  (extract (merging-clojure-type-extractor) clojure-data-structure))

(defn extract-type-simplifying [clojure-data-structure]
  (extract (simplifying-clojure-type-extractor) clojure-data-structure))
