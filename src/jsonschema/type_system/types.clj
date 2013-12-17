(ns jsonschema.type-system.types
  "These are the structures we use to represent type."
  {:author "Alex Bahouth, Matt Halverson"
   :date "12/1/2012"}
  (:require [roxxi.utils.print :refer [print-expr]]
            [roxxi.utils.common :refer [def-]]
            [clojure.set :refer [union]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # SIMPLE TYPES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ## Protocols

(defprotocol Typeable
  (getType [this] "Returns the type of this Typeable thing"))

(defprotocol Ranged
  (getMin [this] "Returns the min value of this Ranged thing")
  (getMax [this] "Returns the max value of this Ranged thing"))

(defprotocol Formatted
  (getFormats [this] "Returns a set of format strings that describe
                      this element's content"))

;; ## Records

(defrecord Int [min max]
  Typeable
  (getType [this] :int)
  Ranged
  (getMin [this] min)
  (getMax [this] max))

(defrecord Real [min max]
  Typeable
  (getType [this] :real)
  Ranged
  (getMin [this] min)
  (getMax [this] max))

(defrecord Str [min max]
  Typeable
  (getType [this] :str)
  Ranged
  (getMin [this] min)
  (getMax [this] max))

(defrecord Bool []
  Typeable
  (getType [this] :bool))

(defrecord Null []
  Typeable
  (getType [this] :null))

(defrecord Date [formats]
  Typeable
  (getType [this] :date)
  Formatted
  (getFormats [this] formats))

;; ## Predicates

(defmacro generate-scalar-predicate [type]
  `(defn ~(symbol (str (name type) "-type?")) [some-val#]
     (and
      (satisfies? Typeable some-val#)
      (= (getType some-val#) ~type))))

;; e.g. null-type? bool-type? etc
(generate-scalar-predicate :int)
(generate-scalar-predicate :real)
(generate-scalar-predicate :str)
(generate-scalar-predicate :bool)
(generate-scalar-predicate :null)
(generate-scalar-predicate :date)

(def- scalar-type-registry
  #{:null :bool :int :real :str :date})

;; `scalar` is a virtual type.
(defn scalar-type?
  ([x] (scalar-type? scalar-type-registry x))
  ([registry x]
     (and (satisfies? Typeable x)
          (contains? registry (getType x)))))

(defn same-type? [s1 s2]
  (and (satisfies? Typeable s1)
       (satisfies? Typeable s2)
       (= (getType s1) (getType s2))))

;; ## Factory functions

(defn make-int
  ([val] (Int. val val))
  ([min max] (Int. min max)))

(defn make-real
  ([val] (Real. val val))
  ([min max] (Real. min max)))

(defn make-str
  ([val] (let [len (count val)]
           (Str. len len)))
  ([min max] (Str. min max)))

(defn make-bool []
  (Bool. ))

(defn make-null []
  (Null. ))

;; NB `set` is not called to dedup but rather to make equality of two Dates
;; be independent of the ORDER of the date-formats.
(defn make-date [date-format-patterns]
  (Date. (set date-format-patterns)))

(defn make-date-with [& date-format-patterns]
  (make-date date-format-patterns))

;; ## Merging

(defn- merge-ranged-scalars [s1 s2 type]
  (let [min-val (min (getMin s1) (getMin s2))
        max-val (max (getMax s1) (getMax s2))
        known-types #{:int :real :str}]
    (condp = type
      :int (make-int min-val max-val)
      :real (make-real min-val max-val)
      :str (make-str min-val max-val)
      (throw
       (RuntimeException.
        (str "Do not know how to merge-ranged-scalars of type " type
             "; only know " known-types))))))

(defn- merge-formatted-scalars [s1 s2 type]
  (let [date-format-patterns (union (getFormats s1) (getFormats s2))
        known-types #{:date}]
    (condp = type
      :date (make-date date-format-patterns)
      (throw
       (RuntimeException.
        (str ("Do not know how to merge-formatted-scalars of type " type
              "; only know " known-types)))))))

(defn merge-same-typed-scalars [s1 s2]
  (let [type (getType s1)
        known-types #{:int :real :str :bool :null :date}]
    (condp = type
      :int (merge-ranged-scalars s1 s2 type)
      :real (merge-ranged-scalars s1 s2 type)
      :str (merge-ranged-scalars s1 s2 type)
      :bool s1
      :null s1
      :date (merge-formatted-scalars s1 s2 type)
      (throw
       (RuntimeException.
        (str ("Do not know how to merge-same-typed-scalars of type " type
              "; only know " known-types)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # COMPLEX TYPES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ## Primitives

(defrecord Document [properties map]
  Typeable
  (getType [this] :document))

(defrecord Union [union-of]
  Typeable
  (getType [this] :union))

(defrecord Collection [coll-of]
  Typeable
  (getType [this] :collection))

;; ## Predicates

(defn document-type? [x]
  (and
   (satisfies? Typeable x)
   (= (getType x) :document)))

(defn union-type? [x]
  (and (satisfies? Typeable x)
       (= (getType x) :union)))

(defn collection-type? [x]
  (and (satisfies? Typeable x)
       (= (getType x) :collection)))



(defn empty-document? [document]
  (and (document-type? document)
       (empty? (:map document))
       (empty? (:properties document))))

(defn document-types [union]
  (filter document-type? (:union-of union)))

(defn non-document-types [union]
  (remove document-type? (:union-of union)))

(defn empty-collection? [collection]
  "This is dependent on the logic of turn-into-a-collection, which knows how
to create collections of :nothing."
  (and (collection-type? collection)
       (= (:coll-of collection) :nothing)))

;; ## Factory functions

;; Note that these are pure factory functions. They do not perform any
;; sanitizing logic. e.g. if you call
;;       (make-union-with (make-int 1 3) (make-int 3 5))
;;    you will NOT get back
;;        #jsonschema.type_system.types.Int{:min 1, :max 5}
;; Instead you will get back exactly what you asked for:
;;        #jsonschema.type_system.types.Union{:union-of
;;            #{#jsonschema.type_system.types.Int{:min 1, :max 3}
;;              #jsonschema.type_system.types.Int{:min 3, :max 5}}
;; So, be aware of that when you call these functions.
;;
;; If you want fancy logic-containing factory functions, go to
;; turn-into-a-union(-with) and turn-into-a-collection(-with)
;; in jsonschema.type-system.merge-common.

(defn make-document [property-type-map]
  (Document. (set (keys property-type-map))
             property-type-map))

;; NB the call to 'set' is not to dedup but to make the equality of
;;    unions be independent of the ORDER of their contents.
(defn make-union [non-mergeable-types]
  (Union. (set non-mergeable-types)))

(defn make-union-with [& non-mergeable-types]
  (make-union non-mergeable-types))

;; NB 'type' may be a simple or complex type (including another collection),
;;    of course.
(defn make-collection [type]
  (Collection. type))
