(ns jsonschema.type-system.types
  (:use roxxi.utils.collections
        roxxi.utils.common))


;; # Type Primitives
;; These are the "structures" which we use to represent type.
;; Ultimately, these will be mapped into Avro structures instead of Clojure

;; ## Protocols

(defprotocol Typeable
  (getType [this] "Returns the type of this Typeable thing"))

(defprotocol Ranged
  (getMin [this] "Returns the min value of this Ranged thing")
  (getMax [this] "Returns the max value of this Ranged thing"))

(defprotocol Formatted
  (getFormat [this] "Returns the format string for this element's content"))

;; ## Records

(defrecord Union [union-of]
  Typeable
  (getType [this] :union))

(defrecord Document [properties map]
  Typeable
  (getType [this] :document))

(defrecord Collection [coll-of]
  Typeable
  (getType [this] :collection))

(defrecord Int [val min max]
  Typeable
  (getType [this] :int)
  Ranged
  (getMin [this] min)
  (getMax [this] max))

(defrecord Real [val min max]
  Typeable
  (getType [this] :real)
  Ranged
  (getMin [this] min)
  (getMax [this] max))

(defrecord Str [val min max]
  Typeable
  (getType [this] :str)
  Ranged
  (getMin [this] min)
  (getMax [this] max))

(defrecord Null []
  Typeable
  (getType [this] :null))

(defrecord Bool [val]
  Typeable
  (getType [this] :bool))

(defrecord Date [val format]
  Typeable
  (getType [this] :date)
  Formatted
  (getFormat [this] format))

;; ## Factory methods

(defn make-int [val]
  (Int. val val val))

(defn make-real [val]
  (Int. val val val))

(defn make-string [val]
  (let [len (count val)]
    (Int. val len len)))

(defn make-null []
  (Null. ))

(defn make-bool [val]
  (Bool. val))

(defn make-date [val format]
  (Date. val format))



(defn make-union-with
  [& types]
  (Union. (apply set-over types)))

(defn make-union [types]
  (Union. types))

(defn maybe-make-union [types]
  (cond
    (empty? types) nil
    (= (count types) 1) (first types)
    (= (count (hash-set types)) 1) (first types)
    :else (make-union types)))

(defn maybe-make-union-with [& types]
  (maybe-make-union types))

(defn make-document [property-type-map]
  (Document. (vec (keys property-type-map))
             property-type-map))

(defn- one? [a-seq]
  (= (count a-seq) 1))

(defn make-collection
  "If a collection contains no types, there
   will be no types. As such, this will be a collection
   :of :nothing"
  [types]
  (Collection. (if (empty? types)
                 :nothing
                 (let [unique-types (apply set-over types)]
                   (if (one? unique-types)
                     (first unique-types)
                     (make-union unique-types))))))

(defn make-collection-with [& types]
  (make-collection types))

;; # Predicates

(defmacro generate-scalar-predicate [type]
  `(defn ~(symbol (str (name type) "-type?")) [some-val#]
     (and
      (satisfies? Typeable some-val#)
      (= (getType some-val#) ~type))))

;; e.g. null-type? bool-type? etc
(generate-scalar-predicate :null)
(generate-scalar-predicate :bool)
(generate-scalar-predicate :int)
(generate-scalar-predicate :real)
(generate-scalar-predicate :str)
(generate-scalar-predicate :date)


(def- scalar-type-registry
  #{:null :bool :int :real :str :date})

(defn scalar-type?
  ([x] (scalar-type? scalar-type-registry x))
  ([registry x]
     (and (satisfies? Typeable x)
          (contains? registry (getType x)))))

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


;; # Helpers

(defn empty-collection? [collection]
  (and (collection-type? collection)
       (= (:coll-of collection) :nothing)))

(defn empty-document? [document]
  (and (document-type? document)
       (empty? (:map document))
       (empty? (:properties document))))

(defn document-types [union]
  (set (filter document-type? (:union-of union))))

(defn non-document-types [union]
  (set (remove document-type? (:union-of union))))
