(ns jsonschema.type-system.types
  (:use roxxi.utils.collections))


;; # Type Primitives
;; These are the "structures" in which we use to represent type.
;; Ultimately, these will be mapped into Avro structures instead of Clojure
(defprotocol Typeable
  (getType [this] "Returns the type of this Typealbe thing"))


(defrecord Union [union-of]
  Typeable
  (getType [this] :union))

(defrecord Document [properties map]
  Typeable
  (getType [this] :document))

(defrecord Collection [coll-of]
  Typeable
  (getType [this] :collection))

(defrecord Scalar [type]
  Typeable
  (getType [this] type))

(defprotocol TypeRegistry
  (registered-types [this] "Returns a set of the configured sigils representing valid types")
  (valid-type? [this type-sigil] "Returns whether or not this sigil represents a valid type"))

(deftype ScalarTypeRegistry [type-sigils-coll]
  TypeRegistry
  (registered-types [this] (set type-sigils-coll))
  (valid-type? [this type-sigil] (contains? type-sigils-coll type-sigil)))


(def ^{:private true} default-scalar-types
  #{:int :real :string :date :id :null :bool})

;; ## Factory methods
(defn make-scalar-type-registry [type-sigil-coll]
  (ScalarTypeRegistry. type-sigil-coll))

(def ^{:private true} scalar-type-registry
  (make-scalar-type-registry default-scalar-types))

(defn make-scalar [type]
  (if (valid-type? scalar-type-registry type)
    (Scalar. type)
    (throw
     (Exception. (str "Invalid scalar type: " type
                      ", valid types: "
                      (registered-types scalar-type-registry))))))
  
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

(defn make-document 
  [property-type-map]
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
  `(defn ~(symbol (str (name type) "-type?")) [some-type#]
     (= (getType some-type#) ~type)))

;; e.g. null-type? bool-type? etc
(generate-scalar-predicate :null)
(generate-scalar-predicate :bool)
(generate-scalar-predicate :int)
(generate-scalar-predicate :real)
(generate-scalar-predicate :string)
(generate-scalar-predicate :id)
(generate-scalar-predicate :date)

(defn scalar-type?
  ([x] (scalar-type? scalar-type-registry x))
  ([reg x]
     (and (satisfies? Typeable x)
          (valid-type? reg (getType x)))))

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




