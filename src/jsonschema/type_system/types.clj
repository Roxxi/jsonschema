(ns jsonschema.type-system.types
  (:use jsonschema.utils))

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

;; # Type Primitives
;; These are the "structures" in which we use to represent type.
;; Ultimately, these will be mapped into Avro structures instead of Clojure
(defprotocol Typeable
  (getType [this] "Returns the type of whatever this is"))

(defrecord Scalar [type]
  Typeable
  (getType [this] type))

(defrecord Union [union-of]
  Typeable
  (getType [this] :union))

(defrecord Document [properties map]
  Typeable
  (getType [this] :document))

(defrecord Collection [coll-of]
  Typeable
  (getType [this] :collection))

;; ## Factory methods
(defn make-scalar [type]  
  (Scalar. type))

(defn make-special [x]
  (cond
   (special-date? x) (make-scalar :date),
   (special-id? x) (make-scalar :id)))
  
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
  "If a collection contains no documents, there
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

;; # Helpers
(defn- document-type? [x]
  (= (getType x) :document))

(defn document-types [union]
   (set (filter document-type? (:union-of union))))

(defn non-document-types [union]
  (set (remove document-type? (:union-of union))))



