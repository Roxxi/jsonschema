(in-ns 'jsonschema.type-system.types)
(clojure.core/with-loading-context
  (clojure.core/require '[clojure.set :refer [union]]
                        '[roxxi.utils.common :refer [def-]]
                        '[roxxi.utils.collections :refer [project-map]]
                        '[roxxi.utils.print :refer [print-expr]]))

;; # Complex Type Primitives

;; ## Records

(defrecord Document [properties map]
  Typeable
  (getType [this] :document))

(defrecord Union [union-of]
  Typeable
  (getType [this] :union))

(defrecord Collection [coll-of]
  Typeable
  (getType [this] :collection))

;; # Predicates

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

;; # Factory methods

(defn make-document [property-type-map]
  (Document. (set (keys property-type-map))
             property-type-map))

(defn make-union [non-mergeable-types]
  (Union. non-mergeable-types))

(defn make-collection [non-mergeable-types]
  (Collection. (cond
                (empty? non-mergeable-types) :nothing
                (one? non-mergeable-types) (first non-mergeable-types)
                :else (make-union non-mergeable-types))))

;; # Helpers

(defn empty-collection? [collection]
  (and (collection-type? collection)
       (= (:coll-of collection) :nothing)))

(defn empty-document? [document]
  (and (document-type? document)
       (empty? (:map document))
       (empty? (:properties document))))

(defn document-types [union]
  (filter document-type? (:union-of union)))

(defn non-document-types [union]
  (remove document-type? (:union-of union)))
