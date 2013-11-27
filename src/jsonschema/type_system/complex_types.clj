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
;;    unions be determined by their contents, regardless of the ORDER
;;    of the sequence 'non-mergeable-types'.
(defn make-union [non-mergeable-types]
  (Union. (set non-mergeable-types)))

(defn make-union-with [& non-mergeable-types]
  (make-union non-mergeable-types))

;; NB 'type' may be a simple or complex type (including another collection),
;;    of course.
(defn make-collection [type]
  (Collection. type))

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
