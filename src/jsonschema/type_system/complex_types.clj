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

;; ## Documents

(defn make-document [property-type-map]
  (Document. (set (keys property-type-map))
             property-type-map))

;; ## Unions

(defn- one? [a-seq]
  (= (count a-seq) 1))

(defn make-union [types type-reducer]
  (let [unique-types (type-reducer types)]
    (if (one? unique-types)
      (first unique-types)
      (Union. unique-types))))

(defn make-union-with [& types-then-reducer]
  (make-union (butlast types-then-reducer)
              (last types-then-reducer)))

(defn maybe-make-union [types type-reducer]
  (cond
    (empty? types) nil
    (one? types) (first types)
    :else (make-union types type-reducer)))

(defn maybe-make-union-with [& types-then-reducer]
  (maybe-make-union (butlast types-then-reducer)
                    (last types-then-reducer)))

;; there should be a "maybe make union from these types, none of which is a union"
;; and a "merge this type into the union"
;; but make-union should definitely just be a factory function :(
;;
;; similarly with collections


;; ## Collections

(defn make-collection
  "If a collection contains no types, there will be no types. As such, this
will be a collection of :nothing"
  [types type-reducer]
  (Collection. (let [unique-types (type-reducer types)]
                 (cond
                  (empty? unique-types) :nothing
                  (one? unique-types) (first unique-types)
                  :else (make-union unique-types type-reducer)))))

(defn make-collection-with [& types-then-reducer]
  (make-collection (butlast types-then-reducer)
                   (last types-then-reducer)))


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
