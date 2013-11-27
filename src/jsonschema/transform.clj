(ns jsonschema.transform
  "Transformations to apply to schemas"
  (:use roxxi.utils.print
        roxxi.utils.collections
        jsonschema.type-system.types))

;; # Table Transform
;; Transforming schemas that are almost flat document maps
;; that may have unions for their types, into schemas
;; that are flat with no unions.

(defn- flattenable-type? [type]
  (not (or (document-type? type) (collection-type? type))))

(defn- union-only-contains-scalars? [union-type]
  (every? flattenable-type? (:union-of union-type)))

(defn type-translatable-to-table-schema? [type]
  "Returns whether or not a type is suitable to use
to define a database table."
  (and (document-type? type)
       (not (empty-document? type))
       (let [property-types (vals (:map type))
             union-properties (filter union-type? property-types)]
         (and (every? flattenable-type? property-types)
              (every? union-only-contains-scalars? union-properties)))))


;; The order here is implicit such that
;; type-rollup[n] can be encompassed by type-rollup[n+1]
(def type-rollup [:null :bool :int :real :str])

(defn- negative? [x]
  (< x 0))

(defn genericize-types [type1 type2]
  (let [i1 (.indexOf type-rollup (getType type1))
        i2 (.indexOf type-rollup (getType type2))]
    (if (or (negative? i1) (negative? i2))
      ;; TODO This is a cop-out, but what else can we do?
      ;; Don't know...
      (make-str "")
      (if (< i1 i2) type2 type1))))

(defn collapse-union [union-type]
  (reduce genericize-types (:union-of union-type)))

(defn scalarify [scalar-or-union-type]
  (if (scalar-type? scalar-or-union-type)
    scalar-or-union-type
    (collapse-union scalar-or-union-type)))

(defn- document-collapse-unions [document-type]
  "A precondition of calling this function is that
you've already established you're passing it a document type"
  (make-document (project-map (:map document-type)
                              :value-xform scalarify)))


(defn db-tablify-type [type]
  "Turn an almost flat document schema that may have unions into a flat
document schema with no unions based on some arbitrary logic..."
  (and (type-translatable-to-table-schema? type)
       (document-collapse-unions type)))
