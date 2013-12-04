(ns jsonschema.type-system.merge
  "Merging has a strict merge policy.
Two documents are only merge-compatible if they have the same keysets.
Two collections are only merge-compatible if their contents are merge-compatible
  (so empty collections are NOT merge-compatible with non-empty collections).
Two unions are only merge-compatible if all of their contents are
  merge-compatible."
  {:author "Alex Bahouth, Matt Halverson"
   :date "12/1/2012"}
  (:require [jsonschema.type-system.types :refer :all]
            [jsonschema.type-system.merge-common :refer
             [type-merge
              make-type-merger
              incongruent?
              merge-compatible?
              reduce-compatible-types
              turn-into-a-union-with]]
            [clojure.set :refer [union]]))

;; # Merging of all of the various types

(declare type-merger)
(declare merge-reducer)
(declare merge-two-types)

;; ## Scalars

(defn merge-scalar-scalar [s1 s2]
  (if (same-type? s1 s2)
    (merge-same-typed-scalars s1 s2)
    (make-union-with s1 s2)))

(defn merge-scalar-document [s d]
  (make-union-with s d))

(defn merge-scalar-collection [s c]
  (make-union-with s c))

(defn merge-scalar-union [s u & {:keys [type-reducer]
                                 :or {type-reducer merge-reducer}}]
  (turn-into-a-union-with type-reducer s u))

;; ## Documents

(defn merge-document-scalar [d s]
  (merge-scalar-document s d))

;; `merge-with` will merge corresponding key-value pairs across maps
;; applying the function provided as the first arguement to `merge-with`
;; to the values _v1, v2, v3, ..._ corresponding  to key _k_ in maps
;; _m1, m2, m3, ..._, respectively.
(defn merge-document-document [d1 d2]
  (if (incongruent? d1 d2)
    (make-union-with d1 d2)
    (make-document (merge-with merge-two-types (:map d1) (:map d2)))))

(defn merge-document-collection [d c]
  (make-union-with d c))

(defn merge-document-union [d u & {:keys [type-reducer]
                                   :or {type-reducer merge-reducer}}]
  (turn-into-a-union-with type-reducer d u))

;; ## Collections

(defn merge-collection-scalar [c s]
  (merge-scalar-collection s c))

(defn merge-collection-document [c d]
  (merge-document-collection d c))

(defn merge-collection-collection [c1 c2]
  (cond
   (and (empty-collection? c1) (empty-collection? c2))
   c1 ;; either, really.
   (or (empty-collection? c1) (empty-collection? c2))
   (make-union-with c1 c2)
   (merge-compatible? (:coll-of c1) (:coll-of c2))
   (make-collection (merge-two-types (:coll-of c1) (:coll-of c2)))
   :else
   (make-union-with c1 c2)))

(defn merge-collection-union [c u & {:keys [type-reducer]
                                     :or {type-reducer merge-reducer}}]
  (turn-into-a-union-with type-reducer c u))

;; ## Unions

(defn merge-union-scalar [u s]
  (merge-scalar-union s u))

(defn merge-union-document [u d]
  (merge-document-union d u))

(defn merge-union-collection [u c]
  (merge-collection-union c u))

(defn merge-union-union [u1 u2 & {:keys [type-reducer]
                                  :or {type-reducer merge-reducer}}]
  (turn-into-a-union-with type-reducer u1 u2))

;; # Putting it all together, and making it extensible...

(def type*type=>merge-fn
  {:scalar {:scalar merge-scalar-scalar
            :document merge-scalar-document
            :collection merge-scalar-collection
            :union merge-scalar-union}
   :document {:scalar merge-document-scalar
              :document merge-document-document
              :collection merge-document-collection
              :union merge-document-union}
   :collection {:scalar merge-collection-scalar
                :document merge-collection-document
                :collection merge-collection-collection
                :union merge-collection-union}
   :union {:scalar merge-union-scalar
           :document merge-union-document
           :collection merge-union-collection
           :union merge-union-union}})

(defn extend-merge-fn-mappings [merge-fn-map [type1 type2 merge-fn]]
  (assoc-in merge-fn-map [type1 type2] merge-fn))

;; # The money

(defn type-merger []
  (make-type-merger type*type=>merge-fn))

(defn merge-types [& types]
  (let [merger (type-merger)]
    (reduce #(type-merge merger % %2) types)))

(defn merge-two-types [t1 t2]
  (type-merge (type-merger) t1 t2))

(defn merge-reducer [types]
  (reduce-compatible-types types
                           merge-compatible?
                           #(merge-two-types % %2)))
