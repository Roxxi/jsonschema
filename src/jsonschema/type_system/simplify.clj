(ns jsonschema.type-system.simplify
  (:use jsonschema.type-system.types
        jsonschema.type-system.merge)
  (:require [clojure.set :refer [union]]
            [roxxi.utils.print :refer [print-expr]]
            [jsonschema.type-system.merge-common :refer
             [type-merge
              make-type-merger
              simplify-compatible?
              reduce-compatible-types]]))

;; # Merging of the various types

(declare type-simplifier)
(declare simplify-reducer)
(declare simplify-two-types)

;; ## Scalars

(defn compact-merge-scalar-union [s u]
  (merge-scalar-union s u :type-reducer simplify-reducer))

;; ## Documents

;; `merge-with` will merge corresponding key-value pairs across maps
;; applying the function provided as the first arguement to `merge-with`
;; to the values _v1, v2, v3, ..._ corresponding  to key _k_ in maps
;; _m1, m2, m3, ..._, respectively.
;;
(defn compact-merge-document-document [d1 d2]
  (make-document (merge-with simplify-two-types (:map d1) (:map d2))))

(defn compact-merge-document-union [d u]
  (merge-document-union d u :type-reducer simplify-reducer))

;; ## Collections

(defn compact-merge-collection-collection [c1 c2]
  (cond
   (and (empty-collection? c1) (empty-collection? c2))
   c1 ;; either, really.
   (empty-collection? c1)
   c2
   (empty-collection? c2)
   c1
   (simplify-compatible? (:coll-of c1) (:coll-of c2))
   (make-collection (simplify-two-types (:coll-of c1) (:coll-of c2)))
   :else
   (make-union [c1 c2])))

(defn compact-merge-collection-union [c u]
  (merge-collection-union c u :type-reducer simplify-reducer))

;; ## Unions

(defn compact-merge-union-scalar [u s]
  (compact-merge-scalar-union s u))

(defn compact-merge-union-document [u d]
  (compact-merge-document-union d u))

(defn compact-merge-union-collection [u c]
  (compact-merge-collection-union c u))

(defn compact-merge-union-union [u1 u2]
  (merge-union-union u1 u2 :type-reducer simplify-reducer))

;; # The money

;; see merge.clj
(def simple-type*type=>merge-fn
  (reduce extend-merge-fn-mappings
          type*type=>merge-fn
          [[:scalar :union compact-merge-scalar-union]
           [:document :document compact-merge-document-document]
           [:document :union compact-merge-document-union]
           [:collection :collection compact-merge-collection-collection]
           [:collection :union compact-merge-collection-union]
           [:union :scalar compact-merge-union-scalar]
           [:union :document compact-merge-union-document]
           [:union :collection compact-merge-union-collection]
           [:union :union compact-merge-union-union]]))

(defn simplify-types [& types]
  (let [simplifier (type-simplifier)]
    (reduce #(type-merge simplifier % %2) types)))

(defn type-simplifier []
  (make-type-merger simple-type*type=>merge-fn))



(defn simplify-two-types [t1 t2]
  (type-merge (type-simplifier) t1 t2))

(defn simplify-reducer [types]
  (reduce-compatible-types types simplify-compatible? #(simplify-two-types % %2)))
