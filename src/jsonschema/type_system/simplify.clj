(ns jsonschema.type-system.simplify
  (:use jsonschema.type-system.types
        jsonschema.type-system.merge)
  (:require [clojure.set :refer [union]]
            [roxxi.utils.print :refer [print-expr]]
            [jsonschema.type-system.merge-common :refer
             [make-type-merger
              congruent?
              incongruent?
              merge-compatible?
              reduce-compatible-things]]))

(declare simplifying-type-merger)

;; ## Merging helpers

(defn type-simplifier []
  (make-type-merger simple-type*type=>merge-fn))

(defn simplify-reducer [types]
  (reduce-compatible-things types
                            simplify-compatible?
                            #(type-merge (type-simplifier) % %2)))

;; ## Merging of the various types

;; `merge-with` will merge corresponding key-value pairs across maps
;; applying the function provided as the first arguement to `merge-with`
;; to the values _v1, v2, v3, ..._ corresponding  to key _k_ in maps
;; _m1, m2, m3, ..._, respectively.
;;
(defn compact-merge-document-document [d1 d2]
  (make-document
   (merge-with
    #(type-merge (simplifying-type-merger) % %2)
    (:map d1) (:map d2))))

(defn compact-merge-document-union [d u]
  (let [docs (document-types u)]
    (cond
      (empty? docs) (make-union (conj (:union-of u) d) simplify-reducer)
      ;; Assumes there is only ONE document in the union
      :else
      (let [merged-doc (reduce compact-merge-document-document d docs)
            non-docs (non-document-types u)]
        (make-union (conj non-docs merged-doc)
                    simplify-reducer)))))

(defn compact-merge-collection-collection [c1 c2]
  (merge-collection-collection c1 c2
                               :mergeable? simplify-compatible?
                               :type-reducer simplify-reducer))

(defn compact-merge-collection-union [c u]
  (merge-collection-union c u
                          :mergeable? simplify-compatible?
                          :type-reducer simplify-reducer))

(defn compact-merge-union-document [u d]
  (compact-merge-document-union d u))

(defn compact-merge-union-collection [u c]
  (compact-merge-collection-union c u))

(defn compact-merge-union-union [u1 u2]
  (let [[u1-docs u2-docs] [(document-types u1) (document-types u2)]]
    (if (and (empty? u1-docs) (empty? u2-docs))
      (make-union (union (non-document-types u1) (non-document-types u2))
                  simplify-reducer)
      (make-union
       (conj
        (union (non-document-types u1) (non-document-types u2))
        (reduce compact-merge-document-document
                (union (document-types u1) (document-types u2))))
       simplify-reducer))))

;; see merge.clj
(def simple-type*type=>merge-fn
  (reduce extend-merge-fn-mappings
          type*type=>merge-fn
          [[:document :document compact-merge-document-document]
           [:document :union compact-merge-document-union]
           [:collection :collection compact-merge-collection-collection]
           [:collection :union compact-merge-collection-union]
           [:union :document compact-merge-union-document]
           [:union :collection compact-merge-union-collection]
           [:union :union compact-merge-union-union]]))



;; Convenience function
(defn simplify-types [& types]
  (let [merger (simplifying-type-merger)]
    (reduce #(type-merge merger % %2) types)))
