(ns jsonschema.type-system.merge
  (:require [jsonschema.type-system.types :refer :all]
            [jsonschema.type-system.merge-common :refer
             [make-type-merger
              congruent?
              incongruent?
              merge-compatible?
              reduce-compatible-things]]
            [clojure.set :refer [union]]))

(declare type*type=>merge-fn)

(defn type-merger []
  (make-type-merger type*type=>merge-fn))

(defn merge-reducer [types]
  (reduce-compatible-things types
                            merge-compatible?
                            #(type-merge (type-merger) % %2)))

;; ## Merging of all of the various types
;; ### Scalar merging

(defn merge-scalar-scalar [s1 s2]
  (if (same-type? s1 s2)
    (merge-same-typed-scalars s1 s2)
    (make-union-with s1 s2 merge-reducer)))

(defn merge-scalar-document [s d]
  (make-union-with s d merge-reducer))

(defn merge-scalar-collection [s c]
  (make-union-with s c merge-reducer))

(defn merge-scalar-union [s u]
  (if (some #(same-type? s %) (:union-of u))
    ;; Assumes that it's only mergeable with ONE element in the union.
    (make-union (map (fn [u-elem]
                       (if (same-type? s u-elem)
                         (merge-same-typed-scalars s u-elem)
                         u-elem))
                     (:union-of u))
                merge-reducer) ;; XXX merge-reducer
    (make-union (conj (:union-of u) s) merge-reducer))) ;; XXX merge-reducer

;; ### Document merging

(defn merge-document-scalar [d s]
  (merge-scalar-document s d))

;; `merge-with` will merge corresponding key-value pairs across maps
;; applying the function provided as the first arguement to `merge-with`
;; to the values _v1, v2, v3, ..._ corresponding  to key _k_ in maps
;; _m1, m2, m3, ..._, respectively.
;;
(defn merge-document-document [d1 d2]
  (if (incongruent? d1 d2)
    (make-union-with d1 d2 merge-reducer)
    (make-document
     (merge-with
      #(type-merge (type-merger) % %2)
      (:map d1) (:map d2)))))

(defn merge-document-collection [d c]
  (make-union-with d c merge-reducer))

(defn merge-document-union [d u]
  (let [docs (document-types u)]
    (cond
     (empty? docs) (make-union (conj (:union-of u) d) merge-reducer)
     (every? #(incongruent? d %) docs) (make-union (conj (:union-of u) d)
                                                   merge-reducer),
     :else
     ;; Assumes that it's only congruent to ONE element in the union.
     (let [congruent-doc (some #(when (congruent? d %) %) docs)
           existing-types (:union-of u)]
       (make-union (conj (disj existing-types congruent-doc)
                         (merge-document-document d congruent-doc)
                         merge-reducer))))))

;; ### Collection merging

(defn merge-collection-scalar [c s]
  (merge-scalar-collection s c))

(defn merge-collection-document [c d]
  (merge-document-collection d c))

(defn merge-collection-collection [c1 c2 & {:keys [mergeable?
                                                   type-reducer]
                                            :or {mergeable? merge-compatible?
                                                 type-reducer merge-reducer}}]
  (if (mergeable? c1 c2)
    (make-collection-with (:coll-of c1) (:coll-of c2) type-reducer)
    (make-union-with c1 c2 type-reducer)))

(defn merge-collection-union [c u & {:keys [mergeable?
                                            type-reducer]
                                     :or {mergeable? merge-compatible?
                                          type-reducer (merge-reducer)}}]
  (if (some #(mergeable? c %) (:union-of u))
    (make-union (map (fn [u-elem]
                       (if (mergeable? c u-elem)
                         (type-merge merger c u-elem)
                         u-elem))
                     (:union-of u))
                type-reducer)
    (make-union (conj (:union-of u) c)
                type-reducer)))

;; ### Union merging

(defn merge-union-scalar [u s]
  (merge-scalar-union s u))

(defn merge-union-document [u d]
  (merge-document-union d u))

(defn merge-union-collection [u c]
  (merge-collection-union c u))


(defn merge-union-union [u1 u2]
  (make-union
   (reduce (fn [merged-types type]
             (if (some true? (map #(merge-compatible? type %) merged-types))
               ;; ... do the merge
               (map (fn [merged-type]
                      (if (merge-compatible? merged-type type)
                        (type-merge (type-merger) merged-type type)
                        merged-type))
                    merged-types)
               ;; ... otherwise insert it
               (conj merged-types type)))
           []
           (union (:union-of u1) (:union-of u2)))
   merge-reducer))

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




;; Convenience function

(defn merge-types [& types]
  (let [merger (type-merger)]
    (reduce #(type-merge merger % %2) types)))


;; (defn init-scalars []
;;   (def s1 (make-int 20 40))
;;   (def s2 (make-int 21 41))
;;   (def s3 (make-int 25 30))
;;   (def s4 (make-int 15 20)))
;; ;; (make-union-with a b)
;; ;; #jsonschema.type_system.types.Union{:union-of (#jsonschema.type_system.types.Int{:min 20, :max 41})}
;; ;; (make-union-with a c)
;; ;; #jsonschema.type_system.types.Union{:union-of (#jsonschema.type_system.types.Int{:min 20, :max 40})}
;; ;; (make-union-with a d)
;; ;; #jsonschema.type_system.types.Union{:union-of (#jsonschema.type_system.types.Int{:min 15, :max 40})}
;; ;; (make-union-with a b c)
;; ;; #jsonschema.type_system.types.Union{:union-of (#jsonschema.type_system.types.Int{:min 20, :max 41})}
;; ;; (make-union-with a b c d)
;; ;; #jsonschema.type_system.types.Union{:union-of (#jsonschema.type_system.types.Int{:min 15, :max 41})}


;; (defn init-docs []
;;   (def d1 (extract-type {:a "2c"}))
;;   (def d2 (extract-type {:a "4chs"}))
;;   (def d3 (extract-type {:a [23]}))
;;   (def d4 (extract-type {:a [24]})))
;; ;; (merge-document-document d1 d2)
;; ;; #jsonschema.type_system.types.Document{:properties #{:a}, :map {:a #jsonschema.type_system.types.Str{:min 2, :max 4}}}
;; ;; (merge-document-document d3 d4)
;; ;; XXX


;; (defn init-colls []
;;   (def c1 (extract-type [1]))
;;   (def c2 (extract-type [1 2]))
;;   (def c3 (extract-type [1 "a"]))
;;   (def c4 (extract-type [2 "asdf"])))
;; ;; (merge-compatible? c1 c2)
;; ;; (merge-compatible? c3 c4)

;; (defn init-unions []
;;   (def u1 (extract-type [1 "a"]))
;;   (def u2 (extract-type [2 "ab"])))
;; ;; (merge-two-merge-compatible-things u1 u2)
