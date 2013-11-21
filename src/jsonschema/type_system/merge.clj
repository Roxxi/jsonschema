(ns jsonschema.type-system.merge
  (:require [jsonschema.type-system.types :refer :all]
            [jsonschema.type-system.extract :refer [extract-type]]
            [clojure.set :refer [union]]))

(declare type*type=>merge-fn)

;; ## Merging helpers

(defn- type-type [type]
  "Returns `:scalar` if type is any kind of scalar. When we add custom types,
  we can derive the set of scalar types dynamically"
  (let [type-sigil (getType type)]
    (or (#{:document} type-sigil)
        (#{:collection} type-sigil)
        (#{:union} type-sigil)
        (and (#{:int :real :str :null :bool :date} type-sigil)
             :scalar))))

;; # Core abstraction of merging types

(defprotocol TypeMerger
  "Given two types, merges the types according to rules specified in this document"
  (type-merge [_ type1 type2]))

(deftype TypeMergerImpl [type*type=>merge-fn]
  TypeMerger
  (type-merge [_ t1 t2]
    (let [merge-fn (get-in type*type=>merge-fn [(type-type t1) (type-type t2)])]
      (merge-fn t1 t2))))

(defn make-type-merger [type*type=>merge-fn]
  (TypeMergerImpl. type*type=>merge-fn))



(defn- type-merger []
  (make-type-merger type*type=>merge-fn))


;; ## Merging of all of the various types
;; ### Scalar merging

(defn merge-scalar-scalar [s1 s2]
  (if (same-type? s1 s2)
    (merge-same-typed-scalars s1 s2)
    (make-union-with s1 s2)))

(defn merge-scalar-document [s d]
  (make-union-with s d))

(defn merge-scalar-collection [s c]
  (make-union-with s c))

(defn merge-scalar-union [s u]
  (if ((:union-of u) s)
    u
    (make-union (conj (:union-of u) s))))

;; ### Document merging

(defn merge-document-scalar [d s]
  (merge-scalar-document s d))

;; `merge-with` will merge corresponding key-value pairs across maps
;; applying the function provided as the first arguement to `merge-with`
;; to the values _v1, v2, v3, ..._ corresponding  to key _k_ in maps
;; _m1, m2, m3, ..._, respectively.
;;
;;TODO
(defn merge-document-document [d1 d2]
  (if (incongruent? d1 d2)
    (make-union-with d1 d2)
    (make-document
     (merge-with
      #(type-merge (type-merger) % %2)
      (:map d1) (:map d2)))))

(defn merge-document-collection [d c]
  (make-union-with d c))

;;TODO
(defn merge-document-union [d u]
  (let [docs (document-types u)]
    (cond
      (empty? docs) (make-union (conj (:union-of u) d))
      (some #(= d %) docs) u, ;; any matches? keep the union
      (every? #(incongruent? d %) docs) (make-union (conj (:union-of u) d)),
      :else
      (let [congruent-doc (some #(when (congruent? d %) %) docs)
            existing-types (:union-of u)]
        (make-union (conj (disj existing-types congruent-doc)
                          (merge-document-document d congruent-doc)))))))

;; ### Collection merging

(defn merge-collection-scalar [c s]
  (merge-scalar-collection s c))

(defn merge-collection-document [c d]
  (merge-document-collection d c))

;;TODO
(defn merge-collection-collection [c1 c2]
  (if (= c1 c2)
    c1
    (make-union-with c1 c2)))

;;TODO
(defn merge-collection-union [c u]
   (if ((:union-of u) c)
     u
     (make-union (conj (:union-of u) c))))


;; ### Union merging

(defn merge-union-scalar [u s]
  (merge-scalar-union s u))

(defn merge-union-document [u d]
  (merge-document-union d u))

(defn merge-union-collection [u c]
  (merge-collection-union c u))

;;TODO
(defn document-union
  "This assumes each set of docs are all incongruent with one another"
  [set-of-docs1 set-of-docs2]
  (loop [iterate-over set-of-docs1
         compare-to set-of-docs2
          doc-acc (transient #{})]
     (if (empty? iterate-over)
       ;; at the end, we take whatever we've been able to merge
       ;; and whatever's left over that had nothing congruent to it
       (set (concat (persistent! doc-acc) compare-to))
       (let [doc (first iterate-over)]
         (if-let [congruent-doc (some #(when (congruent? doc %) %) compare-to)]
           (recur
            (rest iterate-over)
            (disj compare-to congruent-doc)
            ;; add the result of our merged document to ones we've
            ;; accumulated through merging
            (conj! doc-acc (merge-document-document doc congruent-doc)))
           (recur
            (rest iterate-over)
            compare-to
            (conj! doc-acc doc)))))))


;;TODO
(defn merge-union-union [u1 u2]
  (make-union
   (union
    (union (non-document-types u1) (non-document-types u2))
    (document-union (document-types u1) (document-types u2)))))

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


(defn init-scalars []
  (def s1 (make-int 20 40))
  (def s2 (make-int 21 41))
  (def s3 (make-int 25 30))
  (def s4 (make-int 15 20)))
;; (make-union-with a b)
;; #jsonschema.type_system.types.Union{:union-of (#jsonschema.type_system.types.Int{:min 20, :max 41})}
;; (make-union-with a c)
;; #jsonschema.type_system.types.Union{:union-of (#jsonschema.type_system.types.Int{:min 20, :max 40})}
;; (make-union-with a d)
;; #jsonschema.type_system.types.Union{:union-of (#jsonschema.type_system.types.Int{:min 15, :max 40})}
;; (make-union-with a b c)
;; #jsonschema.type_system.types.Union{:union-of (#jsonschema.type_system.types.Int{:min 20, :max 41})}
;; (make-union-with a b c d)
;; #jsonschema.type_system.types.Union{:union-of (#jsonschema.type_system.types.Int{:min 15, :max 41})}


(defn init-docs []
  (def d1 (extract-type {:a "2c"}))
  (def d2 (extract-type {:a "4chs"}))
  (def d3 (extract-type {:a [23]}))
  (def d4 (extract-type {:a [24]})))
;; (merge-document-document d1 d2)
;; #jsonschema.type_system.types.Document{:properties #{:a}, :map {:a #jsonschema.type_system.types.Str{:min 2, :max 4}}}
;; (merge-document-document d3 d4)
;; XXX


(defn init-colls []
  (def c1 (extract-type [1]))
  (def c2 (extract-type [1 2]))
  (def c3 (extract-type [1 "a"]))
  (def c4 (extract-type [2 "asdf"])))
;; (homo-mergeable? c1 c2)
;; (homo-mergeable? c3 c4)

(defn init-unions []
  (def u1 (extract-type [1 "a"]))
  (def u2 (extract-type [2 "ab"])))
;; (merge-two-homo-mergeable-things u1 u2)
