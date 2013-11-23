(ns jsonschema.type-system.merge-common
  "Things that are needed by both merge.clj and simplify.clj."
  (:require [jsonschema.type-system.types :refer :all]
            [clojure.set :refer [union]]))

;; # Core abstraction of merging types

(defn- type-type [type]
  "Returns `:scalar` if type is any kind of scalar. When we add custom types,
  we can derive the set of scalar types dynamically."
  (let [type-sigil (getType type)]
    (or (#{:document} type-sigil)
        (#{:collection} type-sigil)
        (#{:union} type-sigil)
        (and (#{:int :real :str :null :bool :date} type-sigil)
             :scalar))))

(defprotocol TypeMerger
  "Merges two types"
  (type-merge [_ type1 type2]))

(deftype TypeMergerImpl [type*type=>merge-fn]
  TypeMerger
  (type-merge [_ t1 t2]
    (let [merge-fn (get-in type*type=>merge-fn [(type-type t1) (type-type t2)])]
      (merge-fn t1 t2))))

(defn make-type-merger [type*type=>merge-fn]
  (TypeMergerImpl. type*type=>merge-fn))

;; # Predicates

(defn congruent? [d1 d2]
  (= (:properties d1) (:properties d2)))

(defn incongruent? [d1 d2]
  (not (congruent? d1 d2)))

;; There are two outcomes when you try to merge types:
;;   1) The merge result has the same type as one or both input types
;;      (possibly with different metadata)
;;   2) The merge result has a different type than both input types
;;      (and it will necessarily be a Union type).
;; Two things are merge-compatible if merging them looks like case 1.
;; Otherwise, they are not merge-compatible, and merging them will result in a
;; Union type.
(defn type-dispatcher [t1 t2 merge-notion]
  (cond
   (or (union-type? t1) (union-type? t2))
   :union
   (and (scalar-type? t1) (scalar-type? t2))
   :scalar
   (and (document-type? t1) (document-type? t2))
   :document
   (and (collection-type? t1) (collection-type? t2))
   :collection
   (and (satisfies? Typeable t1) (satisfies? Typeable t2))
   :non-mergeable-types))

(defmulti compatible? type-dispatcher)

(defmethod compatible? :scalar [t1 t2 merge-notion]
  (= (getType t1) (getType t2)))

(defmethod compatible? :document [t1 t2 merge-notion]
  (condp = merge-notion
    :merge (congruent? t1 t2)
    :simplify true))

(defmethod compatible? :collection [t1 t2 merge-notion]
  (let [c1 (:coll-of t1)
        c2 (:coll-of t2)]
    (condp = merge-notion
      :merge (cond
              (and (= :nothing c1) (= :nothing c2)) true
              (or (= :nothing c1) (= :nothing c2)) false
              :else (compatible? c1 c2 merge-notion))
      :simplify (cond
                 (or (= :nothing c1) (= :nothing c2)) true
                 :else (compatible? c1 c2 merge-notion)))))

(defmethod compatible? :union [t1 t2 merge-notion]
  (if (and (union-type? t1) (union-type? t2))
    ;; if both unions, they should be compatible unions
    (let [u1 (:union-of t1)
          u2 (:union-of t2)]
      (and (= (count u1) (count u2))
           (every? true? (map
                          (fn [union-element]
                            (some #(compatible? union-element % merge-notion) u2))
                          u1))))
    ;; if only one is a union
    (let [[the-union other] (if (union-type? t1) [t1 t2] [t2 t1])]
      (some #(compatible? other % merge-notion) (:union-of the-union)))))

(defmethod compatible? :non-mergeable-types [t1 t2 merge-notion]
  false)

(defmethod compatible? :default [t1 t2 merge-notion]
  (throw (RuntimeException.
          (str "Don't know how to decide if these two objects "
               "are compatible: " t1 ", " t2))))




(defn merge-compatible? [t1 t2]
  (compatible? t1 t2 :merge))

(defn simplify-compatible? [t1 t2]
  (compatible? t1 t2 :simplify))



;; Assumes the notion of compatibility is transitive...
;;
;; if input is [a1 a2 b1 c1 a3 c2]
;; then output is [merged(a1 a2 a3) b1 merged(c1 c2)]
(defn reduce-compatible-things [types compatible? merge-two-compatible-things]
  (reduce (fn [merged-types type]
            ;; if it's mergeable with something ...
            (if (some #(compatible? type %) merged-types)
              ;; ... do the merge
              (map (fn [merged-type]
                     (if (compatible? merged-type type)
                       (merge-two-compatible-things merged-type type)
                       merged-type))
                   merged-types)
              ;; ... otherwise insert it
              (conj merged-types type)))
          []
          types))





;; (merge-compatible? (make-union-with (make-str "foo") (make-int 3))
;;                  (make-str "foobar"))
;; true
;; (merge-compatible? (make-collection-with (make-int 4)) (make-int 4))
;; false
;; (merge-two-types (extract-type [1 2 3]) (extract-type 4))

;; (merge-two-types (extract-type {:a 1}) (extract-type {:a "hello"}))
;; (make-document {:a (make-union-with (make-int 1) (make-str "hello"))})
;; (merge-two-types (extract-type {:a 1}) (extract-type {:a "hello", :b 1}))
;; (make-union-with (make-document {:a (make-int 1)})
;;                  (make-document {:a (make-str "hello") :b (make-int 1)}))
;; (simplify-two-types (extract-type {:a 1}) (extract-type {:a "hello", :b 1}))
;; (make-document {:a (make-union-with (make-int 1) (make-str "hello")) :b (make-int 1)})