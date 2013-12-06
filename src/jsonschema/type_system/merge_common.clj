(ns jsonschema.type-system.merge-common
  "Things that are needed by both merge.clj and simplify.clj."
  {:author "Alex Bahouth, Matt Halverson"
   :date "11/1/2013"}
  (:require [jsonschema.type-system.types :refer :all]
            [clojure.set :refer [union]]
            [roxxi.utils.print :refer [print-expr]]))

;; # Core abstraction of merging types

(defprotocol TypeMerger
  "Merges two types"
  (type-merge [_ type1 type2]))

(defn- type-type [type]
  "Returns `:scalar` if type is any kind of scalar. When we add custom types,
  we can derive the set of scalar types dynamically."
  (let [type-sigil (getType type)]
    (or (#{:document} type-sigil)
        (#{:collection} type-sigil)
        (#{:union} type-sigil)
        (and (#{:int :real :str :null :bool :date} type-sigil)
             :scalar))))

(deftype TypeMergerImpl [type*type=>merge-fn]
  TypeMerger
  (type-merge [_ t1 t2]
    (let [merge-fn (get-in type*type=>merge-fn [(type-type t1) (type-type t2)])]
      (merge-fn t1 t2))))

(defn make-type-merger [type*type=>merge-fn]
  (TypeMergerImpl. type*type=>merge-fn))

;; # Generic type-compatibility

(defn- type-dispatcher [t1 t2]
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

(defmulti congruent?
  "There are two outcomes when you try to merge types:
 1) The merge result has the same type as one or both input types
    (possibly with different Scalar metadata)
 2) The merge result has a different type than both input types
    (and it will necessarily be a Union type).
Two things are `compatible` if merging them looks like case 1.
Otherwise, they are not compatible, and merging them will result in a
Union type.

More generally, the question 'Are a and b compatible?' can be interpreted
as 'Under a particular notion of merging types, do a and b look alike?'"
  #^{:private true} type-dispatcher)

(defn incongruent? [t1 t2]
  (not (congruent? t1 t2)))

(defmethod congruent? :scalar [s1 s2]
  (same-type? s1 s2))

(defmethod congruent? :document [d1 d2]
  (= (:properties d1) (:properties d2)))

(defmethod congruent? :collection [c1 c2]
  (cond
   (and (empty-collection? c1) (empty-collection? c2)) true
   (or (empty-collection? c1) (empty-collection? c2)) false
   :else (congruent? (:coll-of c1) (:coll-of c2))))

(defmethod congruent? :union [t1 t2]
  (and (union-type? t1)
       (union-type? t2)
       (every? true? (map
                      (fn [union-element]
                        (some #(congruent? union-element %)
                              (:union-of t2)))
                      (:union-of t1)))
       (every? true? (map
                      (fn [union-element]
                        (some #(congruent? union-element %)
                              (:union-of t1)))
                      (:union-of t2)))))

(defmethod congruent? :non-mergeable-types [t1 t2]
  false)

(defmethod congruent? :default [t1 t2]
  (throw (RuntimeException.
          (str "Don't know how to decide if these two objects "
               "are compatible: " t1 ", " t2))))

;; # Generic type-reducer

(defn reduce-compatible-types [arbitrary-unmerged-types
                               mergeable?
                               merge-two-compatible-things]
  "If input is [a1 a2 b1 c1 a3 c2], then output
is [merged(a1 a2 a3) b1 merged(c1 c2)]

This function does not assume that compatibility is transitive, i.e. it
does not assume that
     If (compatible? a b) and (compatible? b c)), then (compatible? a c).
A consequence of this is, it's O(n^2) to reduce-compatible-types.

This is to accommodate for 'simplify' behavior. If you use a simplify-reducer on
   [[1] [\"a\"] [2 \"b\"]]
then you should get collection(collection(union(string, integer))).

Yet, we see that even though [1] and [2 \"b\"] are simplify-mergeable
and [\"a\"] and [2 \"b\"] are simplify-mergeable,
it holds that [1] and [\"a\"] are NOT simplify-mergeable...

So for simplify, mergeable? is not transitive!"
  (reduce (fn [merged-types type]
            (let [incompatibles (remove #(mergeable? type %) merged-types)
                  compatibles (filter #(mergeable? type %) merged-types)
                  merged-compatibles (reduce merge-two-compatible-things
                                             type
                                             compatibles)]
              (conj incompatibles merged-compatibles)))
          []
          arbitrary-unmerged-types))

;; # Super-useful logic-containing factory functions

(defn- one? [a-seq]
  (= (count a-seq) 1))

(defn- flatten-nested-unions [types]
  "Only flattens by one level."
  (let [non-unions (remove union-type? types)
        unions (filter union-type? types)
        flattened-unions (reduce
                          (fn [vector union] (concat vector (:union-of union)))
                          []
                          unions)]
    (union non-unions flattened-unions)))

(defn turn-into-a-union [type-reducer types]
  "If you need to combine a Union with other types (even other Unions),
this has the logic to do that sensibly."
  (if (some union-type? types)
    ;; This will recurse until all nested unions are unflattened.
    (turn-into-a-union type-reducer (flatten-nested-unions types))
    (let [unique-types (type-reducer types)]
      (cond
       (empty? types) nil
       (one? unique-types) (first unique-types)
       :else (make-union unique-types)))))

(defn turn-into-a-union-with [type-reducer & types]
  (turn-into-a-union type-reducer types))

(defn turn-into-a-collection [type-reducer types]
  "If you have a (possibly empty) seq of (possibly compatible/reducible) types
that you want to turn into a Collection, this has the logic to do that
sensibly."
  (let [unique-types (type-reducer types)]
    (make-collection
     (cond
      (empty? unique-types) :nothing
      (one? unique-types) (first unique-types)
      :else (make-union unique-types)))))

(defn turn-into-a-collection-with [type-reducer & types]
  (turn-into-a-collection type-reducer types))
