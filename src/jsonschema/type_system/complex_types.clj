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

(declare reduce-homo-mergeable-things)

;; ## Documents

(defn make-document [property-type-map]
  (Document. (set (keys property-type-map))
             property-type-map))

;; ## Unions

(defn- one? [a-seq]
  (= (count a-seq) 1))

(defn make-union [types]
  (let [unique-types (reduce-homo-mergeable-things types)]
    (if (one? unique-types)
      (first unique-types)
      (Union. unique-types))))

(defn make-union-with [& types]
  (make-union types))

(defn maybe-make-union [types]
  (cond
    (empty? types) nil
    (one? types) (first types)
    :else (make-union types)))

(defn maybe-make-union-with [& types]
  (maybe-make-union types))

;; ## Collections

(defn make-collection
  "If a collection contains no types, there will be no types. As such, this
will be a collection of :nothing"
  [types]
  (Collection. (if (empty? types)
                 :nothing
                 (let [unique-types (reduce-homo-mergeable-things types)]
                   (if (one? unique-types)
                     (first unique-types)
                     (make-union unique-types))))))

(defn make-collection-with [& types]
  (make-collection types))

;; # Merging

(defn congruent? [d1 d2]
  (= (:properties d1) (:properties d2)))

(defn incongruent? [d1 d2]
  (not (congruent? d1 d2)))

;; There are two outcomes when you try to merge types:
;;   1) The merge result has the same type as one or both input types, possibly
;;      with different metadata
;;   2) The merge result has a different type than both input types.
;; Two things are homo-mergeable if merging them looks like case 1.
;; Otherwise, they are not homo-mergeable, and merging them will result in a
;; Union type.
;;
;; need to independently write merge-same-type fxns from
;; merge-diff-type fxns
(defn type-dispatcher [t1 t2]
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
   :non-mergeable-types
   :else
   :default))
(defmulti homo-mergeable? type-dispatcher)

(defmethod homo-mergeable? :scalar [t1 t2]
  (= (getType t1) (getType t2)))

(defmethod homo-mergeable? :document [t1 t2]
  (and (congruent? t1 t2)
       (every? true? (map
                      (fn [[k v1]] (homo-mergeable? v1 (get (:map t2) k)))
                      (:map t1)))))

(defmethod homo-mergeable? :collection [t1 t2]
  (homo-mergeable? (:coll-of t1) (:coll-of t2)))


(defn- both-unions? [t1 t2]
  (and (same-type? t1 t2)
       (= (getType t1) :union)))

(defmethod homo-mergeable? :union [t1 t2]
  (if (both-unions? t1 t2)
    ;; if both unions, they should be homomergeable unions
    (let [u1 (:union-of t1)
          u2 (:union-of t2)]
      (and (= (count u1) (count u2))
           (every? true? (map
                          (fn [union-element]
                            (some true? (map
                                         #(homo-mergeable? union-element %)
                                         u2)))
                          u1))))
    ;; if only one is a union
    (let [[the-union other] (if (union-type? t1) [t1 t2] [t2 t1])]
      (some true? (map #(homo-mergeable? other %) (:union-of the-union))))))

(defmethod homo-mergeable? :non-mergeable-types [t1 t2]
  false)

(defmethod homo-mergeable? :default [t1 t2]
  (throw (RuntimeException.
          (str "Don't know how to decide if these two objects "
               "are homo-mergeable: " t1 t2))))

;; (homo-mergeable? (make-union-with (make-str "foo") (make-int 3))
;;                  (make-str "foobar"))
;; true
;; (homo-mergeable? (make-collection-with (make-int 4)) (make-int 4))
;; false





(defn- build-map-from-pairs [seq-of-pairs]
  (into {} seq-of-pairs))

(defmulti merge-two-homo-mergeable-things type-dispatcher)
(defmethod merge-two-homo-mergeable-things :scalar [t1 t2]
  (merge-same-typed-scalars t1 t2))
(defmethod merge-two-homo-mergeable-things :document [t1 t2]
  (make-document
   (build-map-from-pairs
    (map (fn [[k v1]]
           (let [v2 (get (:map t2) k)
                 merged-v (merge-two-homo-mergeable-things v1 v2)]
             [k merged-v]))
         (:map t1)))))
(defmethod merge-two-homo-mergeable-things :collection [t1 t2]
  (make-collection
   (vec
    (merge-two-homo-mergeable-things (:coll-of t1) (:coll-of t2)))))
(defmethod merge-two-homo-mergeable-things :union [t1 t2]
  (if (both-unions? t1 t2)
    ;; if both are unions
    (make-union
     (map (fn [u1-elem]
            (let [u2-elem (first
                           (filter #(homo-mergeable? u1-elem %) (:union-of t2)))]
              (merge-two-homo-mergeable-things u1-elem u2-elem)))
          (:union-of t1)))
    ;;if only one is a union
    (let [[the-union other] (if (union-type? t1) [t1 t2] [t2 t1])]
      (make-union
       (map (fn [u1-elem]
              (if (homo-mergeable? u1-elem other)
                (merge-two-homo-mergeable-things u1-elem other)
                u1-elem))
            (:union-of t1))))))

(defmethod merge-two-homo-mergeable-things :non-mergeable-types [t1 t2]
  "barf")
(defmethod merge-two-homo-mergeable-things :default [t1 t2]
  "barf")

;; Assumes homo-mergeability is transitive, WHICH IT IS.
;;
;; if input is [a1 a2 b1 c1 a3 c2]
;; then output is [merged(a1 a2 a3) b1 merged(c1 c2)]
(defn reduce-homo-mergeable-things [types]
  (reduce (fn [merged-types type]
            ;; if it's mergeable with something ...
            (if (some true? (map #(homo-mergeable? type %) merged-types))
              ;; ... do the merge
              (map (fn [merged-type]
                     (if (homo-mergeable? merged-type type)
                       (merge-two-homo-mergeable-things merged-type type)
                       merged-type))
                   merged-types)
              ;; ... otherwise insert it
              (conj merged-types type)))
          []
          types))

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
