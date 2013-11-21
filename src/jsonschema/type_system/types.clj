(ns jsonschema.type-system.types
  (:require [clojure.set :refer [union]]
            [roxxi.utils.common :refer [def-]]
            [roxxi.utils.collections :refer [project-map]]
            [roxxi.utils.print :refer [print-expr]]))


;; # Type Primitives
;; These are the "structures" which we use to represent type.
;; Ultimately, these will be mapped into Avro structures instead of Clojure

;; ## Protocols

(defprotocol Typeable
  (getType [this] "Returns the type of this Typeable thing"))

(defprotocol Ranged
  (getMin [this] "Returns the min value of this Ranged thing")
  (getMax [this] "Returns the max value of this Ranged thing"))

(defprotocol Formatted
  (getFormat [this] "Returns the format string for this element's content"))

;; ## Records

(defrecord Union [union-of]
  Typeable
  (getType [this] :union))

(defrecord Document [properties map]
  Typeable
  (getType [this] :document))

(defrecord Collection [coll-of]
  Typeable
  (getType [this] :collection))

(defrecord Int [min max]
  Typeable
  (getType [this] :int)
  Ranged
  (getMin [this] min)
  (getMax [this] max))

(defrecord Real [min max]
  Typeable
  (getType [this] :real)
  Ranged
  (getMin [this] min)
  (getMax [this] max))

(defrecord Str [min max]
  Typeable
  (getType [this] :str)
  Ranged
  (getMin [this] min)
  (getMax [this] max))

(defrecord Null []
  Typeable
  (getType [this] :null))

(defrecord Bool []
  Typeable
  (getType [this] :bool))

(defrecord Date [format]
  Typeable
  (getType [this] :date)
  Formatted
  (getFormat [this] format))

;; # Predicates

(defmacro generate-scalar-predicate [type]
  `(defn ~(symbol (str (name type) "-type?")) [some-val#]
     (and
      (satisfies? Typeable some-val#)
      (= (getType some-val#) ~type))))

;; e.g. null-type? bool-type? etc
(generate-scalar-predicate :null)
(generate-scalar-predicate :bool)
(generate-scalar-predicate :int)
(generate-scalar-predicate :real)
(generate-scalar-predicate :str)
(generate-scalar-predicate :date)


(def- scalar-type-registry
  #{:null :bool :int :real :str :date})

;; `scalar` is a virtual type.
(defn scalar-type?
  ([x] (scalar-type? scalar-type-registry x))
  ([registry x]
     (and (satisfies? Typeable x)
          (contains? registry (getType x)))))

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

;; ## Factory methods

(defn make-int
  ([val] (Int. val val))
  ([min max] (Int. min max)))

(defn make-real
  ([val] (Real. val val))
  ([min max] (Real. min max)))

(defn make-str
  ([val] (let [len (count val)]
           (Str. len len)))
  ([min max] (Str. min max)))

(defn make-null []
  (Null. ))

(defn make-bool []
  (Bool. ))

(defn make-date [format]
  (Date. format))




(defn- merge-ranged-scalars [s1 s2 type]
  (let [min-val (min (getMin s1) (getMin s2))
        max-val (max (getMax s1) (getMax s2))
        known-types #{:int :real :str}]
    (if (contains? known-types type)
      (cond
       (= type :int) (make-int min-val max-val)
       (= type :real) (make-real min-val max-val)
       (= type :str) (make-str min-val max-val))
      (throw
       (RuntimeException.
        (str "Do not know how to merge-ranged-scalars of type " type
             "; only know " known-types))))))

;; TODO what does it mean to merge the formats?
(defn- merge-formatted-scalars [s1 s2 type]
  (let [known-types #{:date}]
    (if (contains? known-types type)
      s1
      (throw
       (RuntimeException.
        (str ("Do not know how to merge-formatted-scalars of type " type
              "; only know " known-types)))))))

(defn merge-same-typed-scalars [s1 s2]
  (let [type (getType s1)]
    (condp = type
      :int (merge-ranged-scalars s1 s2 type)
      :real (merge-ranged-scalars s1 s2 type)
      :str (merge-ranged-scalars s1 s2 type)
      :bool s1
      :null s1
      :date (merge-formatted-scalars s1 s2 type))))








(defn same-type? [s1 s2]
  (= (getType s1) (getType s2)))

(defn congruent? [d1 d2]
  (= (:properties d1) (:properties d2)))

(defn incongruent? [d1 d2]
  (not (congruent? d1 d2)))

;; There are two outcomes when you try to merge types:
;;   1) The merge result has the same type as one or both input types, possibly with different metadata
;;   2) The merge result has a different type than both input types.
;; Two things are homo-mergeable if merging them looks like case 1.
;; Otherwise, they are not homo-mergeable, and merging them will result in a Union type.
;;
;; need to independently write merge-same-type fxns from
;; merge-diff-type fxns
(defn homo-mergeable? [t1 t2]
  (if (same-type? t1 t2)
    (cond
     (scalar-type? t1)
     (= (getType t1) (getType t2))

     (document-type? t1)
     (let [map1 (print-expr (:map t1))
           map2 (print-expr (:map t2))]
       (and (congruent? t1 t2)
            (every? true? (map (fn [[k v1]]
                                 (homo-mergeable? v1 (map2 k)))
                               map1))));;recurse

     (collection-type? t1)
     (homo-mergeable? (:coll-of t1) (:coll-of t2))
     ;;recurse

     (union-type? t1)
     (let [u1 (:union-of t1)
           u2 (:union-of t2)]
       (and (= (count u1) (count u2))
            (every? true? (map (fn [union-element]
                                 (some true? (map #(homo-mergeable? union-element %) u2)))
                               u1))))
     ;;recurse

     :else (throw (RuntimeException. (str "Don't know how to merge these two objects: " t1 t2))))
    false))


(defn- build-map-from-pairs [seq-of-pairs]
  (into {} seq-of-pairs))

(defn merge-two-homo-mergeable-things [t1 t2]
  (cond
   (scalar-type? t1)
   (merge-same-typed-scalars t1 t2)

   (document-type? t1)
   (make-document
    (build-map-from-pairs
     (map (fn [[k v1]]
            (let [v2 (get (:map t2) k)
                  merged-v (merge-two-homo-mergeable-things v1 v2)]
              [k merged-v]))
          (:map t1))))

   (collection-type? t1)
   (make-collection (merge-two-homo-mergeable-things (:coll-of t1) (:coll-of t2)))

   (union-type? t1)
   (let [u1 (:union-of t1)
         u2 (:union-of t2)]
     (make-union
      (map (fn [union-element]
             (merge-two-homo-mergeable-things
              union-element
              (first (filter #(homo-mergeable? union-element %) u2))))
           u1)))

   :else "barf"))




(defn make-document [property-type-map]
  (Document. (set (keys property-type-map))
             property-type-map))




;; Assumes homo-mergeability is transitive, WHICH IT IS.
;;
;; if input is [a1 a2 b1 c1 a3 c2]
;; then output is [merged(a1 a2 a3) b1 merged(c1 c2)]
(defn reduce-homo-mergeable-things [types]
  (reduce (fn [merged-types type]
            ;; if something it's mergeable with something...
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

(defn- one? [a-seq]
  (= (count a-seq) 1))

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






(defn make-union [types]
  (let [unique-types (reduce-homo-mergeable-things types)]
    (if (one? unique-types)
      (first unique-types)
      (Union. unique-types))))

(defn make-union-with
  [& types]
  (make-union (set types)))

(defn maybe-make-union [types]
  (cond
    (empty? types) nil
    (= (count types) 1) (first types)
    (= (count (set types)) 1) (first types)
    :else (make-union types)))

(defn maybe-make-union-with [& types]
  (maybe-make-union types))





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
