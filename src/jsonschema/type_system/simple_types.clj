(in-ns 'jsonschema.type-system.types)
(clojure.core/with-loading-context
  (clojure.core/require '[roxxi.utils.common :refer [def-]]))

;; # Simple Type Primitives
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

(defrecord Bool []
  Typeable
  (getType [this] :bool))

(defrecord Null []
  Typeable
  (getType [this] :null))

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
(generate-scalar-predicate :int)
(generate-scalar-predicate :real)
(generate-scalar-predicate :str)
(generate-scalar-predicate :bool)
(generate-scalar-predicate :null)
(generate-scalar-predicate :date)

(def- scalar-type-registry
  #{:null :bool :int :real :str :date})

;; `scalar` is a virtual type.
(defn scalar-type?
  ([x] (scalar-type? scalar-type-registry x))
  ([registry x]
     (and (satisfies? Typeable x)
          (contains? registry (getType x)))))

;; # Factory methods

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

(defn make-bool []
  (Bool. ))

(defn make-null []
  (Null. ))

(defn make-date [format]
  (Date. format))

;; # Merging

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

;; XXX TODO what does it mean to merge the formats?
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

;; # Helpers

(defn same-type? [s1 s2]
  (and (satisfies? Typeable s1)
       (satisfies? Typeable s2)
       (= (getType s1) (getType s2))))
