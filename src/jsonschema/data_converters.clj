(ns jsonschema.data-converters
  "Functions that up-convert scalar data of one type,
to scalar data of another type"
  (:require [clojure.math.numeric-tower :as num-tower]
            [roxxi.utils.print :refer [print-expr]]
            [roxxi.utils.collections :refer [project-map]]
            [jsonschema.type-system.types :refer [document-type?]]))

(defn- ->null [datum]
  nil)

(defn- boolean? [datum]
  (or (true? datum) (false? datum)))

(defn- ->bool [datum]
  (cond (nil? datum) nil
        datum true
        :else false))


(defn- ->integer
  "This returns a mathematical integer, not necessarily and integer 'type'"
  [datum]
  (cond
   ;; There are contexts where statistical rounding might be more approriate.
   ;; But for now, we'll go with the poor-man's approach.
   (number? datum) (num-tower/round datum)
   (boolean? datum) (if (->bool datum) 1 0)
   :else (->null datum)))

(defn- ->real [datum]
  (cond (number? datum) (double datum)
        (boolean? datum) (if (->bool datum) 1.0 0.0)
        :else (->null datum)))

(defn- ->str [datum]
  (cond (nil? datum) nil
        :else (str datum)))

(def type-desc=>converter
  {:null   ->null
   :bool   ->bool
   :int    ->integer
   :real   ->real
   :str    ->str
   :date   ->str
   :id     ->str})

(def type=>converter
  (project-map type-desc=>converter
               :key-xform #(symbol (str "make-" (name %)))))


(defn make-type-converters [field-name=>type-or-doc]
  (let [field-name=>type (if (document-type? field-name=>type-or-doc)
                          (:map field-name=>type-or-doc)
                          field-name=>type-or-doc)]
    (project-map field-name=>type
                 :value-xform #(get type=>converter %))))
