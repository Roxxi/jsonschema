(ns jsonschema.data-converters
  "Functions that up-convert scalar data of one type,
to scalar data of another type"
  (:use roxxi.utils.print
        roxxi.utils.collections
        jsonschema.type-system.types))

(defn- ->null [datum]  
  nil)

(defn- boolean? [datum]
  (or (true? datum) (false? datum)))

(defn- ->bool [datum]
  (cond (nil? datum) nil        
        datum true
        :else false))

(defn- ->int [datum]
  (cond
   (number? datum) (int datum)
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
   :int    ->int
   :real   ->real
   :string ->str
   :date   ->str
   :id     ->str})
  
(def type=>converter
  (project-map type-desc=>converter
               :key-xform make-scalar))


(defn make-type-converters [field-name=>type-or-doc]
  (let [field-name=>type (if (document-type? field-name=>type-or-doc)
                          (:map field-name=>type-or-doc)
                          field-name=>type-or-doc)]
    (project-map field-name=>type
                 :value-xform #(get type=>converter %))))
