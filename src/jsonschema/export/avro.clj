(ns jsonschema.export.avro
  "Translating a schema into an avro parsable format"  
  (:use roxxi.utils.print
        roxxi.utils.collections
        jsonschema.type-system.types
        simple-avro.schema))


(defmulti rewrite-type
  "Translates a schema-type into a corresponding avro type"
  (fn [type] (getType type)))

(defmethod rewrite-type :null [type] 
  avro-null)
(defmethod rewrite-type :bool [type]
  avro-boolean)
(defmethod rewrite-type :int [type]
  avro-long)
(defmethod rewrite-type :real [type]
  avro-float)
(defmethod rewrite-type :string [type]
  avro-string)

(defmethod rewrite-type :collection [type]
  (avro-array (rewrite-type (:coll-of type))))

(defmethod rewrite-type :document [type]
  (let [field-decls
        ;; Does a merge if you hand avro-record a map.
        ;; As such, we flatten our key-type pairs
        ;; into a sequence where the nth element is the
        ;; key and the nth+1 element is a type
        (flatten
         (seq (project-map (:map type)
                           ;; need to unbox the type field form
                           ;; the avro spec
                           :value-xform #(:type (rewrite-type %)))))]
  (apply avro-record (cons (gensym) field-decls))))

(defmethod rewrite-type :union [type]
  (apply avro-union (map rewrite-type (:union-of type))))
                           