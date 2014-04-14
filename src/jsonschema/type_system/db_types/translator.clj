(ns jsonschema.type-system.db-types.translator
  "DB Type Translator Protocol"
  {:author "Shawn Shah"
   :date "3/5/2014"}
  )

(defprotocol DBTypeTranslator
  (col-type->json-type [_ col-def-str])
  (json-type->col-type [_ json-type])
  )
