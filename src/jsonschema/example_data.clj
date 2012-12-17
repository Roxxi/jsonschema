(ns jsonschema.example-data
  (:use clojure.java.io
        roxxi.utils.print
        roxxi.utils.collections
        jsonschema.parser
        jsonschema.type-system.extract
        jsonschema.type-system.types))

(defprotocol Accumulator
  (append! [this element]
    "For whatever accumulate means here, accumulates this element")
  (values [this]
    "returns whatever values have been accumulated to this point"))
         
;; given a schema representing the types for each prop,
;; find a row that contains a value representing that type.

;; f(prop x type) => boolean
;; {:prop1 {:type1 false :type2 false}}
(defn seen-prop-with-type? [prop=>type=>seen prop type]
  (get-in prop=>type=>seen [prop type]))

(defn truify [thing-of-questionable-truthiness]
  "make thing-of-questionable-truthiness true if not already true"
  (or thing-of-questionable-truthiness (not thing-of-questionable-truthiness)))

(defn mark-prop-type-visited [prop=>type=>seen prop-type-pair]
  (let [prop (key prop-type-pair)
        type (val prop-type-pair)]
    (if (not (seen-prop-with-type? prop=>type=>seen prop type))
      (update-in prop=>type=>seen [prop type] truify)
      prop=>type=>seen)))

(defn mark-props-seen [prop=>type=>seen schema]
 (reduce mark-prop-type-visited prop=>type=>seen (schema :map)))

;; should we accept this row?
;; for a given schema
;; for each pair, check and see if we've seen it
(defn accept? [prop=>type=>seen input-schema]
  (loop [props=>types (seq (input-schema :map))]
    (if-let [prop-type-pair (first props=>types)]
      (if (seen-prop-with-type?
            prop=>type=>seen (key prop-type-pair) (val prop-type-pair))
      (recur (next props=>types))
      input-schema)
    false)))

;; for each row
;; if we should accept it
;; add it to the accumulator
;; update our board with any props this has provided as seen
;; return the board

(defn process-row! [prop=>type=>seen row acc]
  (let [row-schema (extract-type (parse-json-string row))]
    (if (accept? prop=>type=>seen row-schema)
      (do 
        (append! acc row)
        (mark-props-seen prop=>type=>seen row-schema))
      prop=>type=>seen)))

(defn seen-one-of-everything? [prop=>type=>seen]
  (every?
   (fn [prop->type=>seen]
     (every? (fn [type->seen] (val type->seen))
             (val prop->type=>seen)))
   prop=>type=>seen))

;; TODO shouldn't be inspecting properties of elements...
;; should have things like union? and/or scalar?
(defn schema->prop=>type=>seen [schema]
  (let [prop=>type (schema :map)]
    (project-map prop=>type
                 :value-xform
                 (fn [union-or-scalar]
                   (if-let [scalars (:union-of union-or-scalar)]
                     (extract-map scalars :value-extractor (fn [foo] false))
                     {union-or-scalar false})))))                         

(defn process-rows [schema rows acc]
  (loop [prop=>type=>seen (schema->prop=>type=>seen schema)
         rows rows           
         acc acc
         count 1]
      (if (or (seen-one-of-everything? prop=>type=>seen) (empty? rows))
        (do
          (println "Processed" count "rows.")
          acc)
        (recur (process-row! prop=>type=>seen (first rows) acc)
               (next rows)
               acc
               (+ count 1)))))

(defn make-print-acc []
  (reify Accumulator
    (append! [this row] (println row) row)
    (values [this] nil)))



(defn- safe-conj [origval newval]
  (if (set? origval)
    (conj origval newval)
    (set [origval newval])))

 
(defn make-binned-example-acc []
  (let [backing-map (atom {})]
    (reify Accumulator
      (append! [this row]
        (swap! backing-map
               (fn conj-map-vals [base-map]
                 (merge-with safe-conj base-map (parse-json-string row))))
        @backing-map)
      (values [this] (sort @backing-map)))))


(defn representative-data-set [schema json-file-path acc]
  (with-open [rdr (reader json-file-path)]
    (process-rows schema (line-seq rdr) acc)))

(defn binned-examples [schema json-file-path]
  (let [filled-acc
        (representative-data-set schema-file-path
                                 json-file-path
                                 (make-binned-example-acc))]
    (values filled-acc)))
